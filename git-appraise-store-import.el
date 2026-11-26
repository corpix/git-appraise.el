;;; git-appraise-store-import.el --- One-shot git notes import -*- lexical-binding: t; -*-

;;; Commentary:
;; Move the personal state the notes transport wrote -- viewed
;; marks, fold choices, unsent feedback and drafts -- into the
;; SQLite store, then delete the notes refs.  Run once per
;; repository (`M-x git-appraise-import-git-notes'); one-way and
;; recorded: `legacy_imports' keeps every moved ref's pre-delete
;; tip, so a ref can be restored with `git update-ref' until gc.
;; git-appraise's own notes (refs/notes/devtools/*) are never
;; touched, and no ref is deleted before its rows are committed.

;;; Code:

(require 'a)
(require 'dash)
(require 'json)
(require 'subr-x)

(require 'git-appraise-store)
(require 'git-appraise-time)
(require 'git-appraise-utils)

(defconst git-appraise-store-import--refs
  '("refs/notes/git-appraise-el/viewed"
    "refs/notes/git-appraise-el/folds"
    "refs/notes/git-appraise-el/pr-state")
  "The personal notes refs this import consumes and deletes.")

(defconst git-appraise-store-import--fingerprint-regexp
  (concat "^sha256:[0-9a-f]\\{" (number-to-string (* 2 32)) "\\}$")
  "A well-formed sha256 content fingerprint.")

;;; Git plumbing

(defun git-appraise-store-import--git (repo args)
  "Run git ARGS in REPO without a shell.
Return (CODE STDOUT STDERR), all strings stripped of text
properties so values are safe to store later."
  (let ((out (generate-new-buffer " *git-appraise-import out*"))
        (err-file (make-temp-file "git-appraise-import-err")))
    (unwind-protect
        (let ((code (apply #'call-process "git" nil (list out err-file) nil
                           (append (list "-C" (expand-file-name repo)) args))))
          (list code
                (with-current-buffer out
                  (buffer-substring-no-properties (point-min) (point-max)))
                (with-temp-buffer
                  (insert-file-contents err-file)
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (ignore-errors (kill-buffer out))
      (ignore-errors (delete-file err-file)))))

;;; Legacy record validators
;; Faithful ports of what the notes readers accepted, so nothing a
;; previous version stored is silently dropped here.

(defun git-appraise-store-import--viewed-p (record)
  "Non-nil when RECORD is a valid legacy viewed event."
  (and (listp record)
       (equal (a-get record 'type) "viewed")
       (numberp (a-get record 'v)) (= (a-get record 'v) 1)
       (member (a-get record 'item) '("hunk" "file"))
       (stringp (a-get record 'path)) (not (string-empty-p (a-get record 'path)))
       (stringp (a-get record 'fingerprint))
       (string-match-p git-appraise-store-import--fingerprint-regexp
                       (a-get record 'fingerprint))
       (memq (a-get record 'viewed) '(t :json-false))
       (git-appraise-time-parse-utc (a-get record 'at))
       (stringp (a-get record 'id)) (not (string-empty-p (a-get record 'id)))
       (or (not (a-get record 'range)) (stringp (a-get record 'range)))))

(defun git-appraise-store-import--fold-p (record)
  "Non-nil when RECORD is a valid legacy fold event."
  (and (listp record)
       (equal (a-get record 'type) "fold")
       (numberp (a-get record 'v)) (= (a-get record 'v) 1)
       (member (a-get record 'item) '("file" "hunk"))
       (stringp (a-get record 'path))
       (stringp (a-get record 'fingerprint))
       (stringp (a-get record 'actor))
       (git-appraise-time-parse-utc (a-get record 'at))
       (stringp (a-get record 'id))
       (memq (a-get record 'folded) '(t :json-false))))

(defun git-appraise-store-import--pr-state-p (record)
  "Non-nil when RECORD is a valid legacy pr-state snapshot."
  (and (listp record)
       (a-get record 'type)
       (member (a-get record 'v) '(1 2))
       (equal (a-get record 'type) "pr-state")
       (stringp (a-get record 'id)) (not (string-empty-p (a-get record 'id)))
       (git-appraise-time-parse-utc (a-get record 'at))
       (stringp (a-get record 'pr))))

(defun git-appraise-store-import--kind (ref)
  "The mark kind or `pr-state' a legacy notes REF carried."
  (cond ((string-suffix-p "/viewed" ref) 'viewed)
        ((string-suffix-p "/folds" ref) 'fold)
        ((string-suffix-p "/pr-state" ref) 'pr-state)
        (t (error "Unknown legacy notes ref: %s" ref))))

;;; Note reading

(defun git-appraise-store-import--parse (text validate)
  "Parse note TEXT's JSON lines with VALIDATE.
Return (RECORDS . SKIPPED): one malformed or unknown line never
blocks the others, it only counts."
  (let ((records nil)
        (skipped 0))
    (dolist (line (split-string text "\n" t))
      (condition-case _
          (let ((obj (let ((json-object-type 'alist)
                           (json-array-type 'list)
                           (json-false :json-false))
                       (json-read-from-string line))))
            (if (funcall validate obj)
                (push obj records)
              (setq skipped (1+ skipped))))
        (error (setq skipped (1+ skipped)))))
    (cons (nreverse records) skipped)))

(defun git-appraise-store-import--notes (repo ref)
  "The notes of REF in REPO: (TIP (ANCHOR . TEXT)...), or nil.
TIP is the ref's current commit; every pair carries one note's
full text keyed by the object it is anchored on.  A missing ref is
nil; any other git failure signals with git's stderr."
  (pcase-let* ((`(,tip-code ,tip-out ,_)
                (git-appraise-store-import--git
                 repo (list "rev-parse" "--verify" "--quiet" ref))))
    (when (zerop tip-code)
      (pcase-let* ((`(,list-code ,list-out ,list-err)
                    (git-appraise-store-import--git
                     repo (list "notes" (concat "--ref=" ref) "list")))
                   (notes nil))
        (unless (zerop list-code)
          (error "git notes list %s failed: %s" ref (string-trim list-err)))
        (dolist (line (split-string list-out "\n" t))
          (pcase-let* ((`(,note ,anchor) (split-string line))
                       (`(,cat-code ,cat-out ,cat-err)
                        (git-appraise-store-import--git
                         repo (list "cat-file" "-p" note))))
            (unless (zerop cat-code)
              (error "Cannot read note %s of %s: %s"
                     note ref (string-trim cat-err)))
            (push (cons anchor cat-out) notes)))
        (cons (string-trim tip-out) (nreverse notes))))))

;;; Row building

(defun git-appraise-store-import--pr-revision (record)
  "The review revision in pr-state RECORD's identity, or nil."
  (let ((pr (a-get record 'pr)))
    (and (stringp pr)
         (string-match "\\`.*#\\([^#]*\\)\\'" pr)
         (not (string-empty-p (match-string 1 pr)))
         (match-string 1 pr))))

(defun git-appraise-store-import--pr-rows (repo-key records)
  "The pr_state rows for pr-state RECORDS in REPO-KEY.
Records whose identity carries no revision cannot be placed and
are counted in the second result."
  (let ((rows nil)
        (skipped 0))
    (dolist (record records)
      (let ((revision (git-appraise-store-import--pr-revision record))
            (feedback (a-get record 'feedback))
            (comments (a-get record 'local_comments)))
        (if revision
            (push (list repo-key
                        revision
                        (or (a-get record 'actor) "")
                        (a-get record 'v)
                        (a-get record 'id)
                        (and (stringp feedback) (not (string-empty-p feedback))
                             feedback)
                        (git-appraise-store--encode-comments
                         (cond ((vectorp comments) (append comments nil))
                               ((listp comments) comments)
                               (t nil)))
                        (git-appraise-time-parse-utc (a-get record 'at)))
                  rows)
          (setq skipped (1+ skipped)))))
    (cons (nreverse rows) skipped)))

(defun git-appraise-store-import--mark-rows (repo-key anchor records flag)
  "The event-store rows for mark RECORDS of ANCHOR in REPO-KEY.
FLAG is the records' boolean key (`viewed' or `folded')."
  (--map (list repo-key
               anchor
               (a-get it 'id)
               (a-get it 'actor)
               (a-get it 'item)
               (a-get it 'path)
               (a-get it 'fingerprint)
               (a-get it 'range)
               (if (eq (a-get it flag) t) 1 0)
               (git-appraise-time-parse-utc (a-get it 'at)))
         records))

(defun git-appraise-store-import--ref-plan (repo-key ref notes)
  "One plan entry for notes REF in REPO-KEY: (REF TIP KIND ROWS
SKIPPED), or nil when REF carries no importable record."
  (pcase-let* ((kind (git-appraise-store-import--kind ref))
               (`(,tip . ,note-texts) notes)
               (rows nil)
               (skipped 0))
    (pcase kind
      (`pr-state
       (let* ((validate #'git-appraise-store-import--pr-state-p)
              (records nil))
         (dolist (pair note-texts)
           (pcase-let ((`(,recs . ,bad)
                        (git-appraise-store-import--parse (cdr pair) validate)))
             (setq records (append records recs)
                   skipped (+ skipped bad))))
         (pcase-let ((`(,pr-rows . ,no-rev)
                      (git-appraise-store-import--pr-rows repo-key records)))
           (setq rows pr-rows
                 skipped (+ skipped no-rev)))))
      (_
       (let ((validate (if (eq kind 'viewed)
                           #'git-appraise-store-import--viewed-p
                         #'git-appraise-store-import--fold-p))
             (flag (git-appraise-store--flag kind)))
         (pcase-dolist (`(,anchor . ,text) note-texts)
           (pcase-let ((`(,records . ,bad)
                        (git-appraise-store-import--parse text validate)))
             (setq rows (append rows (git-appraise-store-import--mark-rows
                                      repo-key anchor records flag))
                   skipped (+ skipped bad)))))))
    (when rows
      (list ref tip kind rows skipped))))

;;; Plan and application

(defun git-appraise-store-import--recorded-tip (db repo-key ref)
  "The tip REPO-KEY's notes REF was imported at, or nil."
  (pcase-let* ((`(,sql . ,params)
                (git-appraise-sql-select
                 :from 'legacy_imports
                 :columns '(tip)
                 :where `(:and (= repo ,repo-key) (= ref ,ref))
                 :limit 1)))
    (caar (sqlite-select db sql params))))

(defun git-appraise-store-import-read (repo)
  "Read every legacy notes ref of REPO with unimported records.
Return a plan: one (REF TIP KIND ROWS SKIPPED) entry per ref that
exists, carries records, and moved or grew since the last import,
in `git-appraise-store-import--refs' order.  A ref recorded at its
current tip is skipped; a ref that grew afterwards (an old Emacs
writing notes again) is re-read whole -- already stored records
dedupe by id."
  (let* ((repo-key (or (git-appraise-store-repo-key repo)
                       (user-error "Not a git repository: %s" repo)))
         (db (git-appraise-store-db))
         (plan nil))
    (dolist (ref git-appraise-store-import--refs)
      (when-let* ((notes (git-appraise-store-import--notes repo ref)))
        (pcase-let* ((`(,tip . ,_) notes)
                     (recorded (git-appraise-store-import--recorded-tip
                                db repo-key ref)))
          (unless (and recorded (equal recorded tip))
            (when-let* ((entry (git-appraise-store-import--ref-plan
                                repo-key ref notes)))
              (push entry plan))))))
    (nreverse plan)))

(defun git-appraise-store-import-apply (repo plan)
  "Write PLAN's rows into the store, then delete the planned refs.
Every row set and its `legacy_imports' record commits in one
transaction; a ref is deleted only after its rows are committed.
Return (STORED . DELETED): rows stored and refs removed."
  (let* ((repo-key (git-appraise-store-repo-key repo))
         (db (git-appraise-store-db))
         (stored 0)
         (deleted 0))
    (git-appraise-store--call-in-write
     (lambda ()
       (pcase-dolist (`(,ref ,tip ,kind ,rows ,skipped) plan)
         (pcase kind
           (`pr-state
            (git-appraise-store--insert-chunked
             db 'pr_state
             '(repo revision actor v id feedback local_comments at_us)
             rows))
           (_
            (git-appraise-store--insert-chunked
             db (git-appraise-store--table kind)
             (git-appraise-store--event-columns kind) rows)))
         (pcase-let ((`(,sql . ,params)
                      (git-appraise-sql-insert
                       :into 'legacy_imports
                       :columns '(repo ref tip records skipped imported_at_us)
                       :rows (list (list repo-key ref tip
                                         (length rows) skipped
                                         (git-appraise-time-parse-utc
                                          (git-appraise-time-now-utc t))))
                       :on-conflict :replace)))
           (sqlite-execute db sql params))
         (setq stored (+ stored (length rows))))))
    (pcase-dolist (`(,ref ,_tip ,_kind ,_rows ,_skipped) plan)
      (pcase-let ((`(,code ,_ ,stderr)
                   (git-appraise-store-import--git
                    repo (list "update-ref" "-d" ref))))
        (if (zerop code)
            (setq deleted (1+ deleted))
          (message "git-appraise: could not delete %s: %s"
                   ref (string-trim stderr)))))
    (cons stored deleted)))


;;;###autoload
(defun git-appraise-import-git-notes (&optional repo)
  "Move REPO's personal git-appraise notes into the store.
Imports viewed marks, fold choices and unsent feedback/drafts
from the personal notes refs, then deletes those refs.  Run once
per repository after upgrading from the notes backend;
git-appraise's own notes (refs/notes/devtools/*) are untouched.
REPO defaults to the current review's repository."
  (interactive)
  (let* ((repo (or repo (git-appraise-utils--repo-dir)
                   (user-error "No git repository in this context")))
         (plan (git-appraise-store-import-read repo)))
    (if (null plan)
        (message "No unimported git-appraise notes in %s" repo)
      (let* ((summary
              (mapconcat
               (lambda (entry)
                 (pcase-let ((`(,_ref ,_tip ,kind ,rows ,skipped) entry))
                   (concat (format "%d %s" rows
                                   (pcase kind
                                     (`viewed "viewed") (`fold "fold")
                                     (`pr-state "draft")))
                           (if (> skipped 0)
                               (format " (%d unreadable lines skipped)" skipped)
                             ""))))
               plan ", "))
             (existing (git-appraise-store-contains-repo-p repo)))
        (unless (y-or-n-p
                 (format "Import %s from git notes and delete the notes refs?%s "
                         summary
                         (if existing
                             (concat "\n("
                                     "the store already holds state for this"
                                     " repository; newest snapshots win)")
                           "")))
          (user-error "Import cancelled"))
        (pcase-let ((`(,stored . ,deleted)
                     (git-appraise-store-import-apply repo plan)))
          (message "git-appraise: imported %d records from git notes, deleted %d refs%s"
                   stored deleted
                   (if (< deleted (length plan))
                       " (some refs could not be deleted)"
                     "")))))))

(provide 'git-appraise-store-import)
;;; git-appraise-store-import.el ends here
