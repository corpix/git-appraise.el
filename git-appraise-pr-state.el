;;; git-appraise-pr-state.el --- Persist unsent review state in git notes -*- lexical-binding: t; -*-

;;; Commentary:
;; Durable storage for local, unsent review state (feedback and draft
;; comments/replies).  Persisted as JSONL snapshot events in a personal
;; git notes ref, keyed by a deterministic blob anchor derived from the
;; review identity.  This is the DB for local review state: in-memory
;; objects are caches.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'uuidgen)
(require 'a)
(require 'eieio)
(require 'git-appraise-metadata)
(require 'git-appraise-time)

(declare-function git-appraise-db-get-pullreq "git-appraise-db")
(declare-function git-appraise-db-update "git-appraise-db")
(declare-function git-appraise-db--local-comment-entry-p "git-appraise-db")
(declare-function git-appraise-viewed--repo-dir "git-appraise-viewed")

(defcustom git-appraise-pr-state-notes-ref "refs/notes/git-appraise-el/pr-state"
  "Personal git notes ref for unsent review state.
Stores feedback and local draft comments/replies as append-only
JSONL snapshot events."
  :group 'git-appraise
  :type 'string)

(defconst git-appraise-pr-state-version 1
  "Envelope version for pr-state records.")

(defun git-appraise-pr-state--repo (pr)
  "Return the git repository directory for PR, or nil."
  (or (ignore-errors (git-appraise-viewed--repo-dir))
      (let* ((dir (and (boundp 'git-appraise-repo-worktree)
                       (symbol-value 'git-appraise-repo-worktree)))
             (dir (or (and (stringp dir) dir)
                      (and (eieio-object-p pr) (slot-boundp pr 'host) (oref pr host))
                      (and (eieio-object-p pr) (slot-boundp pr 'owner) (oref pr owner)))))
        (when (and (stringp dir)
                   (not (string-empty-p dir))
                   (file-directory-p dir))
          (file-name-as-directory dir)))))

(defun git-appraise-pr-state--identity (pr)
  "Return a stable identity string for PR."
  (format "%s/%s#%s"
          (or (and (slot-boundp pr 'owner) (oref pr owner)) "")
          (or (and (slot-boundp pr 'repo) (oref pr repo)) "")
          (or (and (slot-boundp pr 'number) (oref pr number)) "")))

(defun git-appraise-pr-state--anchor (repo pr)
  "Return a git object sha used as the notes anchor for PR in REPO.
Uses a deterministic blob derived from PR identity so the anchor is
stable across head changes and restarts."
  (let* ((key (format "git-appraise-pr-state:v%d:%s"
                      git-appraise-pr-state-version
                      (git-appraise-pr-state--identity pr)))
         (tmp (make-temp-file "git-appraise-pr-state-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert key))
          (pcase-let ((`(,code ,stdout ,stderr)
                       (git-appraise-metadata--git
                        repo (list "hash-object" "-w" tmp))))
            (if (zerop code)
                (string-trim stdout)
              (error "git hash-object failed: %s" (string-trim stderr)))))
      (ignore-errors (delete-file tmp)))))

(defun git-appraise-pr-state--strip-text-props (obj)
  "Return OBJ with text properties stripped from all strings."
  (cond
   ((stringp obj) (substring-no-properties obj))
   ((consp obj)
    (cons (git-appraise-pr-state--strip-text-props (car obj))
          (git-appraise-pr-state--strip-text-props (cdr obj))))
   ((vectorp obj)
    (apply #'vector (mapcar #'git-appraise-pr-state--strip-text-props (append obj nil))))
   (t obj)))

(defun git-appraise-pr-state--local-comments (pr)
  "Return local draft raw-comment entries from PR, sanitized."
  (let ((raw (and (slot-boundp pr 'raw-comments) (oref pr raw-comments))))
    (git-appraise-pr-state--strip-text-props
     (--filter (git-appraise-db--local-comment-entry-p it) raw))))

(defun git-appraise-pr-state--snapshot (pr actor)
  "Build a pr-state snapshot event for PR by ACTOR."
  (let ((feedback (and (slot-boundp pr 'feedback) (oref pr feedback))))
    `((v . ,git-appraise-pr-state-version)
      (type . "pr-state")
      (id . ,(uuidgen-4))
      (actor . ,actor)
      (pr . ,(git-appraise-pr-state--identity pr))
      (feedback . ,(if (and (stringp feedback) (not (string-empty-p feedback)))
                       (substring-no-properties feedback)
                     :json-false))
      (local_comments . ,(or (git-appraise-pr-state--local-comments pr) []))
      (at . ,(git-appraise-time-now-utc t)))))

(defun git-appraise-pr-state--valid-p (obj)
  "Return (t . nil) when OBJ is a valid pr-state record, else (nil . REASON)."
  (cond
   ((not (and (listp obj) (a-get obj 'type)))
    (cons nil "not a JSON object"))
   ((not (and (numberp (a-get obj 'v))
              (= (a-get obj 'v) git-appraise-pr-state-version)))
    (cons nil (format "unsupported version %S" (a-get obj 'v))))
   ((not (equal (a-get obj 'type) "pr-state"))
    (cons nil (format "unknown type %S" (a-get obj 'type))))
   ((not (and (stringp (a-get obj 'id)) (not (string-empty-p (a-get obj 'id)))))
    (cons nil "missing id"))
   ((not (and (stringp (a-get obj 'at)) (not (string-empty-p (a-get obj 'at)))))
    (cons nil "missing timestamp"))
   ((not (stringp (a-get obj 'pr)))
    (cons nil "missing pr identity"))
   (t (cons t nil))))

(defun git-appraise-pr-state--read (repo pr)
  "Read latest valid pr-state snapshot for PR in REPO.
Return the snapshot alist, or nil when no valid state exists."
  (condition-case err
      (let* ((anchor (git-appraise-pr-state--anchor repo pr))
             (res (git-appraise-metadata--git
                   repo (list "notes" (concat "--ref=" git-appraise-pr-state-notes-ref)
                              "show" anchor))))
        (pcase res
          (`(0 ,stdout ,_)
           (let (latest)
             (dolist (line (split-string stdout "\n" t))
               (condition-case _
                   (let ((obj (with-temp-buffer
                                (insert line)
                                (let ((json-object-type 'alist)
                                      (json-array-type 'list)
                                      (json-false :json-false))
                                  (goto-char (point-min))
                                  (json-read)))))
                     (when (car (git-appraise-pr-state--valid-p obj))
                       (when (or (not latest)
                                 (string< (a-get latest 'at) (a-get obj 'at))
                                 (and (equal (a-get latest 'at) (a-get obj 'at))
                                      (string< (a-get latest 'id) (a-get obj 'id))))
                         (setq latest obj))))
                 (error nil)))
             latest))
          (`(1 ,_ ,stderr)
           (if (string-match-p "no note found" stderr) nil
             (message "pr-state read failed: %s" (string-trim stderr))
             nil))
          (_ (message "pr-state read failed: %s" (string-trim (nth 2 res))) nil)))
    (error
     (message "pr-state read failed: %s" (error-message-string err))
     nil)))

(defun git-appraise-pr-state--actor (repo)
  "Return git user.email for REPO, or nil."
  (pcase-let ((`(,code ,stdout ,_)
               (git-appraise-metadata--git repo '("config" "user.email"))))
    (when (zerop code)
      (let ((email (string-trim stdout)))
        (unless (string-empty-p email) email)))))

(defun git-appraise-pr-state-persist (pr)
  "Persist unsent local state for PR to git notes.
No-op when PR/repo/actor are unavailable.  Signals are caught and
reported as messages so persistence never breaks review flow."
  (condition-case err
      (let ((repo (git-appraise-pr-state--repo pr)))
        (when (and repo (eieio-object-p pr))
          (let ((actor (git-appraise-pr-state--actor repo))
                (anchor (git-appraise-pr-state--anchor repo pr)))
            (when actor
              (git-appraise-metadata-append
               repo git-appraise-pr-state-notes-ref anchor
               (list (git-appraise-pr-state--snapshot pr actor)))))))
    (error
     (message "pr-state persist failed: %s" (error-message-string err))))
  pr)

(defun git-appraise-pr-state-hydrate (pr)
  "Hydrate PR with persisted local feedback and draft comments.
Merges local drafts into raw-comments without duplicating existing
entries; restores feedback when PR has no feedback."
  (condition-case err
      (let ((repo (git-appraise-pr-state--repo pr)))
        (when (and repo (eieio-object-p pr))
          (let ((snap (git-appraise-pr-state--read repo pr)))
            (when snap
              ;; feedback
              (let ((fb (a-get snap 'feedback)))
                (when (and (stringp fb)
                           (not (string-empty-p fb))
                           (slot-boundp pr 'feedback)
                           (not (and (stringp (oref pr feedback))
                                     (not (string-empty-p (oref pr feedback))))))
                  (oset pr feedback (substring-no-properties fb))))
              ;; local draft comments
              (let* ((local (a-get snap 'local_comments))
                     (local (cond ((vectorp local) (append local nil))
                                  ((listp local) local)
                                  (t nil)))
                     (local (git-appraise-pr-state--strip-text-props local))
                     (existing (and (slot-boundp pr 'raw-comments)
                                    (oref pr raw-comments)))
                     (existing-ids
                      (let (ids)
                        (dolist (entry existing)
                          (dolist (n (a-get-in entry '(comments nodes)))
                            (when-let* ((id (or (a-get n 'internal-id)
                                                (a-get n 'databaseId))))
                              (push (format "%s" id) ids))))
                        ids))
                     (new-local
                      (--filter
                       (let ((nodes (a-get-in it '(comments nodes))))
                         (--any? (let ((id (or (a-get it 'internal-id)
                                               (a-get it 'databaseId))))
                                   (not (and id (member (format "%s" id) existing-ids))))
                                 nodes))
                       local)))
                (when new-local
                  (oset pr raw-comments (append existing new-local))))))))
    (error
     (message "pr-state hydrate failed: %s" (error-message-string err))))
  pr)

(provide 'git-appraise-pr-state)
;;; git-appraise-pr-state.el ends here
