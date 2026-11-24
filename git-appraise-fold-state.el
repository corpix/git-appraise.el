;;; git-appraise-fold-state.el --- Persistent review folds -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal fold choices, anchored to the review revision in Git notes.

;;; Code:
(require 'a)
(require 'dash)
(require 'json)
(require 'uuidgen)
(require 'git-appraise-metadata)
(require 'git-appraise-time)
(require 'git-appraise-viewed)
(defvar git-appraise-fold-hunks-by-default)

(defcustom git-appraise-fold-notes-ref "refs/notes/git-appraise-el/folds"
  "Personal Git notes ref used for review fold choices."
  :group 'git-appraise
  :type 'string)

(defvar-local git-appraise-fold-state--index nil
  "Current review's fold events indexed by diff identity.")
(put 'git-appraise-fold-state--index 'permanent-local t)

(defun git-appraise-fold-state--file-fingerprint (info)
  "Return a stable content identity for file inventory INFO."
  (or (plist-get info :file-fp)
      (secure-hash
       'sha256
       (prin1-to-string
        (--map (list (plist-get it :fp) (plist-get it :range))
               (plist-get info :hunks))))))

(defun git-appraise-fold-state--event-key (event)
  "Return the identity key of fold EVENT."
  (list (a-get event 'item) (a-get event 'path)
        (a-get event 'fingerprint) (a-get event 'range)))

(defun git-appraise-fold-state--read (repo revision)
  "Read valid fold events for REVISION in REPO."
  (pcase-let ((`(,code ,stdout ,stderr)
               (git-appraise-metadata--git
                repo (list "notes" (concat "--ref=" git-appraise-fold-notes-ref)
                           "show" revision))))
    (cond
     ((zerop code)
      (--keep
       (condition-case nil
           (let* ((json-object-type 'alist)
                  (json-key-type 'symbol)
                  (json-false :json-false)
                  (event (json-read-from-string it)))
             (when (and (equal (a-get event 'type) "fold")
                        (equal (a-get event 'v) 1)
                        (member (a-get event 'item) '("file" "hunk"))
                        (stringp (a-get event 'path))
                        (stringp (a-get event 'fingerprint))
                        (stringp (a-get event 'actor))
                        (stringp (a-get event 'at))
                        (stringp (a-get event 'id))
                        (memq (a-get event 'folded) '(t :json-false)))
               event))
         (error nil))
       (split-string stdout "\n" t)))
     ((and (= code 1) (string-match-p "no note found" stderr)) nil)
     (t (error "Cannot read fold note: %s" (string-trim stderr))))))

(defun git-appraise-fold-state--prepare ()
  "Project saved fold choices onto the current diff inventory."
  (let* ((repo (git-appraise-viewed--repo-dir))
         (revision (git-appraise-viewed--revision))
         (actor (and repo (git-appraise-viewed--actor repo)))
         (index (make-hash-table :test #'equal)))
    (when (and repo revision actor)
      (condition-case err
          (--each (git-appraise-fold-state--read repo revision)
            (when (equal (a-get it 'actor) actor)
              (let* ((key (git-appraise-fold-state--event-key it))
                     (old (gethash key index)))
                (when (or (not old)
                          (string< (a-get old 'at) (a-get it 'at))
                          (and (equal (a-get old 'at) (a-get it 'at))
                               (string< (a-get old 'id) (a-get it 'id))))
                  (puthash key it index)))))
        (error (message "Fold choices unavailable: %s"
                        (error-message-string err)))))
    (setq git-appraise-fold-state--index index)))

(defun git-appraise-fold-state--choice (item path fp &optional range)
  "Return saved fold choice for ITEM at PATH, FP and RANGE, or `unset'."
  (let ((event (and (hash-table-p git-appraise-fold-state--index)
                    (gethash (list item path fp range)
                             git-appraise-fold-state--index))))
    (if event (eq (a-get event 'folded) t) 'unset)))

(defun git-appraise-fold-state--file-folded-p (path &optional default)
  "Whether PATH is folded, using DEFAULT if it has no saved choice."
  (let* ((info (and (hash-table-p git-appraise-viewed--buffer-inventory)
                    (gethash path git-appraise-viewed--buffer-inventory)))
         (fp (and info (git-appraise-fold-state--file-fingerprint info)))
         (choice (and fp (git-appraise-fold-state--choice "file" path fp))))
    (if (eq choice 'unset) default choice)))

(defun git-appraise-fold-state--hunk-identity (section)
  "Return (PATH FP RANGE) for hunk SECTION, or nil."
  (let* ((spec (oref section value))
         ;; Rename and chmod headers are hunk sections too, but their
         ;; values are symbols rather than the alist used by diff hunks.
         (diff-hunk? (and (consp spec) (consp (car spec))))
         (path (and diff-hunk? (a-get spec 'path)))
         (ranges (and diff-hunk? (a-get spec 'ranges)))
         (range (and ranges
                     (git-appraise-viewed--range-key
                      (git-appraise-viewed--parse-ranges ranges))))
         (info (and path (hash-table-p git-appraise-viewed--buffer-inventory)
                    (gethash path git-appraise-viewed--buffer-inventory)))
         (hunk (and info (--first (equal (plist-get it :range) range)
                                  (plist-get info :hunks)))))
    (when hunk (list path (plist-get hunk :fp) range))))

(defun git-appraise-fold-state--hunk-folded-p (section)
  "Whether hunk SECTION is folded according to saved choices."
  (pcase (git-appraise-fold-state--hunk-identity section)
    (`(,path ,fp ,range)
     (let ((choice (git-appraise-fold-state--choice "hunk" path fp range)))
       (if (eq choice 'unset) git-appraise-fold-hunks-by-default choice)))
    (_ git-appraise-fold-hunks-by-default)))

(defun git-appraise-fold-state--make-event (item path fp range folded actor)
  "Make a persistent fold event for ITEM, PATH, FP, RANGE and ACTOR."
  `((v . 1) (type . "fold") (id . ,(uuidgen-4))
    (actor . ,actor) (item . ,item) (path . ,path)
    (fingerprint . ,fp)
    ,@(when range `((range . ,range)))
    (folded . ,(if folded t :json-false))
    (at . ,(git-appraise-time-now-utc t))))

(defun git-appraise-fold-state--write (targets folded)
  "Persist TARGETS as FOLDED before changing their visible sections.
Each target is (ITEM PATH FP RANGE).  Return the written events."
  (let* ((repo (git-appraise-viewed--repo-dir))
         (revision (git-appraise-viewed--revision))
         (actor (and repo (git-appraise-viewed--actor repo))))
    (unless (and repo revision actor)
      (user-error "Set git user.email in the review repository to save folds"))
    (let ((events (--map (pcase-let ((`(,item ,path ,fp ,range) it))
                           (git-appraise-fold-state--make-event
                            item path fp range folded actor))
                         targets)))
      (condition-case err
          (git-appraise-metadata-append
           repo git-appraise-fold-notes-ref revision events)
        (error (user-error "Could not save fold choice: %s"
                           (error-message-string err))))
      (--each events
        (puthash (git-appraise-fold-state--event-key it) it
                 git-appraise-fold-state--index))
      events)))

(provide 'git-appraise-fold-state)
;;; git-appraise-fold-state.el ends here
