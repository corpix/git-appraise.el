;;; git-appraise-fold-state.el --- Persistent review folds -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal fold choices, anchored to the review revision in the
;; personal SQLite store (`git-appraise-store.el').

;;; Code:
(require 'a)
(require 'dash)
(require 'uuidgen)
(require 'git-appraise-store)
(require 'git-appraise-time)
(require 'git-appraise-viewed)
(defvar git-appraise-fold-hunks-by-default)

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

(defun git-appraise-fold-state--read (repo revision &optional actor)
  "The stored fold events for REVISION in REPO."
  (git-appraise-store-events repo revision 'fold actor))

(defun git-appraise-fold-state--prepare ()
  "Project saved fold choices onto the current diff inventory."
  (let* ((repo (git-appraise-viewed--repo-dir))
         (revision (git-appraise-viewed--revision))
         (actor (and repo (git-appraise-viewed--actor repo)))
         (index (make-hash-table :test #'equal)))
    (when (and repo revision actor)
      (condition-case err
          (--each (git-appraise-fold-state--read repo revision actor)
            (puthash (git-appraise-fold-state--event-key it) it index))
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
  "Whether hunk SECTION is folded according to saved choices.
Hunks without a saved choice are never folded: only their file's
fold conceals them, so any way of opening the file reveals them."
  (pcase (git-appraise-fold-state--hunk-identity section)
    (`(,path ,fp ,range)
     (let ((choice (git-appraise-fold-state--choice "hunk" path fp range)))
       (if (eq choice 'unset) nil choice)))
    (_ git-appraise-fold-hunks-by-default)))

(defun git-appraise-fold-state--make-event (item path fp range folded actor)
  "Make a persistent fold event for ITEM, PATH, FP, RANGE and ACTOR."
  `((v . 1) (type . "fold") (id . ,(uuidgen-4))
    (actor . ,actor) (item . ,item) (path . ,path)
    (fingerprint . ,fp)
    ,@(when range `((range . ,range)))
    (folded . ,(and folded t))
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
          (git-appraise-store-append-events repo revision 'fold events)
        (error (user-error "Could not save fold choice: %s"
                           (error-message-string err))))
      (--each events
        (puthash (git-appraise-fold-state--event-key it) it
                 git-appraise-fold-state--index))
      events)))

(provide 'git-appraise-fold-state)
;;; git-appraise-fold-state.el ends here
