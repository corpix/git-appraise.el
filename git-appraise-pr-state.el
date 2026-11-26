;;; git-appraise-pr-state.el --- Persist unsent review state -*- lexical-binding: t; -*-

;;; Commentary:
;; Durable storage for local, unsent review state (feedback and
;; draft comments/replies).  Snapshots are appended to the personal
;; SQLite store (`git-appraise-store.el'), keyed by the repository,
;; the review revision and the reviewer; the newest snapshot wins
;; on read.  In-memory objects are caches.

;;; Code:

(require 'dash)
(require 'uuidgen)
(require 'a)
(require 'eieio)

(require 'git-appraise-store)
(require 'git-appraise-time)

(declare-function git-appraise-db-get-pullreq "git-appraise-db")
(declare-function git-appraise-db-update "git-appraise-db")
(declare-function git-appraise-db--local-comment-entry-p "git-appraise-db")
(declare-function git-appraise-viewed--repo-dir "git-appraise-viewed")

(defconst git-appraise-pr-state-version 2
  "Record version for pr-state snapshots.")

(defun git-appraise-pr-state--repo (pr)
  "Return the git repository directory for PR, or nil."
  (or (let ((owner (and (eieio-object-p pr)
                        (slot-boundp pr 'owner)
                        (oref pr owner))))
        (and (stringp owner) (file-directory-p owner)
             (file-name-as-directory owner)))
      (ignore-errors (git-appraise-viewed--repo-dir))
      (let* ((dir (and (boundp 'git-appraise-repo-worktree)
                       (symbol-value 'git-appraise-repo-worktree)))
             (dir (or (and (stringp dir) dir)
                      (and (eieio-object-p pr) (slot-boundp pr 'host) (oref pr host))
                      (and (eieio-object-p pr) (slot-boundp pr 'owner) (oref pr owner)))))
        (when (and (stringp dir)
                   (not (string-empty-p dir))
                   (file-directory-p dir))
          (file-name-as-directory dir)))))

(defun git-appraise-pr-state--revision (pr)
  "The review revision PR's state is stored under, or nil."
  (and (slot-boundp pr 'number) (oref pr number)))

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
  "Build a pr-state snapshot for PR by ACTOR."
  (let ((feedback (and (slot-boundp pr 'feedback) (oref pr feedback))))
    `((v . ,git-appraise-pr-state-version)
      (id . ,(uuidgen-4))
      (actor . ,actor)
      (feedback . ,(and (stringp feedback) (not (string-empty-p feedback))
                        (substring-no-properties feedback)))
      (local_comments . ,(git-appraise-pr-state--local-comments pr))
      (at . ,(git-appraise-time-now-utc t)))))

(defun git-appraise-pr-state--actor (repo)
  "Return git user.email for REPO, or nil."
  (let ((email (git-appraise-utils--git repo "config" "user.email")))
    (and email (not (string-empty-p email)) email)))

(defun git-appraise-pr-state-persist (pr &optional signal-errors)
  "Persist unsent local state for PR to the store.
No-op when PR/repo/actor are unavailable.  Signals are caught and
reported as messages unless SIGNAL-ERRORS is non-nil."
  (condition-case err
      (let ((repo (git-appraise-pr-state--repo pr)))
        (when (and signal-errors (not repo))
          (error "Cannot save draft: review repository is unavailable"))
        (when (and repo (eieio-object-p pr))
          (let ((actor (git-appraise-pr-state--actor repo))
                (revision (git-appraise-pr-state--revision pr)))
            (when (and signal-errors (not actor))
              (error "Cannot save draft: git user.email is unavailable"))
            (when (and actor revision)
              (git-appraise-store-save-pr-state
               repo revision actor
               (git-appraise-pr-state--snapshot pr actor))))))
    (error
     (if signal-errors
         (signal (car err) (cdr err))
       (message "pr-state persist failed: %s" (error-message-string err)))))
  pr)

(defun git-appraise-pr-state-hydrate (pr)
  "Hydrate PR with persisted local feedback and draft comments.
Merges local drafts into raw-comments without duplicating existing
entries; restores feedback when PR has no feedback."
  (condition-case err
      (let ((repo (git-appraise-pr-state--repo pr)))
        (when (and repo (eieio-object-p pr))
          (let* ((actor (git-appraise-pr-state--actor repo))
                 (revision (git-appraise-pr-state--revision pr))
                 (snap (and actor revision
                            (git-appraise-store-pr-state repo revision actor))))
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
              (let* ((local (git-appraise-pr-state--strip-text-props
                             (a-get snap 'local_comments)))
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
