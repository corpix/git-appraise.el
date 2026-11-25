;;; git-appraise-backend.el --- Local git-appraise backend facade and writes -*- lexical-binding: t; -*-

;;; Commentary:
;; Native git-appraise commands.  Review reads live in backend-read.

;;; Code:

(require 'git-appraise-backend-read)
(require 'git-appraise-db)
(require 'git-appraise-utils)
(require 'dash)
(require 'subr-x)
(declare-function git-appraise-backend--base "git-appraise-backend-read")

(defclass git-appraise-native-publication ()
  ((state :initform nil)
   (pr :initform nil)
   (local-comments :initform nil)
   (feedback :initform nil)))

(defclass git-appraise-native-replies ()
  ((pr :initform nil)
   (replies :initform nil)))

(defun git-appraise-backend--send-comment (pr body &optional path line parent status)
  "Write BODY to PR at PATH/LINE or below PARENT.
STATUS is nil for FYI, `needs-work' or `resolved'."
  (when (string-empty-p (string-trim (or body "")))
    (user-error "git-appraise comments require a message"))
  (let ((args (list "comment" "-F" "-")))
    (pcase status
      ('needs-work (setq args (append args '("-nmw"))))
      ('resolved (setq args (append args '("-lgtm")))))
    (when parent (setq args (append args (list "-p" parent))))
    (when path (setq args (append args (list "-f" path))))
    (when line (setq args (append args (list "-l" (number-to-string line)))))
    ;; Go's flag parser stops at the first positional argument.
    (apply #'git-appraise-backend--run-message
           (oref pr owner) body
           (append args (list (oref pr number))))))

(defun git-appraise-backend-update-request (pr description)
  "Append an updated native request for PR with DESCRIPTION.
Keep the source, target, and reviewers from the current request."
  (let* ((repo (oref pr owner))
         (revision (oref pr number))
         (request (a-get (git-appraise-backend--read-review repo revision)
                         'request))
         (source (a-get request 'reviewRef))
         (target (a-get request 'targetRef))
         (reviewers (a-get request 'reviewers)))
    (unless (and (stringp source) (stringp target)
                 (not (string-empty-p source))
                 (not (string-empty-p target)))
      (user-error "Cannot update this review request without its source and target refs"))
    (apply #'git-appraise-backend--run-message
           repo description
           (append (list "request" "-F" "-"
                         "-source" source "-target" target)
                   (when reviewers
                     (list "-r" (string-join reviewers ",")))
                   (list revision)))))

(defun git-appraise-backend--line-for-comment (pr comment)
  "Return the head-file line for COMMENT in PR, or signal a useful error.
Never reuse a stored diff position after the review head moves."
  (when (oref pr sha)
    (let* ((review (git-appraise-backend--json
                    (git-appraise-backend--run
                     (oref pr owner) "show" "-json" (oref pr number))
                    "show"))
           (head (git-appraise-backend--head (oref pr owner) review)))
      (unless (equal (oref pr sha) head)
        ;; A position in the old aggregate diff may designate a different
        ;; line after new commits.  Keep the draft for explicit re-anchoring.
        (git-appraise-backend-refresh
         pr (git-appraise-pr-buffer-name pr)
         "Review head changed; refreshed")
        (user-error "Review head changed; re-anchor this draft before sending"))
      (when (and (slot-boundp pr 'native-review)
                 (oref pr native-review)
                 (not (eq (oref (oref pr native-review) submitted)
                          (eq (a-get review 'submitted) t))))
        (git-appraise-backend-refresh
         pr (git-appraise-pr-buffer-name pr)
         "Review integration state changed; refreshed")
        (user-error "Review integration state changed; re-anchor this draft before sending"))
      (when (and (slot-boundp pr 'native-review)
                 (oref pr native-review)
                 (not (equal (oref (oref pr native-review) base)
                             (git-appraise-backend--base
                              (oref pr owner) review head))))
        (git-appraise-backend-refresh
         pr (git-appraise-pr-buffer-name pr)
         "Review base changed; refreshed")
        (user-error "Review base changed; re-anchor this draft before sending"))))
  (let* ((path (oref comment path))
         (side (and (slot-boundp comment 'side) (oref comment side)))
         (tables (or (oref pr diff-tables)
                     (let ((parsed (git-appraise-backend--diff-tables
                                    (oref pr raw-diff))))
                       (oset pr diff-tables parsed)
                       parsed)))
         (position (or (oref comment position)
                       (and (equal side "RIGHT")
                            (numberp (oref comment line))
                            (git-appraise-backend--position
                             tables path (oref comment line)))))
         (row (git-appraise-backend--row tables path position))
         (type (a-get row 'type)))
    (when (or (equal side "LEFT")
              (string-equal (oref comment line-type) "REMOVED"))
      (user-error "git-appraise cannot comment on a removed line"))
    (unless row
      (user-error "No current diff line at %s:%s; refresh the review and retry"
                  path (or (oref comment line) position)))
    (when (string-equal type "del")
      (user-error "git-appraise cannot comment on a removed line"))
    (or (a-get row 'ln2) (a-get row 'ln))))

(defun git-appraise-backend-send-code-comment (pr local-comment callback)
  "Send LOCAL-COMMENT in PR and call CALLBACK on success."
  (let ((path (oref local-comment path)))
    (if (equal (oref local-comment line-type) "FILE")
        (git-appraise-backend--send-comment pr (oref local-comment msg) path)
      (git-appraise-backend--send-comment
       pr (oref local-comment msg) path
       (git-appraise-backend--line-for-comment pr local-comment))))
  (funcall callback))

(defun git-appraise-backend-send-general-comment (pr comment-msg callback)
  "Send a general COMMENT-MSG for PR and call CALLBACK."
  (git-appraise-backend--send-comment pr comment-msg)
  (funcall callback))

(defun git-appraise-backend-send-reply (pr parent body &optional resolve)
  "Send BODY as a reply to PARENT in PR; RESOLVE marks it addressed."
  (unless (and (stringp parent) (not (string-empty-p parent)))
    (user-error "Cannot reply: git-appraise comment hash is missing"))
  (git-appraise-backend--send-comment
   pr body nil nil parent (when resolve 'resolved)))

(defun git-appraise-backend-publish-review (review callback)
  "Send REVIEW comments and verdict, then call CALLBACK."
  (let* ((pr (oref review pr))
         (state (oref review state))
         (feedback (oref review feedback)))
    (condition-case err
        (progn
          (when (and (equal state "COMMENT")
                     (not (oref review local-comments))
                     (string-empty-p (string-trim (or feedback ""))))
            (user-error "Add a comment before submitting"))
          (when (and (equal state "REQUEST_CHANGES")
                     (string-empty-p (string-trim (or feedback ""))))
            (user-error "A message is required to request changes"))
          (--each (reverse (oref review local-comments))
            (let* ((comment it)
                   (path (oref comment path)))
              (git-appraise-backend--send-comment
               pr (oref comment body) path
               (unless (equal (oref comment line-type) "FILE")
                 (git-appraise-backend--line-for-comment pr comment))
               nil
               (when (and (equal state "NEEDS_WORK")
                          (not (oref comment fyi)))
                 'needs-work))
              (git-appraise-db-delete-raw-comment (oref comment internal-id))))
          (pcase state
            ("APPROVE"
             (git-appraise-backend--run-message
              (oref pr owner) (or feedback "")
              "accept" "-F" "-" (oref pr number)))
            ("REQUEST_CHANGES"
             (git-appraise-backend--run-message
              (oref pr owner) feedback
              "reject" "-F" "-" (oref pr number)))
            ("NEEDS_WORK"
             (when (and feedback (not (string-empty-p (string-trim feedback))))
               (git-appraise-backend--send-comment
                pr feedback nil nil nil
                (unless (--any? (not (oref it fyi))
                                (oref review local-comments))
                  'needs-work))))
            ("COMMENT"
             (when (and feedback (not (string-empty-p (string-trim feedback))))
               (git-appraise-backend--send-comment pr feedback)))
            (_ (user-error "Unsupported git-appraise review action: %s" state)))
          (funcall callback))
      (error
       ;; Successful comments were removed from the draft cache one by one.
       ;; Redraw the remaining drafts before a retry can send them again.
       (let ((git-appraise-section-full-refresh? nil))
         (ignore-errors (git-appraise--build-buffer)))
       (signal (car err) (cdr err))))))

(defun git-appraise-backend-publish-replies (replies callback)
  "Send REPLIES to git-appraise and call CALLBACK."
  (--each (oref replies replies)
    (git-appraise-backend-send-reply
     (oref replies pr) (oref it reply-to-id) (oref it body)
     (oref it resolve))
    (git-appraise-db-delete-raw-comment (oref it internal-id)))
  (funcall callback))

(defun git-appraise-backend-abandon (directory revision message)
  "Abandon REVIEW REVISION in DIRECTORY with MESSAGE."
  (let ((msg (or message "")))
    (apply #'git-appraise-backend--run directory
           (append (if (string-empty-p (string-trim msg))
                       '("abandon")
                     (list "abandon" "-m" msg))
                   (list revision)))))

;;;###autoload
(defun git-appraise-abandon-review (revision)
  "Abandon git-appraise REVISION in the current repository."
  (interactive
   (list (git-appraise-backend-select-review)))
  (let* ((root (git-appraise-backend--root))
         (message (read-string "Abandon message (optional): ")))
    (git-appraise-backend-abandon root revision message)
    (message "Review abandoned: %s" revision)))

(defun git-appraise-backend-push (directory &optional remote)
  "Push git-appraise notes from DIRECTORY to REMOTE.
REMOTE defaults to \"origin\"."
  (git-appraise-backend--run directory "push" (or remote "origin")))

(defun git-appraise-backend-pull (directory &optional remote)
  "Pull git-appraise notes into DIRECTORY from REMOTE.
REMOTE defaults to \"origin\"."
  (git-appraise-backend--run directory "pull" (or remote "origin")))

;;;###autoload
(defun git-appraise-push-notes (&optional remote)
  "Push git-appraise notes to REMOTE."
  (interactive
   (list (read-string "Remote: " "origin")))
  (let ((root (git-appraise-backend--root)))
    (message "Pushing git-appraise notes to %s..." remote)
    (git-appraise-backend-push root remote)
    (message "Pushed git-appraise notes to %s" remote)))

;;;###autoload
(defun git-appraise-pull-notes (&optional remote)
  "Pull git-appraise notes from REMOTE."
  (interactive
   (list (read-string "Remote: " "origin")))
  (let ((root (git-appraise-backend--root)))
    (message "Pulling git-appraise notes from %s..." remote)
    (git-appraise-backend-pull root remote)
    (when (fboundp 'git-appraise--build-buffer)
      (ignore-errors (git-appraise--build-buffer)))
    (message "Pulled git-appraise notes from %s" remote)))

(defun git-appraise-backend-rebase (directory &optional revision)
  "Rebase git-appraise REVIEW REVISION in DIRECTORY onto its target."
  (let ((process-environment
         (append '("GIT_SEQUENCE_EDITOR=true" "GIT_EDITOR=true")
                 process-environment)))
    (apply #'git-appraise-backend--run directory
           (append '("rebase") (when revision (list revision))))))

;;;###autoload
(defun git-appraise-rebase-review (revision)
  "Rebase git-appraise REVISION in the current repository."
  (interactive
   (list (git-appraise-backend-select-review)))
  (unless (yes-or-no-p (format "Rebase review %s onto its target? " revision))
    (user-error "Rebase cancelled"))
  (let ((root (git-appraise-backend--root)))
    (message "Rebasing review %s..." revision)
    (git-appraise-backend-rebase root revision)
    (message "Review rebased: %s" revision)))

(defun git-appraise-backend-land-review (directory &optional revision strategy)
  "Land git-appraise REVIEW REVISION in DIRECTORY.
STRATEGY may be nil, \"merge\", \"rebase\", or \"fast-forward\"."
  (let ((process-environment
         (append '("GIT_SEQUENCE_EDITOR=true" "GIT_EDITOR=true")
                 process-environment))
        (args (append '("submit")
                      (pcase strategy
                        ("merge" '("--merge"))
                        ("rebase" '("--rebase"))
                        ("fast-forward" '("--fast-forward"))
                        ((or 'nil "") nil)
                        (_ (user-error "Unknown submit strategy: %s" strategy)))
                      (when revision (list revision)))))
    (apply #'git-appraise-backend--run directory args)))

;;;###autoload
(defun git-appraise-land-review (revision strategy)
  "Land git-appraise REVISION in the current repository using STRATEGY."
  (interactive
   (let ((rev (git-appraise-backend-select-review))
         (strategy (completing-read "Submit strategy: "
                                    '("default" "merge" "rebase" "fast-forward")
                                    nil t nil nil "default")))
     (list rev (unless (equal strategy "default") strategy))))
  (unless (yes-or-no-p (format "Land review %s%s? "
                               revision
                               (if strategy (format " (%s)" strategy) "")))
    (user-error "Land cancelled"))
  (let ((root (git-appraise-backend--root)))
    (message "Landing review %s..." revision)
    (git-appraise-backend-land-review root revision strategy)
    (message "Review landed: %s" revision)))

(provide 'git-appraise-backend)
;;; git-appraise-backend.el ends here
