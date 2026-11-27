;;; git-appraise-remove.el --- Remove local review notes -*- lexical-binding: t; -*-

;;; Commentary:
;; git-appraise stores requests and discussions in different notes refs.
;; Removing only a request leaves comments and decisions attached to the
;; same commit.  A fresh review of that commit needs both notes removed.

;;; Code:

(require 'git-appraise-backend)
(require 'dash)
(require 'subr-x)
(require 'git-appraise-store)
(require 'git-appraise-db)
(declare-function git-appraise-pr-buffer-name "git-appraise-section" (&optional pr))
(declare-function git-appraise-store-clear-review "git-appraise-store" (repo revision))
(defvar git-appraise-comment-review-buffer)
(defvar git-appraise-review-buffer-pr-id)

(defconst git-appraise-remove--request-ref "refs/notes/devtools/reviews")
(defconst git-appraise-remove--discussion-ref "refs/notes/devtools/discuss")

(defun git-appraise-remove--remote-tip (root remote)
  "Return REMOTE's request-notes tip in ROOT, :none, or :unknown.
The check is read-only and capped at three seconds."
  (let* ((default-directory (file-name-as-directory root))
         (process-environment (cons "GIT_TERMINAL_PROMPT=0"
                                    process-environment))
         (buffer (generate-new-buffer " *git-appraise-remote-note-check*"))
         (deadline (+ (float-time) 3.0))
         (process nil))
    (unwind-protect
        (condition-case nil
            (progn
              (setq process
                    (make-process
                     :name "git-appraise-remote-note-check"
                     :buffer buffer
                     :command (list "git" "ls-remote" remote
                                    git-appraise-remove--request-ref)
                     :noquery t :sentinel #'ignore))
              (while (and (process-live-p process)
                          (< (float-time) deadline))
                (accept-process-output process 0.05))
              (if (process-live-p process)
                  :unknown
                (if (zerop (process-exit-status process))
                    (with-current-buffer buffer
                      (let ((line (string-trim (buffer-string))))
                        (if (string-empty-p line)
                            :none
                          (car (split-string line "[[:space:]]+" t)))))
                  :unknown)))
          (error :unknown))
      (when (and process (process-live-p process))
        (delete-process process))
      (kill-buffer buffer))))

(defun git-appraise-remove--remote-status (root remote revision)
  "Return whether REMOTE currently has a request note for REVISION.
Result is `published', `not-published', or `unknown'."
  (let ((tip (git-appraise-remove--remote-tip root remote)))
    (cond
     ((eq tip :none) 'not-published)
     ((eq tip :unknown) 'unknown)
     ((not (ignore-errors
             (git-appraise-backend--git
              root "cat-file" "-e" (concat tip "^{commit}"))
             t))
      'unknown)
     ((let ((paths (split-string
                    (git-appraise-backend--git
                     root "ls-tree" "-r" "--name-only" tip)
                    "\n" t)))
        (--any? (equal (replace-regexp-in-string "/" "" it) revision)
                paths))
      'published)
     (t 'not-published))))

(defun git-appraise-remove--warning (root revision)
  "Return a sharing warning for ROOT's REVISION across configured remotes."
  (let ((remotes (split-string
                  (git-appraise-backend--git root "remote") "\n" t))
        published unknown)
    (dolist (remote remotes)
      (pcase (git-appraise-remove--remote-status root remote revision)
        ('published (push remote published))
        ('unknown (push remote unknown))))
    (concat
     (when published
       (format "Review note exists on %s. The next normal notes push will share this removal. "
               (string-join (nreverse published) ", ")))
     (when unknown
       (format "Could not verify %s; the review may be shared. "
               (string-join (nreverse unknown) ", "))))))

(defun git-appraise-remove--note-exists-p (root ref revision)
  "Return non-nil if REF has a note on REVISION in ROOT."
  (let ((default-directory (file-name-as-directory root)))
    (zerop (process-file "git" nil nil nil "notes" (concat "--ref=" ref)
                         "show" revision))))

(defun git-appraise-remove--ref-tip (root ref)
  "Return current tip of REF in ROOT, or nil."
  (ignore-errors
    (git-appraise-backend--git root "rev-parse" "--verify" ref)))

(defun git-appraise-remove--restore-ref (root ref tip)
  "Restore REF in ROOT to TIP after a failed removal."
  (when tip
    (git-appraise-backend--git root "update-ref" ref tip)))

(defun git-appraise-remove--forget-buffer (root revision)
  "Forget ROOT's in-memory review and close its buffer for REVISION."
  (let (ids)
    (maphash
     (lambda (id pr)
       (when (and (equal (oref pr number) revision)
                  (equal (file-name-as-directory
                          (file-truename (oref pr owner)))
                         (file-name-as-directory (file-truename root))))
         (when-let* ((buffer (get-buffer (git-appraise-pr-buffer-name pr))))
           (dolist (candidate (buffer-list))
             (when (with-current-buffer candidate
                     (and (derived-mode-p 'git-appraise-comment-mode)
                          (eq git-appraise-comment-review-buffer buffer)))
               (kill-buffer candidate)))
           (kill-buffer buffer))
         (push id ids)))
     git-appraise-db--objects)
    (dolist (id ids)
      (remhash id git-appraise-db--objects)
      (when (equal git-appraise-db--pullreq-id id)
        (setq git-appraise-db--pullreq-id nil)))))

(defun git-appraise-remove-review-notes (root revision)
  "Remove local request, discussion, and personal state for REVISION.
The Git commit remains intact.  If a step fails, restore the old
notes refs; no garbage collection is performed."
  (unless (git-appraise-remove--note-exists-p
           root git-appraise-remove--request-ref revision)
    (user-error "No local review request note for %s" revision))
  (let* ((request-tip (git-appraise-remove--ref-tip
                       root git-appraise-remove--request-ref))
         (discussion-tip (git-appraise-remove--ref-tip
                          root git-appraise-remove--discussion-ref))
         (discussion? (git-appraise-remove--note-exists-p
                       root git-appraise-remove--discussion-ref revision)))
    (condition-case err
        (progn
          (when discussion?
            (git-appraise-backend--git
             root "notes" (concat "--ref=" git-appraise-remove--discussion-ref)
             "remove" revision))
          (git-appraise-backend--git
           root "notes" (concat "--ref=" git-appraise-remove--request-ref)
           "remove" revision)
          (git-appraise-store-clear-review root revision))
      (error
       (git-appraise-remove--restore-ref
        root git-appraise-remove--request-ref request-tip)
       (git-appraise-remove--restore-ref
        root git-appraise-remove--discussion-ref discussion-tip)
       (signal (car err) (cdr err)))))
  (git-appraise-remove--forget-buffer root revision)
  revision)

;;;###autoload
(defun git-appraise-remove-review (revision)
  "Remove local review REVISION and its discussion and personal state."
  (interactive (list (git-appraise-backend-select-review)))
  (let* ((root (git-appraise-backend--root))
         (warning (git-appraise-remove--warning root revision)))
    (unless (yes-or-no-p
             (concat warning
                     (format "Remove local review %s, its comments, and personal state? "
                             revision)))
    (user-error "Review removal cancelled"))
    (git-appraise-remove-review-notes root revision))
  (message "Removed local review %s" revision))

(defun git-appraise-remove-current-review ()
  "Remove the review shown in the current buffer."
  (interactive)
  (let ((pr (and (bound-and-true-p git-appraise-review-buffer-pr-id)
                 (gethash git-appraise-review-buffer-pr-id
                          git-appraise-db--objects))))
    (unless pr
      (user-error "No review is open"))
    (let ((default-directory (file-name-as-directory (oref pr owner))))
      (git-appraise-remove-review (oref pr number)))))

(provide 'git-appraise-remove)
;;; git-appraise-remove.el ends here
