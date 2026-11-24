;;; git-appraise-actions.el --- Actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski

;;; Commentary:
;;  Actions for git-appraise reviews.

;;; Code:

(require 'git-appraise-comment)
(require 'git-appraise-db)
(require 'git-appraise-diff)
(require 'git-appraise-utils)
(require 'let-alist)
(require 'dash)

(defcustom git-appraise-lgtm-message "LGTM! :thumbsup:"
  "Message to add to fast track LGTM git-appraise."
  :type 'string
  :group 'git-appraise)

(defvar git-appraise-comment-buffer-name)
(defvar git-appraise-commit-buffer-name)
(defvar git-appraise-buffer-name)
(defvar git-appraise-comment-cursor-pos)
(defvar git-appraise-section-full-refresh?)
(defvar git-appraise-comment-pending-submit-event nil)

;;;###autoload
(defun git-appraise-section-delete-comment ()
  "Delete the local draft comment at point."
  (interactive)
  (let* ((section (magit-current-section))
         (value (and section (oref section value)))
         (id (and (eieio-object-p value)
                  (slot-boundp value 'internalId)
                  (oref value internalId))))
    (unless (and (or (git-appraise-local-comment-section-p section)
                     (git-appraise-reply-comment-section-p section))
                 id)
      (user-error "No local draft comment at point"))
    (let ((git-appraise-db--pullreq-id git-appraise-review-buffer-pr-id)
          (buff-name (git-appraise-pr-buffer-name)))
      (git-appraise-db-delete-raw-comment id)
      (let ((deleted
             (when (buffer-live-p (get-buffer buff-name))
               (with-current-buffer buff-name
                 (let ((inhibit-read-only t)
                       (start (copy-marker (oref section start)))
                       (end (copy-marker (oref section end) t))
                       (parent (and (slot-boundp section 'parent)
                                    (oref section parent))))
                   (save-excursion
                     (goto-char start)
                     (when parent
                       (oset parent children (delq section (oref parent children))))
                     (delete-region start end)
                     (set-marker start nil)
                     (set-marker end nil)
                     t))))))
        (unless deleted
          (let ((git-appraise-section-full-refresh? nil))
            (git-appraise--build-buffer buff-name)))))
    (message "Draft comment deleted")))

;;;###autoload
(defun git-appraise-delete-feedback ()
  "Clear the unsent review feedback."
  (interactive)
  (let ((git-appraise-db--pullreq-id git-appraise-review-buffer-pr-id)
        (buff-name (git-appraise-pr-buffer-name)))
    (git-appraise-db--pullreq-feedback-update nil)
    (unless (git-appraise-section--update-feedback-in-place buff-name nil)
      (let ((git-appraise-section-full-refresh? nil))
        (git-appraise--build-buffer buff-name))))
  (message "Review feedback cleared"))

;;;###autoload
(defun git-appraise-visit-binary-file ()
  "Visit the binary file at point in the local worktree."
  (interactive)
  (let* ((section (magit-current-section))
         (raw-path (and section
                        (git-appraise-binary-file-section-p section)
                        (oref section value)))
         (root (git-appraise-utils--repo-dir))
         (path (and (stringp raw-path) root
                    (expand-file-name
                     (git-appraise-diff--strip-path raw-path) root))))
    (unless (and path (file-exists-p path))
      (user-error "Binary file is not available in the local worktree"))
    (find-file path)))

(defclass git-appraise-submit-local-comment ()
  ((path       :initarg :path)
   (position   :initarg :position)
   (side       :initarg :side)
   (line       :initarg :line)
   (start-side :initarg :start-side)
   (start-line :initarg :start-line)
   (body       :initarg :body)
   (internal-id :initarg :internal-id)
   (line-type  :initarg :line-type)))

(defclass git-appraise-submit-review ()
  ((state :initform nil)
   (pr :initform nil)
   (local-comments :initform nil
                   :type (satisfies
                          (lambda (it)
                            (-all-p #'git-appraise-submit-local-comment-p it))))
   (feedback :initform nil)))

(defclass git-appraise-submit-reply ()
  ((reply-to-id :initarg :reply-to-id)
   (body        :initarg :body)
   (internal-id :initarg :internal-id)))

(defclass git-appraise-submit-replies ()
  ((pr      :initform nil)
   (replies :initform nil
            :type (satisfies
                   (lambda (it)
                     (-all-p #'git-appraise-submit-reply-p it))))))

(defun git-appraise-submit--unique? (previous-obj current-id)
  "Verify if PREVIOUS-OBJ has CURRENT-ID."
  (if (not previous-obj)
      t
    (not (-contains-p (-map (lambda (it) (oref it internal-id))
                            previous-obj)
                      current-id))))

(defun git-appraise--submit-feedback-required? (review-obj)
  "Return non-nil when REVIEW-OBJ requires feedback before submit."
  (and (not (oref review-obj feedback))
       (not (string-equal (oref review-obj state) "APPROVE"))
       (not (and (git-appraise-backend-repo-p (oref review-obj pr))
                 (string-equal (oref review-obj state) "COMMENT")
                 (oref review-obj local-comments)))))

;;;###autoload
(defun git-appraise-set-feedback ()
  "Add review feedback locally.  Required to Comment and Request Change reviews."
  (interactive)
  (when (git-appraise-db-local-pr-p)
    (user-error "Local diff reviews are read-only"))
  (let ((buffer (get-buffer-create git-appraise-comment-buffer-name))
        (pr (git-appraise-db-get-pullreq)))
    (setq git-appraise-comment-feedback? t)
    (setq git-appraise-comment-cursor-pos (point))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (or (oref pr feedback) git-appraise-comment-feedback-msg))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (git-appraise-comment-mode))))

;;;###autoload
(defun git-appraise--submit (event &optional feedback only-reply?)
  "Submit your review with a final verdict EVENT.
If FEEDBACK is provided, use it.  If ONLY-REPLY? is non-nil, submit replies."
  (interactive)
  (setq git-appraise-comment-cursor-pos (point))
  (let* ((pr (git-appraise-db-get-pullreq))
         (buff-name (git-appraise-pr-buffer-name pr))
         (pr-id (oref pr id))
         (review-obj (git-appraise-submit-appraise-review))
         (replies-obj (git-appraise-submit-appraise-replies)))
    (oset review-obj state event)
    (oset review-obj pr pr)
    (oset replies-obj pr pr)
    (when feedback
      (oset review-obj feedback feedback))
    (let ((replies nil)
          (local-comments nil))
      (with-current-buffer (git-appraise-review-buffer)
        (save-excursion
          (goto-char (point-min))
          (magit-wash-sequence
           (lambda ()
             (let ((section (magit-current-section)))
               (with-slots (value) section
                 (when (and (git-appraise-feedback-section-p section)
                            (not feedback))
                   (oset review-obj feedback (oref value msg)))
                 (when (git-appraise-reply-comment-section-p section)
                   (when (git-appraise-submit--unique? replies (oref value internalId))
                     (push (git-appraise-submit-reply
                            :reply-to-id (oref value id)
                            :body (oref value msg)
                            :internal-id (oref value internalId))
                           replies)))
                 (when (git-appraise-local-comment-section-p section)
                   (when (git-appraise-submit--unique? local-comments (oref value internalId))
                     (push (git-appraise-submit-local-comment
                            :path (oref value path)
                            :position (oref value position)
                            :side (ignore-errors (oref value side))
                            :line (ignore-errors (oref value line))
                            :start-side (ignore-errors (oref value start-side))
                            :start-line (ignore-errors (oref value start-line))
                            :body (oref value msg)
                            :internal-id (oref value internalId)
                            :line-type (oref value line-type))
                           local-comments))))
               (forward-line)))))
      (oset replies-obj replies replies)
      (oset review-obj local-comments local-comments)
      (if (and (git-appraise--submit-feedback-required? review-obj)
               (not only-reply?))
          (progn
            (setq git-appraise-comment-pending-submit-event event)
            (message "Provide review feedback to submit this review.")
            (git-appraise-set-feedback))
        (progn
          (when (not only-reply?)
            (git-appraise-send-review
             review-obj
             (lambda (&rest _)
               (oset pr finished t)
               (oset pr finished-at (current-time-string))
               (oset pr feedback nil)
               (git-appraise-db-update pr)
               (when (fboundp 'git-appraise-pr-state-persist)
                 (git-appraise-pr-state-persist pr))
               (if (get-buffer buff-name)
                   (message "Done submitting review (%d comment(s) marked sent)"
                            (git-appraise-section--mark-local-comments-sent
                             buff-name))
                 (setq git-appraise-db--pullreq-id pr-id)
                 (git-appraise--build-buffer buff-name nil "Done submitting review")))))
          (when (oref replies-obj replies)
            (git-appraise-send-replies
             replies-obj
             (lambda (&rest _)
               (if (get-buffer buff-name)
                   (message "Done submitting review and replies (%d comment(s) marked sent)"
                            (git-appraise-section--mark-local-comments-sent
                             buff-name))
                 (setq git-appraise-db--pullreq-id pr-id)
                 (git-appraise--build-buffer buff-name nil "Done submitting review and replies")))))))))))

;;;###autoload
(defun git-appraise-submit-approve (&optional feedback)
  "Approve current PR.  Optionally set a FEEDBACK message."
  (interactive)
  (git-appraise--submit "APPROVE" feedback))

;;;###autoload
(defun git-appraise-submit-comments ()
  "Submit a Review Comment for the current PR."
  (interactive)
  (git-appraise--submit "COMMENT"))

;;;###autoload
(defun git-appraise-submit-request-changes ()
  "Submit a Request Change for the current PR."
  (interactive)
  (git-appraise--submit "REQUEST_CHANGES"))

;;;###autoload
(defun git-appraise-submit-lgtm ()
  "Submit an Approve Review with a LGTM message."
  (interactive)
  (git-appraise-submit-approve git-appraise-lgtm-message))

;;;###autoload
(defun git-appraise-submit-only-replies ()
  "Submit only replies comments."
  (interactive)
  (git-appraise--submit nil nil t))

;;;###autoload
(defun git-appraise-submit-single-top-level-comment ()
  "Submit a single comment without an attached Review."
  (interactive)
  (let ((git-appraise-comment-single-comment? t))
    (git-appraise-comment-add
     git-appraise-comment-single-comment-msg)))

;;;###autoload
(defun git-appraise-submit-single-diff-comment-at-point ()
  "Submit a single diff comment without an attached Review."
  (interactive)
  (let ((section (magit-current-section)))
    (with-slots (value) section
      (if (magit-hunk-section-p section)
          (progn
            (setq git-appraise-comment-send? t)
            (let ((git-appraise-comment-buffer-msg
                   git-appraise-comment-single-comment-msg))
              (git-appraise-comment-add-or-edit)))
        (error "Single diff comment at point only available in hunk sections")))))

;;;###autoload
(defun git-appraise-reload ()
  "Reload the buffer."
  (interactive)
  (let ((pr (git-appraise-db-get-pullreq))
        (git-appraise-section-full-refresh? t))
    (setq git-appraise-comment-cursor-pos (point))
    (if pr
        (git-appraise--build-buffer)
      (error "No PR found"))))

;;; PR description popup

(defconst git-appraise-pr-description-buffer-name
  "*Git Appraise: PR description*"
  "Popup buffer showing the description of the PR under review.")

(defcustom git-appraise-pr-description-display-alist
  '((display-buffer-in-side-window)
    (side . bottom)
    (window-height . 0.33))
  "Display buffer alist for the PR description popup."
  :group 'git-appraise
  :type 'alist)

(declare-function git-appraise--insert-html "git-appraise-section")

(defun git-appraise-pr-description-insert (pr)
  "Insert the title and description of PR into the current buffer."
  (let-alist (oref pr raw-infos)
    (let* ((html? (and .bodyHTML (not (string-empty-p .bodyHTML))))
           (body (cond (html? .bodyHTML)
                       ((and .bodyText (not (string-empty-p .bodyText)))
                        .bodyText)
                       (t "No description provided."))))
      (insert (propertize (oref pr title) 'face 'bold))
      (insert ?\n ?\n)
      (cond
       (html? (git-appraise--insert-html body))
       (t (insert body))))))

;;;###autoload
(defun git-appraise-popup-pr-description ()
  "Show the description of the PR under review in a popup buffer."
  (interactive)
  (let ((win (get-buffer-window git-appraise-pr-description-buffer-name)))
    (if win
        (quit-window nil win)
      (let ((pr (ignore-errors
                  (if-let ((buf (git-appraise-review-buffer)))
                      (with-current-buffer buf
                        (git-appraise--sync-db-pullreq)
                        (git-appraise-db-get-pullreq))
                    (git-appraise-db-get-pullreq)))))
        (unless pr (user-error "No PR under review"))
        (with-current-buffer
            (get-buffer-create git-appraise-pr-description-buffer-name)
          (let ((inhibit-read-only t))
            (special-mode)
            (erase-buffer)
            (git-appraise-pr-description-insert pr)
            (goto-char (point-min))))
        (display-buffer (get-buffer git-appraise-pr-description-buffer-name)
                        git-appraise-pr-description-display-alist)))))

;;; Hunk navigation

(defun git-appraise--section-visible-p (section)
  "Non-nil if SECTION's heading is visible through all its parents."
  (let ((s (oref section parent))
        (visible t))
    (while (and s visible)
      (when (oref s hidden) (setq visible nil))
      (setq s (oref s parent)))
    visible))

(defun git-appraise--navigation-sections ()
  "Visible file and hunk headings in buffer order."
  (let (items)
    (magit-map-sections
     (lambda (s)
       (when (and (memq (oref s type) '(file hunk))
                  (git-appraise--section-visible-p s))
         (push s items))))
    (sort items (lambda (a b) (< (oref a start) (oref b start))))))

(defun git-appraise-next-item ()
  "Move to the next visible file or hunk heading."
  (interactive)
  (if-let* ((target (--first (> (oref it start) (point))
                            (git-appraise--navigation-sections))))
      (goto-char (oref target start))
    (user-error "No next file or hunk")))

(defun git-appraise-previous-item ()
  "Move to the previous visible file or hunk heading."
  (interactive)
  (if-let* ((target (--first (< (oref it start) (point))
                            (reverse (git-appraise--navigation-sections)))))
      (goto-char (oref target start))
    (user-error "No previous file or hunk")))

(defun git-appraise--hunk-sections ()
  "All visible diff hunk sections of the buffer, in order."
  (let ((hunks nil))
    (magit-map-sections
     (lambda (s)
       (when (and (eq (eieio-object-class s) 'magit-hunk-section)
                  (git-appraise--section-visible-p s))
         (push s hunks))))
    (nreverse hunks)))

(defun git-appraise-next-hunk ()
  "Move to the next diff hunk, skipping comment sections."
  (interactive)
  (let ((target nil))
    (dolist (h (git-appraise--hunk-sections))
      (when (and (not target) (> (oref h start) (point)))
        (setq target h)))
    (if target
        (goto-char (oref target start))
      (user-error "No next hunk"))))

(defun git-appraise-previous-hunk ()
  "Move to the previous diff hunk, skipping comment sections."
  (interactive)
  (let ((target nil))
    (dolist (h (reverse (git-appraise--hunk-sections)))
      (when (and (not target)
                 (< (oref h start) (point))
                 (not (and (>= (point) (oref h start))
                           (<= (point) (oref h end)))))
        (setq target h)))
    (if target
        (goto-char (oref target start))
      (user-error "No previous hunk"))))

(provide 'git-appraise-actions)
;;; git-appraise-actions.el ends here
