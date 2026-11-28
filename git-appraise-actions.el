;;; git-appraise-actions.el --- Actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski

;;; Commentary:
;;  Actions for git-appraise reviews.

;;; Code:

(require 'git-appraise-comment)
(require 'git-appraise-backend)
(require 'git-appraise-db)
(require 'git-appraise-diff)
(require 'git-appraise-utils)
(require 'let-alist)
(require 'dash)

(declare-function git-appraise-backend-abandon "git-appraise-backend")
(declare-function git-appraise-backend-rebase "git-appraise-backend")
(declare-function git-appraise-backend-land-review "git-appraise-backend")
(declare-function git-appraise-backend--root "git-appraise-backend")
(declare-function git-appraise-backend-send-reply "git-appraise-backend")
(declare-function git-appraise-comment--find-section-by-internal-id
                  "git-appraise-comment-draft")
(declare-function git-appraise-comment--refresh-file-count
                  "git-appraise-comment-draft" (path buff-name))
(declare-function git-appraise-comment--refresh-conversation-in-place
                  "git-appraise-comment-draft" (buff-name))
(declare-function git-appraise-section--update-review-status-in-place
                  "git-appraise-section-header" (buffer pr))
(declare-function git-appraise-lazy-placeholder-section-p "git-appraise-lazy"
                  (section))
(declare-function git-appraise--sync-db-pullreq "git-appraise-section")

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
      (when (and (slot-boundp value 'path) (oref value path))
        (git-appraise-comment--refresh-file-count
         (oref value path) buff-name))
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
(defun git-appraise-toggle-draft-fyi ()
  "Toggle the FYI exception for the local draft at point.
Needs-work submissions mark other staged findings actionable."
  (interactive)
  (let* ((section (magit-current-section))
         (value (and section (oref section value)))
         (id (and (git-appraise-local-comment-section-p section)
                  (eieio-object-p value) (oref value internalId)))
         (pr (git-appraise-db-get-pullreq)))
    (unless id
      (user-error "No local draft comment at point"))
    (let* ((entry (--first
                   (--any? (equal (a-get it 'internal-id) id)
                           (a-get-in it '(comments nodes)))
                   (oref pr raw-comments)))
           (copy (and entry (copy-tree entry)))
           (node (and copy (car (a-get-in copy '(comments nodes))))))
      (unless node
        (user-error "Draft is no longer in local state"))
      (let ((fyi (not (a-get node 'fyi))))
        (setq copy (a-assoc-in copy '(comments nodes)
                               (list (a-assoc node 'fyi fyi))))
        (git-appraise-db--upsert-local-comment copy id)
        (oset value fyi? fyi))
      (when-let* ((outer
                   (git-appraise-comment--find-section-by-internal-id
                    (git-appraise-pr-buffer-name pr) id)))
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (oref outer start))
            (let ((end (line-end-position)))
              (when (search-forward " · DRAFT" end t)
                (if (oref value fyi?)
                    (insert (propertize " · FYI"
                                        'face 'git-appraise-state-face))
                  (when (search-forward " · FYI" (line-end-position) t)
                    (delete-region (- (point) (length " · FYI"))
                                   (point)))))))))
      (message "Draft is %s" (if (oref value fyi?) "FYI" "actionable")))))

(defun git-appraise-actions--section-comment-id (section)
  "Return the published comment ID represented by SECTION."
  (let ((value (and section (oref section value))))
    (cond
     ((and (eieio-object-p value)
           (slot-exists-p value 'id)
           (slot-boundp value 'id))
      (oref value id))
     ((and section
           (eq (oref section type) 'git-appraise-historical-comment-section)
           (listp value))
      (a-get value 'databaseId)))))

;;;###autoload
(defun git-appraise-resolve-comment-at-point (message)
  "Reply to the published comment at point and mark it addressed.
MESSAGE explains the resolution; this writes one native `-lgtm' reply."
  (interactive (list (read-string "Resolution reply: ")))
  (let* ((section (magit-current-section))
         (id (git-appraise-actions--section-comment-id section))
         (pr (git-appraise-db-get-pullreq)))
    (unless (and (git-appraise-backend-repo-p pr)
                 (stringp id) (not (string-empty-p id)))
      (user-error "No published git-appraise comment at point"))
    (when (string-empty-p (string-trim message))
      (user-error "A resolution reply is required"))
    (git-appraise-backend-send-reply pr id message t)
    (let ((buff-name (git-appraise-pr-buffer-name pr)))
      (git-appraise--submit-refresh pr buff-name nil)
      (when-let* ((updated
                   (git-appraise-section--find-section
                    (get-buffer buff-name)
                    (lambda (section)
                      (equal (git-appraise-actions--section-comment-id
                              section) id)))))
        (with-current-buffer buff-name
          (goto-char (oref updated start)))))
    (message "Resolution reply saved locally; push notes to share it")))

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
   (line-type  :initarg :line-type)
   (fyi        :initarg :fyi :initform nil)))

(defclass git-appraise-submit-reply ()
  ((reply-to-id :initarg :reply-to-id)
   (body        :initarg :body)
   (internal-id :initarg :internal-id)
   (resolve     :initarg :resolve :initform nil)))

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
                 (member (oref review-obj state) '("COMMENT" "NEEDS_WORK"))
                 (if (equal (oref review-obj state) "NEEDS_WORK")
                     (--any? (not (oref it fyi))
                             (oref review-obj local-comments))
                   (oref review-obj local-comments))))))

(defun git-appraise--submit-collect (buff-name review-obj replies-obj feedback)
  "Collect staged comments and replies in BUFF-NAME for publication."
  (let (replies local-comments)
    (with-current-buffer buff-name
      (magit-map-sections
       (lambda (section)
         (let ((value (oref section value)))
           (cond
            ((and (git-appraise-feedback-section-p section)
                  (not feedback))
             (oset review-obj feedback (oref value msg)))
            ((git-appraise-reply-comment-section-p section)
             (when (git-appraise-submit--unique? replies
                                                (oref value internalId))
               (push (git-appraise-submit-reply
                      :reply-to-id (oref value id)
                      :body (oref value msg)
                      :internal-id (oref value internalId)
                      :resolve (and (slot-boundp value 'resolve?)
                                    (oref value resolve?)))
                     replies)))
            ((git-appraise-local-comment-section-p section)
             (when (git-appraise-submit--unique? local-comments
                                                (oref value internalId))
               (push (git-appraise-submit-local-comment
                      :path (oref value path)
                      :position (oref value position)
                      :side (ignore-errors (oref value side))
                      :line (ignore-errors (oref value line))
                      :start-side (ignore-errors (oref value start-side))
                      :start-line (ignore-errors (oref value start-line))
                      :body (oref value msg)
                      :internal-id (oref value internalId)
                      :line-type (oref value line-type)
                      :fyi (and (slot-boundp value 'fyi?)
                                (oref value fyi?)))
                     local-comments))))))
       magit-root-section))
    (oset replies-obj replies replies)
    (oset review-obj local-comments local-comments)))

(defun git-appraise--submit-refresh (pr buff-name had-local)
  "Refresh published state in BUFF-NAME without washing PR's diff.
HAD-LOCAL means staged comment headings also need to become SENT."
  (let* ((git-appraise-db--pullreq-id (oref pr id))
         (buffer (get-buffer buff-name))
         (cursor (and buffer (with-current-buffer buffer
                               (copy-marker (point)))))
         (anchor-id (and buffer
                         (with-current-buffer buffer
                           (git-appraise-actions--section-comment-id
                            (magit-current-section)))))
         (anchor-offset
          (and anchor-id buffer
               (with-current-buffer buffer
                 (- (point) (oref (magit-current-section) start)))))
         (windows (and buffer
                       (--map (cons it (copy-marker (window-start it)))
                              (get-buffer-window-list buffer nil t))))
         (partial nil))
    (unwind-protect
        (pcase (git-appraise-backend-refresh-comments pr)
          ('ok
           (condition-case err
               (with-current-buffer buffer
                 (setq git-appraise-section-grouped-comments
                       (git-appraise-utils-make-group
                        (git-appraise-db--pullreq-raw-comments)))
                 (unless (and
                          (git-appraise-comment--refresh-conversation-in-place
                           buff-name)
                          (git-appraise-section--update-review-status-in-place
                           buffer pr)
                          (git-appraise-section--update-feedback-in-place
                           buff-name nil))
                   (error "Could not update review sections in place"))
                 (when had-local
                   (git-appraise-section--mark-local-comments-sent buff-name))
                 (setq partial t))
             (error
              (git-appraise-utils--log
               "submit:in-place" (error-message-string err))
              (let ((git-appraise-section-full-refresh? nil))
                (git-appraise--build-buffer buff-name)))))
          ('head-changed
           (let ((git-appraise-section-full-refresh? t))
             (git-appraise--build-buffer buff-name)))
          (_
           (let ((git-appraise-section-full-refresh? nil))
             (git-appraise--build-buffer buff-name))))
      (when (and partial (buffer-live-p buffer) cursor)
        (with-current-buffer buffer
          (let ((anchor
                 (and anchor-id
                      (git-appraise-section--find-section
                       buffer
                       (lambda (section)
                         (equal (git-appraise-actions--section-comment-id
                                 section) anchor-id))))))
            (if anchor
                (goto-char (min (1- (oref anchor end))
                                (+ (oref anchor start) anchor-offset)))
              (goto-char cursor))))
        (dolist (entry windows)
          (when (and (window-live-p (car entry))
                     (eq (window-buffer (car entry)) buffer))
            (set-window-start (car entry) (cdr entry)))))
      (when cursor (set-marker cursor nil))
      (dolist (entry windows)
        (set-marker (cdr entry) nil)))))

;;;###autoload
(defun git-appraise-set-feedback ()
  "Add review feedback locally.  Required to Comment and Request Change reviews."
  (interactive)
  (when (git-appraise-db-local-pr-p)
    (user-error "Local diff reviews are read-only"))
  (let* ((review-buffer (git-appraise-review-buffer))
         (buffer (git-appraise-comment--composer-buffer review-buffer))
         (pr (git-appraise-db-get-pullreq)))
    (setq git-appraise-comment-feedback? t)
    (setq git-appraise-comment-cursor-pos (point))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (or (oref pr feedback) git-appraise-comment-feedback-msg))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (git-appraise-comment-mode)
      (setq git-appraise-comment-review-buffer review-buffer)
      (git-appraise-comment--capture-session))))

;;;###autoload
(defun git-appraise--submit (event &optional feedback only-reply?)
  "Submit your review with a final verdict EVENT.
If FEEDBACK is provided, use it.  If ONLY-REPLY? is non-nil, submit replies."
  (interactive)
  (let* ((pr (git-appraise-db-get-pullreq))
         (buff-name (git-appraise-pr-buffer-name pr))
         (pr-id (oref pr id))
         (review-obj (git-appraise-native-publication))
         (replies-obj (git-appraise-native-replies)))
    (setq git-appraise-comment-cursor-pos
          (with-current-buffer buff-name (point)))
    (oset review-obj state event)
    (oset review-obj pr pr)
    (oset replies-obj pr pr)
    (when feedback
      (oset review-obj feedback feedback))
    (git-appraise--submit-collect buff-name review-obj replies-obj feedback)
    (if (and (git-appraise--submit-feedback-required? review-obj)
             (not only-reply?))
          (progn
            (setq git-appraise-comment-pending-submit-event event)
            (message "Provide review feedback to submit this review.")
            (git-appraise-set-feedback))
        (condition-case err
            (progn
              ;; Every successful reply is removed from the draft store by
              ;; the backend.  Send the verdict last so retry after a
              ;; partial failure cannot append the same decision twice.
              (when (oref replies-obj replies)
                (git-appraise-backend-publish-replies replies-obj #'ignore))
              (unless only-reply?
                (git-appraise-backend-publish-review review-obj #'ignore)
                (oset pr feedback nil)
                (git-appraise-db-update pr)
                (git-appraise-pr-state-persist pr))
              (setq git-appraise-db--pullreq-id pr-id)
              (git-appraise--submit-refresh
               pr buff-name
               (or (oref review-obj local-comments)
                   (oref replies-obj replies)))
              (message "Review events saved locally; push notes to share"))
          (error
           (setq git-appraise-db--pullreq-id pr-id)
           (let ((git-appraise-section-full-refresh? nil))
             (ignore-errors (git-appraise--build-buffer buff-name)))
           (signal (car err) (cdr err)))))))

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
(defun git-appraise-submit-needs-work ()
  "Publish staged findings as needs-work comments."
  (interactive)
  (git-appraise--submit "NEEDS_WORK"))

(defalias 'git-appraise-submit-request-changes
  #'git-appraise-submit-needs-work)

;;;###autoload
(defun git-appraise-submit-reject ()
  "Explicitly reject the current git-appraise review."
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
(defun git-appraise-abandon-current-review ()
  "Abandon the current git-appraise review."
  (interactive)
  (let* ((pr (git-appraise-db-get-pullreq))
         (root (or (and pr (slot-boundp pr 'owner) (oref pr owner))
                   (git-appraise-backend--root)))
         (revision (and pr (slot-boundp pr 'number) (oref pr number)))
         (message (read-string "Abandon message (optional): ")))
    (unless (and root revision)
      (user-error "No current review to abandon"))
    (git-appraise-backend-abandon root revision message)
    (when (get-buffer (git-appraise-pr-buffer-name))
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise-reload)))
    (message "Review abandoned")))

;;;###autoload
(defun git-appraise-rebase-current-review ()
  "Rebase the current git-appraise review onto its target."
  (interactive)
  (let* ((pr (git-appraise-db-get-pullreq))
         (root (or (and pr (slot-boundp pr 'owner) (oref pr owner))
                   (git-appraise-backend--root)))
         (revision (and pr (slot-boundp pr 'number) (oref pr number))))
    (unless (and root revision)
      (user-error "No current review to rebase"))
    (unless (yes-or-no-p (format "Rebase current review %s onto its target? " revision))
      (user-error "Rebase cancelled"))
    (message "Rebasing review %s..." revision)
    (git-appraise-backend-rebase root revision)
    (when (get-buffer (git-appraise-pr-buffer-name))
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise-reload)))
    (message "Review rebased")))

;;;###autoload
(defun git-appraise-land-current-review (strategy)
  "Land the current git-appraise review using STRATEGY."
  (interactive
   (let ((strategy (completing-read "Submit strategy: "
                                    '("default" "merge" "rebase" "fast-forward")
                                    nil t nil nil "default")))
     (list (unless (equal strategy "default") strategy))))
  (let* ((pr (git-appraise-db-get-pullreq))
         (root (or (and pr (slot-boundp pr 'owner) (oref pr owner))
                   (git-appraise-backend--root)))
         (revision (and pr (slot-boundp pr 'number) (oref pr number))))
    (unless (and root revision)
      (user-error "No current review to land"))
    (unless (yes-or-no-p (format "Land current review %s%s? "
                                 revision
                                 (if strategy (format " (%s)" strategy) "")))
      (user-error "Land cancelled"))
    (message "Landing review %s..." revision)
    (git-appraise-backend-land-review root revision strategy)
    (when (get-buffer (git-appraise-pr-buffer-name))
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise-reload)))
    (message "Review landed")))

;;;###autoload
(defun git-appraise-reload ()
  "Reload the buffer."
  (interactive)
  (when (derived-mode-p 'git-appraise-mode)
    (git-appraise--sync-db-pullreq))
  (let ((pr (git-appraise-db-get-pullreq))
        (git-appraise-section-full-refresh? t)
        ;; the anchor is this reload's intent only: leaving it set
        ;; would steer every later render to a stale position
        (git-appraise-comment-cursor-pos (point)))
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
       (when (and (or (memq (oref s type) '(file hunk))
                      (git-appraise-lazy-placeholder-section-p s))
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

;;;###autoload
(defun git-appraise-next-unviewed-file ()
  "Move to the next file that is not fully viewed."
  (interactive)
  (let (target)
    (magit-map-sections
     (lambda (section)
       (when (and (not target)
                  (or (magit-file-section-p section)
                      (git-appraise-lazy-placeholder-section-p section))
                  (markerp (oref section start))
                  (eq (marker-buffer (oref section start)) (current-buffer))
                  (> (oref section start) (point))
                  (let* ((path (git-appraise-folding--file-path section))
                         (entry (and path (hash-table-p git-appraise-section--viewed)
                                     (gethash path git-appraise-section--viewed))))
                    (not (plist-get entry :all-hunks))))
         (setq target section))))
    (if target
        (goto-char (oref target start))
      (user-error "No next unviewed file"))))

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
