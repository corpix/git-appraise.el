;;; git-appraise-comment.el --- Manage comments -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski
;;
;; Version: 0.0.7
;; Homepage: https://github.com/corpix/git-appraise.el
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  This library implements support functions to manage git-appraise comments.
;;
;;; Code:

(require 'git-appraise-section)
(require 'git-appraise-utils)
(require 'markdown-mode)
(defvar git-appraise-repo-worktree)             ; git-appraise-repo.el

(defcustom git-appraise-comment-buffer-name "*Git Appraise Comment*"
  "Name of comment buffer."
  :group 'git-appraise
  :type 'string)

(defcustom git-appraise-comment-buffer-msg ";;; Type C-c C-c to include your comment locally\n;;; After that check the transient menu (press r) to see possible actions"
  "Helper text on top of comment buffer."
  :group 'git-appraise
  :type 'string)

(defcustom git-appraise-comment-feedback-msg ";;; Leave a comment here."
  "Default text to feedback slot."
  :group 'git-appraise
  :type 'string)

(defcustom git-appraise-comment-single-comment-msg ";;; Equivalent to add a simple comment to the PR without a review."
  "Default text to single comment section e.g. conversation."
  :group 'git-appraise
  :type 'string)

(defcustom git-appraise-comment-edit-remote-msg ";;; Editing a submitted comment\n;;; C-c C-c to send the edit, C-c C-k to abort"
  "Helper text on top of the comment buffer when editing a
comment that was already submitted."
  :group 'git-appraise
  :type 'string)

;;; internal vars

(defvar git-appraise-comment-cursor-pos nil
  "Variable to hold the cursor position where the comment will be added.
For internal usage only.")

(defvar git-appraise-comment-feedback? nil
  "Differentiate between a regular comment from the main feedback comment.
For internal usage only.")

(defvar git-appraise-comment-title? nil
  "Are you writing a title?.
For internal usage only.")

(defvar git-appraise-comment-description? nil
  "Are you writing a description?.
For internal usage only.")

(defvar git-appraise-comment-single-comment? nil
  "Include a single new comment to the PR without a Review.
For internal usage only.")

(defvar git-appraise-comment-uncommitted nil
  "Hold uncommitted comment.
For internal usage only.")

(defvar git-appraise-comment-commit-buffer? nil
  "Toggle if we are at the commit review buffer.
For internal usage only.")

(defvar git-appraise-comment-send? nil
  "Toggle if we should send the local comment immediately.
For internal usage only.")

(defvar git-appraise-comment-pending-submit-event nil
  "Review event to submit after feedback is committed.
For internal usage only.")

(declare-function git-appraise--submit "git-appraise-actions")

;; remove free variable warnings
(defvar git-appraise-comment-buffer-name)
(defvar git-appraise-commit-buffer-name)
(defvar git-appraise-buffer-name)

;;; general functons

(defun git-appraise-comment-reset-global-vars ()
  "Reset all stateful vars."
  (setq git-appraise-comment-cursor-pos nil
        git-appraise-comment-feedback? nil
        git-appraise-comment-title? nil
        git-appraise-comment-description? nil
        git-appraise-comment-uncommitted nil
        git-appraise-comment-commit-buffer? nil
        git-appraise-comment-single-comment? nil
        git-appraise-comment-send? nil
        git-appraise-comment-pending-submit-event nil))

(defun git-appraise-comment-strip-known-helpers (msg)
  "Remove known git-appraise comment helper text from MSG."
  (let ((msg (or msg ""))
        (helpers (delq nil
                       (list (and (boundp 'git-appraise-comment-buffer-msg)
                                  git-appraise-comment-buffer-msg)
                             (and (boundp 'git-appraise-comment-feedback-msg)
                                  git-appraise-comment-feedback-msg)
                             (and (boundp 'git-appraise-comment-single-comment-msg)
                                  git-appraise-comment-single-comment-msg)
                             (and (boundp 'git-appraise-comment-edit-remote-msg)
                                  git-appraise-comment-edit-remote-msg)))))
    (dolist (helper helpers)
      (setq msg (git-appraise-utils--comment-clean-msg msg helper)))
    msg))

(defun git-appraise--compute-line-and-side ()
  "Compute the review comment LINE and SIDE at or near point.
Returns a cons (SIDE . LINE), where SIDE is \"RIGHT\" (new) or \"LEFT\" (old).
Skips review UI lines when counting and anchors to nearest patch line."
  (when-let* ((hunk (magit-current-section))
              (_ (magit-hunk-section-p hunk)))
    (let* ((startc (git-appraise--hunk-content-start hunk))
           (anchor (or (and (git-appraise--patch-line-p)
                            (line-beginning-position))
                       (git-appraise--nearest-patch-line-in-hunk hunk)
                       startc))
           (adv (git-appraise--count-patch-advances hunk startc anchor))
           (old-start (car (oref hunk from-range)))
           (new-start (car (oref hunk to-range))))
      (save-excursion
        (goto-char anchor)
        (let ((ch (char-after (line-beginning-position))))
          (if (eq ch ?-)
              (cons "LEFT" (+ old-start (car adv)))
            (cons "RIGHT" (+ new-start (cdr adv)))))))))

;; Return location as plist supporting optional region
(defun git-appraise--compute-line-side-and-range ()
  "Compute review location at point with optional region.
Returns plist with :side :line and optionally :start-side :start-line."
  (let* ((curr (git-appraise--compute-line-and-side))
         (side (car curr))
         (line (cdr curr)))
    (if (and (use-region-p)
             (not (= (region-beginning) (region-end))))
        (save-excursion
          (let* ((rb (region-beginning))
                 (re (region-end))
                 (beg (progn (goto-char rb) (git-appraise--compute-line-and-side)))
                 (end (progn (goto-char re) (git-appraise--compute-line-and-side)))
                 (start (if (<= rb re) beg end))
                 (finish (if (<= rb re) end beg)))
            (list :side (car finish)
                  :line (cdr finish)
                  :start-side (car start)
                  :start-line (cdr start)))
          )
      (list :side side :line line))))

;;; Comment C_UD

(defun git-appraise-comment-add (&optional msg)
  "Add comment.
Optionally define a MSG."
  (let ((buffer (get-buffer-create git-appraise-comment-buffer-name))
        (review-buffer (git-appraise-review-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (if msg msg git-appraise-comment-buffer-msg))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (git-appraise-comment-mode)
      (setq git-appraise-comment-review-buffer review-buffer))))

(defun git-appraise-comment-edit ()
  "Edit comment."
  (with-slots (value) (magit-current-section)
    (let ((buffer (get-buffer-create git-appraise-comment-buffer-name))
          (review-buffer (git-appraise-review-buffer)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (oref value msg))
        (insert ?\n)
        (goto-char (point-min)))
      (switch-to-buffer-other-window buffer)
      (git-appraise-comment-mode)
      (setq git-appraise-comment-review-buffer review-buffer))))

;;; handlers ADD or EDIT

(cl-defmethod git-appraise-comment-handler-add-or-edit ((obj git-appraise-code-comment-section))
  "Reply to comment OBJ."
  (let ((reply-comment (git-appraise-reply-comment-section
                        :state (oref obj state)
                        :author (oref obj author)
                        :path (oref obj path)
                        :position (oref obj position)
                        :diffHunk (oref obj diffHunk)
                        :id (oref obj id))))
    (setq git-appraise-comment-uncommitted reply-comment)
    (git-appraise-comment-add)))

(cl-defmethod git-appraise-comment-handler-add-or-edit ((obj git-appraise-comment-section))
  "Reply to an appraise conversation comment OBJ."
  (if (git-appraise-backend-repo-p (git-appraise-db-get-pullreq))
      (let ((reply (git-appraise-reply-comment-section
                    :state "COMMENTED"
                    :author (oref obj author)
                    :path ""
                    :position 0
                    :diffHunk nil
                    :id (oref obj id))))
        (setq git-appraise-comment-uncommitted reply)
        (git-appraise-comment-add))
    (cl-call-next-method)))

(cl-defmethod git-appraise-comment-handler-add-or-edit ((obj git-appraise-local-comment-section))
  "Edit local comment in OBJ."
  (oset obj edit? t)
  (setq git-appraise-comment-uncommitted obj)
  (git-appraise-comment-edit))

(cl-defmethod git-appraise-comment-handler-add-or-edit ((obj git-appraise-title-section))
  "Change title in OBJ."
  (setq git-appraise-comment-uncommitted obj
        git-appraise-comment-title? t)
  (git-appraise-comment-add))

;;; Editing comments that were already submitted

(defclass git-appraise-editable-remote-comment ()
  ((kind :initarg :kind
         :documentation "Which endpoint the edit goes to:
\"issue-comment\", \"review-summary\" or \"review-comment\"
\(see `git-appraise-update-comment').")
   (id   :initarg :id
         :documentation "Backend databaseId of the comment/review.")
   (body :initarg :body
         :documentation "Original raw (markdown) body, for reference.")
   (msg  :initform nil
         :documentation "Holds the NEW body once the comment
buffer is committed (the commit machinery osets this slot).")))

(defun git-appraise-comment--raw-body (obj)
  "Return the raw (markdown) body stored on comment OBJ, or nil.
Guards both the slot not existing (objects rendered before a
reload by the long-lived daemon) and it being unbound."
  (and (slot-exists-p obj 'body)
       (slot-boundp obj 'body)
       (oref obj body)))

(defun git-appraise-comment--edit-target (value)
  "Return the edit KIND for the comment data object VALUE, or nil.
`local' for local (pending) comments, one of \"issue-comment\",
\"review-summary\" or \"review-comment\" for submitted ones, nil
when VALUE is not an editable comment.  Exact-class predicates
decide (eieio `-p' predicates are NOT subclass-aware on every
Emacs this package supports), and the `local?' SLOT is never
consulted: `git-appraise-outdated-comment-section' sets it to t
even for comments fetched from the backend."
  (cond
   ((git-appraise-comment-section-p value)
    (pcase (and (slot-boundp value 'typename) (oref value typename))
      ("IssueComment" "issue-comment")
      ("PullRequestReview" "review-summary")
      (_ nil)))
   ;; local (pending) comments and replies keep their RET flow
   ((or (git-appraise-local-comment-section-p value)
        (git-appraise-reply-comment-section-p value))
    'local)
   ;; diff-anchored comments fetched from the backend
   ((or (git-appraise-code-comment-section-p value)
        (git-appraise-outdated-comment-section-p value))
    "review-comment")
   (t
    nil)))

(defun git-appraise-comment--start-remote-edit (value kind)
  "Open the comment buffer to edit the submitted comment VALUE.
KIND selects the provider endpoint (see
`git-appraise-update-comment')."
  (let* ((pr (git-appraise-db-get-pullreq))
         (id (and (slot-exists-p value 'id)
                  (slot-boundp value 'id)
                  (oref value id)))
         (body (git-appraise-comment--raw-body value))
         (author (and (slot-boundp value 'author) (oref value author)))
         (user (git-appraise-utils--git-get-user)))
    (when (git-appraise-backend-repo-p pr)
      (user-error "Editing submitted comments is not supported in git-appraise"))
    (unless id
      (user-error "Comment has no backend id; nothing to edit"))
    (unless body
      (user-error "Raw comment body unavailable: press G to fully reload the review, then try again"))
    (when (and author user
               (not (string-equal (downcase author) (downcase user)))
               (not (y-or-n-p (format "This comment was authored by @%s.  Edit it anyway? "
                                      author))))
      (user-error "Edit aborted"))
    (setq git-appraise-comment-cursor-pos (point)
          git-appraise-comment-uncommitted
          (git-appraise-editable-remote-comment
           :kind kind :id id :body body))
    (git-appraise-comment-add
     (format "%s\n\n%s" git-appraise-comment-edit-remote-msg body))))

;;;###autoload
(defun git-appraise-edit-remote-comment-at-point ()
  "Edit a comment that was already submitted.
Works on diff comments (including outdated ones) and on
Conversation comments; a \"PullRequestReview\" conversation
entry edits the submitted review summary.  The edit is sent when
you press \\<git-appraise-comment-mode-map>\\[git-appraise-comment-commit] in the comment buffer, and the
review re-fetches its data afterward.  The backend rejects edits to
other people's comments."
  (interactive)
  (when (git-appraise-db-local-pr-p)
    (user-error "Local diff reviews are read-only"))
  (let* ((section (magit-current-section))
         (value (and section
                     (slot-boundp section 'value)
                     (oref section value))))
    (if-let ((kind (and value (git-appraise-comment--edit-target value))))
        (if (eq kind 'local)
            ;; pending local comments/replies keep their RET flow
            (git-appraise-comment-add-or-edit)
          (git-appraise-comment--start-remote-edit value kind))
      (user-error "No editable comment at point"))))

(cl-defmethod git-appraise-comment-handler-add-or-edit (obj)
  "Add a comment in the OBJ."
  ;;; only hunks allowed here
  (with-slots (type) (magit-current-section)
    (if (and (equal type 'file)
             (git-appraise-backend-repo-p (git-appraise-db-get-pullreq)))
        (let ((comment (git-appraise-local-comment-section
                        :state "LOCAL COMMENT"
                        :author (git-appraise-utils--git-get-user)
                        :path (string-remove-prefix "b/" (substring-no-properties obj))
                        :position 0
                        :line-type "FILE"
                        :send? t)))
          (setq git-appraise-comment-uncommitted comment)
          (git-appraise-comment-add))
      (if (not (equal type 'hunk))
          (message "You can't add text over unspecified region.")
        (let* ((current-line (line-number-at-pos))
             (line (save-excursion
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
             (line-type (cond
                         ((string-prefix-p "-" line)
                          "REMOVED")
                         ((string-prefix-p "+" line)
                          "ADDED")
                         (t
                          "UNCHANGED")))
             (amount-loc nil))
        (save-excursion
          (while (and (not (looking-at
                            "Comment by\\|Reviewed by\\|Reply by\\|modified\\|new file\\|deleted"))
                      (not (equal (point) (point-min))))
            (forward-line -1))
          (let ((section (magit-current-section)))
            (if (not section)
                (setq amount-loc 0)
              (with-slots (type value) section
                (if (equal type 'file)
                    (setq amount-loc 0)
                  (setq amount-loc (or (oref value amount-loc) 0)))))))

        ;; Compute line/side and optional range (region)
        (let* ((loc (git-appraise--compute-line-side-and-range))
               (side (plist-get loc :side))
               (line (plist-get loc :line))
               (start-side (plist-get loc :start-side))
               (start-line (plist-get loc :start-line))
               (local-comment (git-appraise-local-comment-section
                               :state "LOCAL COMMENT"
                               :author (git-appraise-utils--git-get-user)
                               :path (a-get obj 'path)
                               :side side
                               :line line
                               :start-side start-side
                               :start-line start-line
                               :line-type line-type
                               :send? (or git-appraise-comment-send?
                                          (git-appraise-backend-repo-p
                                           (git-appraise-db-get-pullreq))))))
          (setq git-appraise-comment-uncommitted local-comment)
          (git-appraise-comment-add)))))))


;;;###autoload
(defun git-appraise-comment-add-or-edit ()
  "Add or edit comment depending on context."
  (interactive)
  (let ((section (magit-current-section)))
    (with-current-buffer (git-appraise-review-buffer)
      (setq git-appraise-comment-cursor-pos (point))
      (with-slots (value) section
        (git-appraise-comment-handler-add-or-edit value)))))

;;; handlers COMMIT

(require 'git-appraise-comment-draft)

(cl-defmethod git-appraise-comment-handler-commit ((obj git-appraise-editable-remote-comment) _default-buff-msg)
  "Send the edit of an already-submitted comment OBJ to the backend.
The new body is the `msg' slot the commit machinery sets from the
comment buffer; the helper header is stripped first."
  (let* ((pr (git-appraise-db-get-pullreq))
         (pr-id (oref pr id))
         (buff-name (git-appraise-pr-buffer-name pr))
         (kind (oref obj kind))
         (comment-id (oref obj id))
         (new-body (git-appraise-utils--comment-clean-msg
                    (oref obj msg)
                    git-appraise-comment-edit-remote-msg)))
    (if (string-empty-p new-body)
        (user-error "Comment is empty")
      (git-appraise-update-comment
       pr kind comment-id new-body
       (lambda (&rest _)
         (setq git-appraise-db--pullreq-id pr-id)
         (let* ((buf (get-buffer buff-name))
                (cursor-pos (or (and buf (window-live-p (get-buffer-window buf))
                                     (window-point (get-buffer-window buf)))
                                (and buf (with-current-buffer buf (point))))))
           (pcase (git-appraise-backend-refresh-comments (git-appraise-db-get-pullreq))
             ('ok
              (let ((patched (and (buffer-live-p buf)
                                  (git-appraise-comment--update-remote-comment-in-place
                                   buff-name comment-id new-body))))
                (unless patched
                  (let ((git-appraise-section-full-refresh? nil)
                        (git-appraise-comment-cursor-pos cursor-pos))
                    (git-appraise--build-buffer buff-name))))
              (message "Comment edited"))
             ('head-changed
              (let ((git-appraise-section-full-refresh? t)
                    (git-appraise-comment-cursor-pos cursor-pos))
                (git-appraise--build-buffer buff-name))
              (message "Comment edited"))
             (_
              (let ((git-appraise-section-full-refresh? nil)
                    (git-appraise-comment-cursor-pos cursor-pos))
                (when (buffer-live-p buf)
                  (git-appraise--build-buffer buff-name)))
              (message "Comment edited; refresh failed")))))))))

(defun git-appraise-comment--refresh (pr)
  "Refresh PR without depending on the current review's global context."
  (let ((git-appraise-db--pullreq-id (oref pr id)))
    (git-appraise--build-buffer (git-appraise-pr-buffer-name pr))))

(defun git-appraise-comment--commit-text (pr comment-text)
  "Dispatch COMMENT-TEXT for PR while the composer remains available."
  (cond
   (git-appraise-comment-description?
    (oset pr raw-infos (-> (oref pr raw-infos)
                          (a-assoc 'bodyText comment-text)
                          (a-assoc 'bodyHTML nil)))
    (git-appraise-send-description
     pr
     (lambda ()
       (git-appraise-db-update pr)
       (git-appraise-comment--refresh pr)
       (git-appraise-comment-reset-global-vars))))
   (git-appraise-comment-title?
    (oset pr title comment-text)
    (git-appraise-send-title
     pr
     (lambda ()
       (git-appraise-db-update pr)
       (git-appraise-comment--refresh pr)
       (git-appraise-comment-reset-global-vars))))
   (git-appraise-comment-feedback?
    (let* ((msg (git-appraise-utils--comment-clean-msg
                 comment-text git-appraise-comment-feedback-msg))
           (pending-event git-appraise-comment-pending-submit-event)
           (buff-name (git-appraise-pr-buffer-name pr)))
      (when (string-empty-p msg)
        (user-error "Feedback is empty"))
      (git-appraise-db--pullreq-feedback-update msg)
      (unless (git-appraise-section--update-feedback-in-place buff-name msg)
        (let ((git-appraise-section-full-refresh? nil))
          (git-appraise--build-buffer buff-name)))
      (git-appraise-comment-reset-global-vars)
      (when pending-event
        (git-appraise--submit pending-event))))
   (git-appraise-comment-send?
    (oset git-appraise-comment-uncommitted msg comment-text)
    (git-appraise-comment-handler-commit
     git-appraise-comment-uncommitted git-appraise-comment-single-comment-msg)
    (git-appraise-comment-reset-global-vars))
   (git-appraise-comment-single-comment?
    (let* ((msg (git-appraise-utils--comment-clean-msg
                 comment-text git-appraise-comment-single-comment-msg))
           (buff-name (git-appraise-pr-buffer-name pr))
           (buf (get-buffer buff-name))
           (cursor-pos (or (and buf (window-live-p (get-buffer-window buf))
                                (window-point (get-buffer-window buf)))
                           (and buf (with-current-buffer buf (point)))))
           (callback (lambda (&rest _)
                       (let ((git-appraise-db--pullreq-id (oref pr id)))
                         (pcase (git-appraise-backend-refresh-comments
                                 (git-appraise-db-get-pullreq))
                           ('head-changed
                            (let ((git-appraise-section-full-refresh? t)
                                  (git-appraise-comment-cursor-pos cursor-pos))
                              (git-appraise--build-buffer buff-name)))
                           (_
                            (let ((git-appraise-section-full-refresh? nil)
                                  (git-appraise-comment-cursor-pos cursor-pos))
                              (git-appraise--build-buffer buff-name))))
                         (git-appraise-comment-reset-global-vars)))))
      (when (string-empty-p msg)
        (user-error "Comment is empty"))
      (git-appraise-new-issue-comment pr msg callback)))
   (t
    (oset git-appraise-comment-uncommitted msg comment-text)
    (git-appraise-comment-handler-commit
     git-appraise-comment-uncommitted
     git-appraise-comment-buffer-msg)
    (git-appraise-comment-reset-global-vars))))

;;;###autoload
(defun git-appraise-comment-commit ()
  "Commit to the composer's recorded review, retaining it if saving fails."
  (interactive)
  (let* ((buffer (get-buffer git-appraise-comment-buffer-name))
         (window (get-buffer-window buffer t))
         (review-buffer (buffer-local-value 'git-appraise-comment-review-buffer buffer))
         (git-appraise-db--pullreq-id
          (if review-buffer
              (or (and (buffer-live-p review-buffer)
                       (buffer-local-value 'git-appraise-review-buffer-pr-id review-buffer))
                  (user-error "Original review is no longer available"))
            git-appraise-db--pullreq-id))
         (comment-text (string-trim
                        (with-current-buffer buffer
                          (buffer-substring-no-properties (point-min) (point-max)))))
         (pr (or (git-appraise-db-get-pullreq)
                 (user-error "Original review is no longer available"))))
    (git-appraise-comment--commit-text pr comment-text)
    ;; The handler must persist the draft before the composer disappears.
    ;; Use the captured buffer/window: rendering can change the selection.
    (kill-buffer buffer)
    (when (window-live-p window)
      (if (= 1 (length (window-list (window-frame window))))
          (when (> (length (frame-list)) 1)
            (delete-frame (window-frame window)))
        (delete-window window)))))

;;; ----

;;;###autoload
(defun git-appraise-input-mention-user-at-point ()
  "Insert @USERNAME at current point to mention a user."
  (interactive)
  (let* ((pr (git-appraise-db-get-pullreq))
         (users (ignore-errors (git-appraise-get-mentionable-users pr))))
    (if (not users)
        (user-error "No mentionable users found for this repository")
      (let* ((logins (sort (-map (lambda (it) (a-get it 'login)) users)
                           #'string-lessp))
             (user (completing-read "Mention user: " logins nil nil)))
        (insert "@" user " ")))))

;;;###autoload
(defun git-appraise-comment-quit ()
  "Quit the comment window."
  (interactive)
  (kill-buffer (current-buffer))
  (if (= 1 (length (window-list (window-frame (selected-window)))))
      (delete-frame (window-frame (selected-window)))
    (delete-window (selected-window)))
  (with-current-buffer (git-appraise-review-buffer)
    (goto-char git-appraise-comment-cursor-pos)
    (git-appraise-comment-reset-global-vars)))

(defvar git-appraise-comment-mode-map
  (let ((map (copy-keymap markdown-mode-map)))
    (define-key map (kbd "C-c C-c") 'git-appraise-comment-commit)
    (define-key map (kbd "C-c C-k") 'git-appraise-comment-quit)
    (define-key map (kbd "C-c @") 'git-appraise-input-mention-user-at-point)
    (set-keymap-parent map markdown-mode-map)
    map))

(define-derived-mode git-appraise-comment-mode markdown-mode "Git Appraise Comment"
  "Git Appraise Comment."
  ;; Review buffers are per-PR: bind this comment buffer to the
  ;; review it was opened for.  The DB's current pullreq is the PR
  ;; of the review buffer the user just interacted with.
  (setq git-appraise-comment-review-buffer
        (get-buffer (git-appraise-pr-buffer-name))))

(provide 'git-appraise-comment)
;;; git-appraise-comment.el ends here
