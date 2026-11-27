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
(declare-function git-appraise-backend-update-request "git-appraise-backend")
(declare-function git-appraise-backend-send-general-comment "git-appraise-backend")
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

(defvar-local git-appraise-comment--session nil
  "Composer context captured from the review that opened this buffer.")
(put 'git-appraise-comment--session 'permanent-local t)

(defun git-appraise-comment--capture-session ()
  "Keep the comment action with this composer across review switches."
  (setq git-appraise-comment--session
        (list :cursor git-appraise-comment-cursor-pos
              :feedback git-appraise-comment-feedback?
              :title git-appraise-comment-title?
              :description git-appraise-comment-description?
              :single git-appraise-comment-single-comment?
              :target git-appraise-comment-uncommitted
              :commit-buffer git-appraise-comment-commit-buffer?
              :send git-appraise-comment-send?
              :submit git-appraise-comment-pending-submit-event)))

(defun git-appraise-comment--composer-buffer (review-buffer)
  "Return a composer for REVIEW-BUFFER without replacing another review's."
  (let ((existing (get-buffer git-appraise-comment-buffer-name)))
    (if (and existing
             (buffer-local-value 'git-appraise-comment-review-buffer existing)
             (not (eq (buffer-local-value
                       'git-appraise-comment-review-buffer existing)
                      review-buffer)))
        (generate-new-buffer git-appraise-comment-buffer-name)
      (get-buffer-create git-appraise-comment-buffer-name))))

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
  "Remove every known composer helper text from MSG and trim the rest.
Whichever flow opened a composer seeded one of the known helper
texts; a commit must not depend on which one."
  (string-trim
   (let ((msg (or msg ""))
         (helpers (delq nil
                        (list (and (boundp 'git-appraise-comment-buffer-msg)
                                   git-appraise-comment-buffer-msg)
                              (and (boundp 'git-appraise-comment-feedback-msg)
                                   git-appraise-comment-feedback-msg)
                              (and (boundp 'git-appraise-comment-single-comment-msg)
                                   git-appraise-comment-single-comment-msg)))))
     (dolist (helper helpers msg)
       (setq msg (git-appraise-utils--comment-clean-msg msg helper))))))

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
(defun git-appraise--compute-line-side-and-range (&optional region-beg region-end)
  "Compute review location at point with optional REGION-BEG/REGION-END.
Without bounds the active region is used when one is active.
Returns plist with :side :line and optionally :start-side :start-line."
  (let* ((curr (git-appraise--compute-line-and-side))
         (side (car curr))
         (line (cdr curr))
         (beg (or region-beg (and (use-region-p) (region-beginning))))
         (end (or region-end (and (use-region-p) (region-end)))))
    (if (and beg end (/= beg end))
        (save-excursion
          (let* ((beg-side (progn (goto-char beg)
                                  (git-appraise--compute-line-and-side)))
                 (end-side (progn (goto-char end)
                                  (git-appraise--compute-line-and-side)))
                 (start (if (<= beg end) beg-side end-side))
                 (finish (if (<= beg end) end-side beg-side)))
            (list :side (car finish)
                  :line (cdr finish)
                  :start-side (car start)
                  :start-line (cdr start))))
      (list :side side :line line))))

(defun git-appraise--comment-region-bounds ()
  "Validate the active region as patch lines within a single hunk.
Return (BEG END).  A selection ending on a line boundary counts as
ending through the previous line, so selecting a hunk's last lines
with whole-line motions anchors inside the hunk."
  (let* ((beg (region-beginning))
         (end (if (save-excursion (goto-char (region-end))
                                  (and (bolp) (> (region-end) beg)))
                  (1- (region-end))
                (region-end))))
    (save-excursion
      (let ((hunk-at
             (lambda (pos)
               (goto-char pos)
               (unless (git-appraise--patch-line-p)
                 (user-error "Select only patch lines for a comment"))
               (magit-current-section))))
        (unless (eq (funcall hunk-at beg) (funcall hunk-at end))
          (user-error "Select patch lines within a single hunk"))))
    (list beg end)))

;;; Comment C_UD

(defun git-appraise-comment-add (&optional msg)
  "Add comment.
Optionally define a MSG."
  (let* ((review-buffer (git-appraise-review-buffer))
         (buffer (git-appraise-comment--composer-buffer review-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (if msg msg git-appraise-comment-buffer-msg))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (git-appraise-comment-mode)
      (setq git-appraise-comment-review-buffer review-buffer)
      (git-appraise-comment--capture-session))))

(defun git-appraise-comment-edit ()
  "Edit comment."
  (with-slots (value) (magit-current-section)
    (let* ((review-buffer (git-appraise-review-buffer))
           (buffer (git-appraise-comment--composer-buffer review-buffer)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (oref value msg))
        (insert ?\n)
        (goto-char (point-min)))
      (switch-to-buffer-other-window buffer)
      (git-appraise-comment-mode)
      (setq git-appraise-comment-review-buffer review-buffer)
      (git-appraise-comment--capture-session))))

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

(cl-defmethod git-appraise-comment-handler-add-or-edit (obj)
  "Add a comment in the OBJ.
Comments must anchor to a file heading or a patch line: a position
between the file heading and the hunk header has no source location."
  (with-slots (type) (magit-current-section)
    (if (and (equal type 'file)
             (git-appraise-backend-repo-p (git-appraise-db-get-pullreq)))
        (let ((file (magit-current-section)))
          (unless (= (line-beginning-position)
                     (save-excursion
                       (goto-char (oref file start))
                       (line-beginning-position)))
            (user-error "Place the file comment on its heading"))
          (let ((comment (git-appraise-local-comment-section
                          :state "LOCAL COMMENT"
                          :author (git-appraise-utils--git-get-user)
                          :path (string-remove-prefix "b/" (substring-no-properties obj))
                          :position 0
                          :line-type "FILE"
                          :send? nil)))
            (setq git-appraise-comment-uncommitted comment)
            (git-appraise-comment-add)))
      (unless (equal type 'hunk)
        (user-error "Place the comment on a file heading or patch line"))
      (unless (git-appraise--patch-line-p)
        (user-error "Place the comment on a patch line"))
      (let* ((region (when (use-region-p)
                       (git-appraise--comment-region-bounds)))
             (line-type (pcase (char-after (line-beginning-position))
                          (?- "REMOVED")
                          (?+ "ADDED")
                          (_ "UNCHANGED")))
             (loc (if region
                      (apply #'git-appraise--compute-line-side-and-range region)
                    (git-appraise--compute-line-side-and-range)))
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
                             :send? git-appraise-comment-send?)))
        (setq git-appraise-comment-uncommitted local-comment)
        (git-appraise-comment-add)))))


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

(defun git-appraise-comment--refresh (pr)
  "Refresh PR without depending on the current review's global context."
  (let ((git-appraise-db--pullreq-id (oref pr id)))
    (git-appraise--build-buffer (git-appraise-pr-buffer-name pr))))

(defun git-appraise-comment--commit-text (pr comment-text)
  "Dispatch COMMENT-TEXT for PR while the composer remains available."
  (cond
   (git-appraise-comment-description?
    (git-appraise-backend-update-request pr comment-text)
    (git-appraise-comment--refresh pr)
    (git-appraise-comment-reset-global-vars))
   (git-appraise-comment-title?
    (when (string-empty-p comment-text)
      (user-error "Review title is empty"))
    (let* ((old (or (a-get (oref pr raw-infos) 'bodyText) ""))
           (newline (string-match "\n" old))
           (message (concat comment-text
                            (if newline (substring old newline) ""))))
      (git-appraise-backend-update-request pr message)
      (git-appraise-comment--refresh pr)
      (git-appraise-comment-reset-global-vars)))
   (git-appraise-comment-feedback?
    (let* ((msg (git-appraise-comment-strip-known-helpers comment-text))
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
    (oset git-appraise-comment-uncommitted msg
          (git-appraise-comment-strip-known-helpers comment-text))
    (git-appraise-comment-handler-commit
     git-appraise-comment-uncommitted git-appraise-comment-single-comment-msg)
    (git-appraise-comment-reset-global-vars))
   (git-appraise-comment-single-comment?
    (let* ((msg (git-appraise-comment-strip-known-helpers comment-text))
           (buff-name (git-appraise-pr-buffer-name pr))
           (buf (get-buffer buff-name))
           (cursor-pos (or (and buf (window-live-p (get-buffer-window buf))
                                (window-point (get-buffer-window buf)))
                           (and buf (with-current-buffer buf (point)))))
           (callback (lambda (&rest _)
                       (let ((git-appraise-db--pullreq-id (oref pr id)))
                         (pcase (git-appraise-backend-refresh-comments
                                 (git-appraise-db-get-pullreq))
                           ('ok
                            (unless (git-appraise-comment--refresh-conversation-in-place
                                     buff-name)
                              (let ((git-appraise-section-full-refresh? nil)
                                    (git-appraise-comment-cursor-pos cursor-pos))
                                (git-appraise--build-buffer buff-name))))
                           ('head-changed
                            (let ((git-appraise-section-full-refresh? t)
                                  (git-appraise-comment-cursor-pos cursor-pos))
                              (git-appraise--build-buffer buff-name)))
                           (_ (message "Comment sent; local refresh failed")))
                         (git-appraise-comment-reset-global-vars)))))
      (when (string-empty-p msg)
        (user-error "Comment is empty"))
      (git-appraise-backend-send-general-comment pr msg callback)))
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
  (let* ((buffer (if (derived-mode-p 'git-appraise-comment-mode)
                     (current-buffer)
                   (get-buffer git-appraise-comment-buffer-name)))
         (window (get-buffer-window buffer t))
         (review-buffer (and buffer (buffer-local-value
                                     'git-appraise-comment-review-buffer buffer)))
         (session (and buffer (buffer-local-value
                               'git-appraise-comment--session buffer))))
    (unless (and buffer session)
      (user-error "No active comment composer"))
    (unwind-protect
        (let* ((git-appraise-comment-cursor-pos (plist-get session :cursor))
               (git-appraise-comment-feedback? (plist-get session :feedback))
               (git-appraise-comment-title? (plist-get session :title))
               (git-appraise-comment-description? (plist-get session :description))
               (git-appraise-comment-single-comment? (plist-get session :single))
               (git-appraise-comment-uncommitted (plist-get session :target))
               (git-appraise-comment-commit-buffer? (plist-get session :commit-buffer))
               (git-appraise-comment-send? (plist-get session :send))
               (git-appraise-comment-pending-submit-event (plist-get session :submit))
               (git-appraise-db--pullreq-id
                (if review-buffer
                    (or (and (buffer-live-p review-buffer)
                             (buffer-local-value 'git-appraise-review-buffer-pr-id
                                                 review-buffer))
                        (user-error "Original review is no longer available"))
                  git-appraise-db--pullreq-id))
               (comment-text (string-trim
                              (with-current-buffer buffer
                                (buffer-substring-no-properties
                                 (point-min) (point-max)))))
               (pr (or (git-appraise-db-get-pullreq)
                       (user-error "Original review is no longer available"))))
          (git-appraise-comment--commit-text pr comment-text)
          (kill-buffer buffer)
          (when (window-live-p window)
            (if (= 1 (length (window-list (window-frame window))))
                (when (> (length (frame-list)) 1)
                  (delete-frame (window-frame window)))
              (delete-window window))))
      (git-appraise-comment-reset-global-vars))))

;;; ----

;;;###autoload
(defun git-appraise-comment-quit ()
  "Quit the comment window."
  (interactive)
  (let ((review-buffer git-appraise-comment-review-buffer)
        (cursor (plist-get git-appraise-comment--session :cursor)))
    (kill-buffer (current-buffer))
    (if (= 1 (length (window-list (window-frame (selected-window)))))
        (delete-frame (window-frame (selected-window)))
      (delete-window (selected-window)))
    (when (buffer-live-p review-buffer)
      (with-current-buffer review-buffer
        (when cursor (goto-char cursor))))
    (git-appraise-comment-reset-global-vars)))

(defvar git-appraise-comment-mode-map
  (let ((map (copy-keymap markdown-mode-map)))
    (define-key map (kbd "C-c C-c") 'git-appraise-comment-commit)
    (define-key map (kbd "C-c C-k") 'git-appraise-comment-quit)
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
