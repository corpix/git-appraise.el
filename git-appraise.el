;;; git-appraise.el --- Review code with git-appraise -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski
;;
;; Version: 0.0.7
;; Keywords: git, tools, vc
;; Homepage: https://github.com/corpix/git-appraise.el
;; Package-Requires: ((emacs "28.1") (magit "3.0.0") (transient "0.3.7") (a "1.0.0") (uuidgen "1.2") (deferred "0.5.1") (markdown-mode "2.4") (emojify "1.2") (s "1.13.1") (dash "2.19.1"))
;;
;; This file is not part of GNU Emacs

;;; Commentary:
;;
;; Review local git-appraise reviews in Emacs.

;;; Code:

(require 'magit-section)
(require 'git-appraise-backend)
(require 'git-appraise-diff)
(require 'git-appraise-section)
(require 'git-appraise-comment)
(require 'git-appraise-utils)
(require 'git-appraise-db)
(require 'git-appraise-interfaces)
(require 'git-appraise-faces)
(require 'git-appraise-actions)
(require 'git-appraise-folding)
(require 's)

(defgroup git-appraise nil
  "Git Appraise tool for git-appraise."
  :group 'tools)

(defcustom git-appraise-start-backend 'appraise
  "Backend selected by interactive `git-appraise-start'.
Only `appraise' is supported."
  :group 'git-appraise
  :type '(choice (const :tag "Local git-appraise" appraise)))

(defcustom git-appraise-headers-hook
  '(git-appraise-section-insert-header-title
    git-appraise-section-insert-files-summary
    git-appraise-section-insert-author
    git-appraise-section-insert-title
    git-appraise-section-insert-state
    git-appraise-section-insert-ref
    git-appraise-section-insert-reviewers)
  "Hook run to insert headers into the git-appraise buffer."
  :group 'git-appraise
  :type 'hook)

(defcustom git-appraise-sections-hook
  '(git-appraise-section-insert-headers
    git-appraise-section-insert-commits
    git-appraise-section-insert-pr-description
    git-appraise-section-insert-feedback-heading
    git-appraise-section-insert-top-level-comments)
  "Hook run to insert sections into a git-appraise buffer."
  :group 'git-appraise
  :type 'hook)

(defcustom git-appraise-sections-commit-hook
  '(git-appraise-section-insert-headers)
  "Hook run to insert sections into a git-appraise commit buffer."
  :group 'git-appraise
  :type 'hook)

(defun git-appraise-auth-source-debug ()
  "Compatibility no-op."
  (setq-local auth-source-debug (lambda (&rest _))))

;;;###autoload
(defun git-appraise-start (url)
  "Start a review from a git-appraise revision.
When `git-appraise-start-backend' is `appraise', interactive use
offers local reviews instead of prompting for a URL."
  (interactive
   (list (if (eq git-appraise-start-backend 'appraise)
             (git-appraise-backend-select-review)
           (read-string "Review revision: "))))
  (let ((git-appraise-section-full-refresh? t))
    (git-appraise-backend-open url)
    (git-appraise--build-buffer)))

(define-minor-mode git-appraise-commit-minor-mode
  "Git Appraise Commit."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "b") 'git-appraise-commit-buffer-back)
            (define-key map (kbd "q") 'git-appraise-commit-buffer-back)
            map))

(defun git-appraise-transient-dispatch ()
  "Open the review transient."
  (interactive)
  (git-appraise-transient-api))

(transient-define-prefix git-appraise-help ()
  "Show the commands available while reading a git-appraise."
  [["At point"
    ("c" "Comment" git-appraise-comment-add-or-edit)]
   ["Move and display"
    ("n" "Next file or hunk" git-appraise-next-item)
    ("p" "Previous file or hunk" git-appraise-previous-item)
    ("f" "Fold file" git-appraise-fold-file-at-point)
    ("h" "Fold file hunks" git-appraise-fold-hunks-in-file)
    ("H" "Unfold file hunks" git-appraise-unfold-hunks-in-file)
    ("F" "Fold all files" git-appraise-fold-all-files)
    ("U" "Unfold all files" git-appraise-unfold-all-files)
    ("b" "Description" git-appraise-popup-pr-description)
    ("x" "Toggle generated" git-appraise-toggle-exclude-gitattributes-generated)]]
  ["Review"
   ("r" "Submit" git-appraise-transient-dispatch)
   ("G" "Reload" git-appraise-reload)])

;;;###autoload (autoload 'git-appraise-magit-dispatch "git-appraise" nil t)
(transient-define-prefix git-appraise-magit-dispatch ()
  "Start a git-appraise from a Magit buffer."
  [["Review"
    ("o" "Open" git-appraise-magit-start-appraise)
    ("n" "New" git-appraise-backend-request)]])

(defun git-appraise-magit-start-appraise ()
  "Select a local git-appraise review from the current repository."
  (interactive)
  (let ((git-appraise-start-backend 'appraise))
    (call-interactively #'git-appraise-start)))

;;;###autoload
(with-eval-after-load 'magit
  (unless (ignore-errors
            (transient-get-suffix 'magit-dispatch "R"))
    (transient-append-suffix
      'magit-dispatch 'magit-info
      '("R" "Code review" git-appraise-magit-dispatch)))
  (when (boundp 'magit-status-mode-map)
    (define-key magit-status-mode-map (kbd "R") #'git-appraise-magit-dispatch))
  (when (boundp 'magit-revision-mode-map)
    (define-key magit-revision-mode-map (kbd "R") #'git-appraise-magit-dispatch)))

(transient-define-prefix git-appraise-transient-api ()
  "Git Appraise."
  [["Review"
    ("a" "Approve" git-appraise-submit-approve)
    ("r" "Request changes" git-appraise-submit-request-changes)
    ("c" "Comment" git-appraise-submit-comments)]
   ["Fast track"
    ("l" "LGTM" git-appraise-submit-lgtm)
    ("s c" "Single comment" git-appraise-submit-single-top-level-comment)
    ("s C" "Diff comment" git-appraise-submit-single-diff-comment-at-point)]
   ["Buffer"
    ("d" "Description" git-appraise-popup-pr-description)
    ("v" "Toggle viewed" git-appraise-viewed-toggle)
    ("V" "Toggle file viewed" git-appraise-viewed-toggle-file)
    ("x" "Toggle generated" git-appraise-toggle-exclude-gitattributes-generated)
    ("G" "Reload" git-appraise-reload)
    ("q" "Quit" transient-quit-one)]])

(defvar git-appraise-mode-map
  (let ((map (copy-keymap magit-section-mode-map)))
    (suppress-keymap map t)
    (define-key map (kbd "r") 'git-appraise-help)
    (define-key map (kbd "RET") 'git-appraise-comment-add-or-edit)
    (define-key map (kbd "TAB") #'git-appraise-fold-toggle-at-point)
    (define-key map (kbd "C-c RET") 'git-appraise-submit-single-diff-comment-at-point)
    (define-key map (kbd "C-c C-n") 'git-appraise-next-hunk)
    (define-key map (kbd "C-c C-p") 'git-appraise-previous-hunk)
    (define-key map (kbd "n") 'git-appraise-next-item)
    (define-key map (kbd "p") 'git-appraise-previous-item)
    (define-key map (kbd "G") 'git-appraise-reload)
    (define-key map (kbd "d") 'git-appraise-popup-pr-description)
    (define-key map (kbd "v") 'git-appraise-viewed-toggle)
    (define-key map (kbd "V") 'git-appraise-viewed-toggle-file)
    (define-key map (kbd "S-<tab>") 'git-appraise-fold-file-at-point)
    (define-key map (kbd "<backtab>") 'git-appraise-fold-file-at-point)
    (set-keymap-parent map magit-section-mode-map)
    map))

(define-derived-mode git-appraise-mode magit-section-mode "Git Appraise"
  "Git Appraise mode."
  (add-hook 'after-change-functions #'git-appraise--after-change nil t)
  (when (boundp 'magit-section-highlight-current)
    (setq-local magit-section-highlight-current nil))
  (when (boundp 'magit-section-highlight-selection)
    (setq-local magit-section-highlight-selection nil))
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1)))

(defcustom git-appraise-post-hook nil
  "Hook run after the Git Appraise buffer is fully loaded and rendered."
  :group 'git-appraise
  :type 'hook)

(defun git-appraise-kill-pr-url (_jump?)
  "No URL for git-appraise reviews."
  (interactive "P")
  (user-error "Git-appraise reviews have no URL"))

(provide 'git-appraise)
;;; git-appraise.el ends here
