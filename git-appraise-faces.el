;;; git-appraise-faces.el --- Define faces used in the package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski
;;
;; Version: 0.0.7
;; Homepage: https://github.com/corpix/git-appraise.el
;;
;; This file is not part of GNU Emacs.

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

;;; Commentary:
;;
;;  Define faces used in the package
;;
;;; Code:

(defface git-appraise-timestamp-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DimGrey"
     :slant italic)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DimGrey"
     :slant italic))
  "Face for timestamps."
  :group 'git-appraise)

(defface git-appraise-diff-tag-face
  `((((class color) (background light))
     :inherit font-lock-builtin-face
     :weight bold)
    (((class color) (background  dark))
     :inherit font-lock-builtin-face
     :weight bold))
  "Face for file classification tags like [DOC] or [WS-ONLY]."
  :group 'git-appraise)

(defface git-appraise-thread-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "SlateGrey"
     :weight bold)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "SlateGrey"
     :weight bold))
  "Face for threads."
  :group 'git-appraise)

(defface git-appraise-state-face
  '((t :inherit bold))
  "Face used for default state keywords."
  :group 'git-appraise)

(defface git-appraise-checker-name-face
  '((t :inherit bold :slant italic))
  "Face used for commit check name."
  :group 'git-appraise)

(defface git-appraise-checker-detail-face
  '((t :inherit magit-section-heading
       :slant italic
       :weight normal))
  "Face for details word."
  :group 'git-appraise)

(defface git-appraise-author-face
  '((t :inherit font-lock-keyword-face))
  "Face used for author names."
  :group 'git-appraise)

(defface git-appraise-url-header-face
  '((t :inherit font-lock-keyword-face
       :slant italic
       :underline t))
  "Face used for PR url in the header."
  :group 'git-appraise)

(defface git-appraise-author-header-face
  '((t :inherit font-lock-keyword-face
       :slant italic
       :underline t))
  "Face used for author name in the header."
  :group 'git-appraise)

(defface git-appraise-error-state-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face used for error state (e.g. changes requested)."
  :group 'git-appraise)

(defface git-appraise-success-state-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "Green")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "Green"))
  "Face used for success state (e.g. merged)."
  :group 'git-appraise)

(defface git-appraise-info-state-face
  '((t :slant italic))
  "Face used for info (unimportant) state (e.g. resolved)."
  :group 'git-appraise)

(defface git-appraise-pending-state-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "yellow4")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkKhaki"))
  "Face used for pending state."
  :group 'git-appraise)

(defface git-appraise-recent-comment-heading
  '((t :inherit magit-branch-remote :weight bold))
  "Face for recent comments."
  :group 'git-appraise)

(defface git-appraise-outdated-comment-heading
  '((t :inherit magit-cherry-equivalent :weight bold))
  "Face for outdated comments."
  :group 'git-appraise)

;; Background faces for comment bodies by author type
(defface git-appraise-comment-self-bg
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "light cyan")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background nil))
  "Background face for comments authored by the current user."
  :group 'git-appraise)

(defface git-appraise-comment-author-bg
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#eaf6ff") ; light sky-blue like
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#233040"))
  "Background face for comments authored by the PR author."
  :group 'git-appraise)

(defface git-appraise-comment-other-bg
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#fffbe5") ; light yellow
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#2b2b20"))
  "Background face for comments authored by other reviewers."
  :group 'git-appraise)

(defface git-appraise-dimmed
  '((((class color) (background light))
     :foreground "grey50")
    (((class color) (background  dark))
     :foreground "grey50"))
  "Face for text that shouldn't stand out."
  :group 'magit-faces)

;; Face used for hover interactions in Git Appraise buffers without
;; adding a background that would obscure diff colors.
(defface git-appraise-hover-face
  '((((class color)) :underline t))
  "Mouse hover face with no background."
  :group 'git-appraise)

;; Fringe marker for diff lines that carry a review thread (phase 9).
;; Uses the same violet family as `git-appraise-constant-face' so
;; "there is a conversation here" reads as one visual language.
(defface git-appraise-fringe-comment-face
  '((((class color) (background light))
     :foreground "dark violet")
    (((class color) (background dark))
     :foreground "MediumPurple1"))
  "Face for fringe markers flagging diff lines with review threads."
  :group 'git-appraise)

(defface git-appraise-commented-hunk-face
  '((t :inherit git-appraise-fringe-comment-face :weight bold))
  "Face for the comment marker on a folded diff hunk."
  :group 'git-appraise)

;; Phase 15: delicacy badge on hunk headings.  A warning-family
;; foreground, no background, so it never obscures the diff colors
;; (the hunk heading line itself carries no +/- faces).
(defface git-appraise-delicate-hunk-face
  '((((class color) (background light))
     :inherit font-lock-warning-face)
    (((class color) (background dark))
     :inherit font-lock-warning-face))
  "Face for the risk badge on delicate hunk headings (phase 15)."
  :group 'git-appraise)

(defface git-appraise-topic-label-face
  '((t (:box (:line-width (-1 . -1) :style nil))))
  "Face used for PR topic labels in the header."
  :group 'git-appraise)

(provide 'git-appraise-faces)
;;; git-appraise-faces.el ends here
