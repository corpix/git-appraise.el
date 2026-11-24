;;; git-appraise-section-shared.el --- shared section-render primitives -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski
;;
;; Maintainer: Andrea <andrea-dev@hotmail.com>
;; Keywords: git, tools, vc
;; Homepage: https://github.com/corpix/git-appraise.el
;;
;; Split from git-appraise-section.el (phase 15c):
;; git-appraise-section.el, moved verbatim by the split; the fork and
;; this file are maintained by Andrea.

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; shared section-render primitives (part of the git-appraise section rendering, split from
;; git-appraise-section.el in phase 15c).

;;; Code:

(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'a)
(require 'git-appraise-faces)
(require 'git-appraise-db)
(require 'git-appraise-utils)
(require 'shr)
(require 'dom)

(defclass git-appraise-files-report-section (magit-section)
  ()
  "Section for the files changed report.")




(defcustom git-appraise-section-indent-width 1
  "Indent width for nested sections."
  :type 'integer
  :group 'git-appraise)
(defcustom git-appraise-section-image-scaling 0.8
  "Image scaling number used to resize images in buffer."
  :type 'float
  :group 'git-appraise)
(defcustom git-appraise-fill-column 80
  "Column number to wrap comments."
  :group 'git-appraise
  :type 'integer)
(defvar-local git-appraise-focus-mode nil
  "When non-nil, files auto-flagged as noise are hidden.
See `git-appraise-diff-noise-rules' and
`git-appraise-toggle-focus-mode'.")
(put 'git-appraise-focus-mode 'permanent-local t)
(defvar-local git-appraise-section--file-classifications nil
  "Hash table path -> plist (:tag :collapse :hide) for the diff.
Recomputed by `git-appraise--diff--classify-diff' on every render.
For internal usage only.")
;; permanent-local: the deferred render tail RE-RUNS the mode body
;; after the wash (kill-all-local-variables), and a wiped table
;; silently disables the viewed-counter rewrite forever — its
;; hash-table-p guard just no-ops, leaving the count stale (the
;; 2026-10-30 counter bug).  The wash recomputes it on every render,
;; so persistence across mode re-runs is safe.
(put 'git-appraise-section--file-classifications 'permanent-local t)

(defcustom git-appraise-viewed-indicator "✓"
  "Indicator shown for viewed files/hunks.
Set to nil to disable."
  :group 'git-appraise
  :type '(choice (const :tag "Checkmark" "✓")
                 (const :tag "None" nil)
                 string))

(defcustom git-appraise-comment-indicator "💬"
  "Icon shown before comment counts in file headings.
Set to nil to disable the comment count column."
  :group 'git-appraise
  :type '(choice (const :tag "Speech balloon" "💬")
                 (const :tag "None" nil)
                 string))

(define-obsolete-variable-alias 'git-appraise-section--viewed-tag
  'git-appraise-viewed-indicator "0.0.8")

(defconst git-appraise-section--viewed-counter-format "%d/%d viewed"
  "The N/M viewed counter line below the Files changed heading.
Rendered by `git-appraise-section-insert-files-changed' from the
viewed projection (`git-appraise-viewed--counter-string').")

(defconst git-appraise-section--viewed-counter-regexp
  (replace-regexp-in-string "%d" "[0-9]+"
                            git-appraise-section--viewed-counter-format)
  "Regexp matching the counter piece, derived from
`git-appraise-section--viewed-counter-format' so the two cannot
drift apart.")

(defconst git-appraise-section--changed-tag "  [CHANGED]"
  "The tag on a file heading viewed before but changed since.
Inserted by `git-appraise-wash--insert-file-tags' when the
classification carries :changed; a soft signal that never affects
the rollup or the counter.")
(defcustom git-appraise-fold-header-sections t
  "When non-nil, low-signal top sections start folded.
\"Commits\" (with their CI checks), \"Description\", \"Your
Review Feedback\" and \"Conversation\" are collapsed to their
headings, so the diff is the first thing you see when the buffer
opens.  Press TAB on any of them to expand."
  :type 'boolean
  :group 'git-appraise)
(defcustom git-appraise-collapse-bot-comments t
  "When non-nil, comments authored by bots start folded.
AI-bot review comments (and bot-to-bot chatter in particular)
are noise for the human reviewer, so each is collapsed to its
one-line \"@author - date\" heading.  Threads where a human
replied stay fully expanded, and TAB unfolds any of them."
  :type 'boolean
  :group 'git-appraise)
(defcustom git-appraise-bot-author-regexp
  "\\`\\(github-actions\\|devin\\|cursor\\|copilot\\|gemini\\|codeium\\|coderabbit\\|codspeed\\|greptile\\|codecov\\|claassistant\\|renovate\\|dependabot\\|imgbot\\|semantic-release\\|all-contributors\\)\\|\\[bot\\]\\'"
  "Regexp matching comment authors considered bots.
Matched case-insensitively against the login, e.g.
\"devin-ai-integration\" or \"cursor[bot]\".  Extend this list
when a new automated reviewer shows up in your PRs.
See `git-appraise-collapse-bot-comments'."
  :type 'regexp
  :group 'git-appraise)
(defcustom git-appraise-comment-fringe-markers t
  "When non-nil, diff lines carrying a review thread get a fringe marker.
The marker (violet chevron in the left fringe) flags exactly which
diff lines have a conversation attached, including threads folded
away by `git-appraise-collapse-bot-comments' or collapsed hunks.
Mouse-1 on the marked line jumps to the thread.
Re-laid automatically on every render; turn off if the fringe
noise bothers you."
  :type 'boolean
  :group 'git-appraise)
(defun git-appraise--propertize-keyword (str)
  "Add property face to STR."
  (propertize str 'face
              (cond
               ((member str '("MERGED" "SUCCESS" "COMPLETED" "APPROVED" "REJECTED"))
                'git-appraise-success-state-face)
               ((member str '("FAILURE" "TIMED_OUT" "ERROR" "CHANGES_REQUESTED" "CLOSED" "CONFLICTING"))
                'git-appraise-error-state-face)
               ((member str '("RESOLVED" "OUTDATED"))
                'git-appraise-info-state-face)
               ((member str '("PENDING"))
                'git-appraise-pending-state-face)
               (t
                'git-appraise-state-face))))
(defvar git-appraise-section-full-refresh? nil
  "Indicate if we want to perform a complete restart.
For internal usage only.")
(defvar-local git-appraise-section-grouped-comments nil
  "Hold grouped comments to avoid computation on every hunk line.
For internal usage only.")
(put 'git-appraise-section-grouped-comments 'permanent-local t)
(defvar-local git-appraise-section-hold-written-comment-ids nil
  "List to hold written comments ids.
For internal usage only.")
(put 'git-appraise-section-hold-written-comment-ids 'permanent-local t)
(defvar-local git-appraise-section-hold-written-comment-count nil
  "List of number of lines of comments written in the buffer.
For internal usage only.")
(put 'git-appraise-section-hold-written-comment-count 'permanent-local t)
(defvar git-appraise-section--display-all-comments t
  "Variable to define if we should display all comments or not.
For internal usage only.")
(defvar git-appraise-section--display-top-level-comments t
  "Variable to define if we should display top level comments or not.
For internal usage only.")
(defvar git-appraise-section--display-diff-comments t
  "Variable to define if we should display diff comments or not.
For internal usage only.")

;; utility functions

;; Phase 11b: git-appraise OWNS the diff wash.  Up to this point the
;; render went through magit-diff internals (`magit-diff-wash-diff',
;; `magit-diff-insert-file-section', `magit-diff-wash-hunk') via
;; :override advices, whose private contracts changed repeatedly
;; across magit 4.x (paint helpers renamed/removed, `:washer' became
;; a lazy-body inserter, return values silently drive the wash loop).
;; The wash primitives below read plain unified-diff text, whose
;; format git has kept stable for decades.  Only magit-SECTION is
;; used (the `magit-insert-section' macro, section classes,
;; visibility), which is the stable part of magit.

(defun git-appraise-wash--delete-line ()
  "Delete the current line, including its newline."
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))
(defun git-appraise-wash--paint-hunk (section)
  "Paint the body of hunk SECTION with the diff faces.
Our own base pass: the first character of each line selects the
face (`magit-diff-added', `magit-diff-removed' or
`magit-diff-context'), which guarantees red/green hunks on every
magit version.  When the stable `magit-section-paint' generic is
available, call it on top for the extras (whitespace and
refinement details)."
  (save-excursion
    (goto-char (oref section start))
    (forward-line)                       ; skip the hunk heading
    (let ((end (oref section end)))
      (while (< (point) end)
        (put-text-property (point) (line-end-position)
                           'font-lock-face
                           (cond ((eq (char-after) ?+) 'magit-diff-added)
                                 ((eq (char-after) ?-) 'magit-diff-removed)
                                 (t 'magit-diff-context)))
        (forward-line))))
  (when (fboundp 'magit-section-paint)
    (save-excursion
      (goto-char (oref section start))
      (magit-section-paint section nil))))
(defun git-appraise--html-written-loc (body &optional indent)
  "Compute HTML BODY's line count, preserving the review point.
INDENT is an optional.
SHR can temporarily display its scratch buffer for pixel measurements.
Restoring that window configuration also restores other windows' points,
including this review's stale window-point during a composer save."
  (save-excursion
    (let ((shr-indentation (* (or indent 0) (shr-string-pixel-width "-")))
          (image-scaling-factor git-appraise-section-image-scaling)
          (shr-width git-appraise-fill-column)
          start
          dom)
      (with-temp-buffer
        (insert body)
        (setq dom (libxml-parse-html-region (point-min) (point-max))))
      (let ((res (catch 'shr-failed
                   (with-temp-buffer
                     (setq start (point))
                     (insert " ")
                     (narrow-to-region start (1+ start))
                     (goto-char start)
                     (condition-case err
                         (progn
                           (shr-insert-document dom)
                           ;; delete the inserted " "
                           (delete-char 1)
                           (buffer-substring-no-properties
                            (point-min) (point-max)))
                       (error
                        ;; the same shr crash the plain insert would
                        ;; hit: mirror the plain-text fallback's line
                        ;; count approximation
                        (git-appraise-utils--log
                         "git-appraise--html-written-loc"
                         (format "shr render failed (%s); \
estimating from raw text"
                                 (error-message-string err)))
                        (throw 'shr-failed nil)))))))
        (if (stringp res)
            (1- (length (split-string res "\n")))
          ;; fallback approximation: the raw body's line count
          (1+ (cl-count ?\n body)))))))
(defun git-appraise--dom-string (dom)
  "Return string of DOM."
  (mapconcat (lambda (sub)
               (if (stringp sub)
                   sub
                 (git-appraise--dom-string sub)))
             (dom-children dom) ""))
(defun git-appraise--shr-tag-div (dom)
  "Rendering div tag as DOM in shr, with special handle for suggested-changes."
  (if (not (string-match-p ".*suggested-changes.*" (or (dom-attr dom 'class) "")))
      (shr-tag-div dom)
    (let ((tbody (dom-by-tag dom 'tbody)))
      (let ((shr-current-font 'git-appraise-info-state-face))
        (shr-insert "* Suggested change:")
        (insert "\n"))
      (dolist (tr (dom-non-text-children tbody))
        (dolist (td (dom-non-text-children tr))
          (let ((classes (split-string (or (dom-attr td 'class) ""))))
            (cond
             ((member "blob-num" classes) t)
             ((member "blob-code-deletion" classes)
              (let ((shr-current-font 'diff-indicator-removed))
                (shr-insert "-"))
              (insert (propertize (concat (git-appraise--dom-string td)
                                          "\n")
                                  'face 'diff-removed)))
             ((member "blob-code-addition" classes)
              (let ((shr-current-font 'diff-indicator-added))
                (shr-insert "+"))
              (insert (propertize (concat (git-appraise--dom-string td)
                                          "\n")
                                  'face 'diff-added)))
             (t
              (shr-generic td)))))))))
(defun git-appraise--shr-tag-sub (dom)
  "Like `shr-tag-sub', but survive a <sub> at the START of the narrowing.
shr's version does (forward-char -1) at bol to delete the newline
before the subscript; at the very beginning of the buffer that
signals (beginning-of-buffer) and kills the whole render
(2026-11-05, a greptile badge comment whose first element is a
<sub> inside the one-char narrowing hack)."
  (when (bolp)
    (unless (bobp)
      (forward-char -1)
      (delete-char 1)))
  (let ((start (point)))
    (shr-generic dom)
    (put-text-property start (point) 'display `(raise ,shr-sub-raise-factor))
    (add-face-text-property start (point) 'shr-sup)))

(defun git-appraise--insert-html (body &optional indent)
  "Insert html content BODY.
INDENT is an optional number, if provided,
INDENT count of spaces are added at the start of every line.
A shr crash must never kill the render: it is logged to
`git-appraise-log-file' (with the body's size and a snippet, so it
can be fixed when it happens) and BODY degrades to plain text —
a lower-resolution comment beats no diff buffer."
  (let ((shr-indentation (* (or indent 0) (shr-string-pixel-width "-")))
        (image-scaling-factor git-appraise-section-image-scaling)
        (shr-external-rendering-functions
         '((div . git-appraise--shr-tag-div)
           (sub . git-appraise--shr-tag-sub)))
        (shr-width git-appraise-fill-column)
        (start (point))
        end
        dom)
    (with-temp-buffer
      (insert body)
      (setq dom (libxml-parse-html-region (point-min) (point-max))))
    ;; narrow the buffer and insert dom. otherwise there would be an extra new line at start
    (save-restriction
      (insert " ")
      (narrow-to-region start (1+ start))
      (goto-char start)
      (condition-case err
          (progn
            (shr-insert-document dom)
            ;; delete the inserted " "
            (delete-char 1)
            (setq end (point)))
        (error
         (let ((snippet (replace-regexp-in-string
                         "[\n\t]+" " "
                         (substring-no-properties
                          body 0 (min 300 (length body))))))
           (git-appraise-utils--log
            "git-appraise--insert-html"
            (format "shr render failed (%s); body %d bytes, \
starts with: %s"
                    (error-message-string err)
                    (length body)
                    snippet))
           ;; clean the partial render out of the narrowed region and
           ;; degrade to the raw body as plain text
           (delete-region (point-min) (point-max))
           (goto-char (point-min))
           (insert body)
           (setq end (point))))))
    (when (> shr-indentation 0)
      ;; shr-indentation does not work for images and code block
      ;; let's fix it: prepend space for any lines that does not starts with a space
      ;; (but we still need to use shr-indentation because otherwise the line will be too long)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (unless (or (looking-at-p "\n")
                      (eq 'space (car-safe (get-text-property (point) 'display))))
            (beginning-of-line)
            (insert (propertize " " 'display `(space :width (,shr-indentation)))))
          (forward-line))))))
(defun git-appraise--diff--file-has-comments-p (path)
  "Non-nil when PATH carries review comments."
  (let ((res nil)
        (groups git-appraise-section-grouped-comments))
    (while (and groups (not res))
      (let ((objs (cdr (pop groups))))
        (while (and objs (not res))
          (let ((c (pop objs)))
            (when (and (slot-boundp c 'path)
                       (string-equal (oref c path) path))
              (setq res t))))))
    res))
(defun git-appraise-section--hide-if-hidden (section)
  "Fold the body of SECTION when it was created hidden.
Magit computes the initial visibility of a section when it is
created, but the actual folding only happens while a buffer is
refreshed via `magit-refresh-buffer'.  Git Appraise renders its
buffers directly, so sections that should start collapsed
(outdated comments, commit CI details) must be folded here
explicitly."
  (when (and (oref section hidden)
             (not (eq section magit-root-section)))
    (save-excursion
      (magit-section-hide section))))

(defun git-appraise-section--file-collapse-p (path info)
  "Return PATH's default collapse policy from classification INFO.
Files carrying review comments stay open so discussion remains visible."
  (and (plist-get info :collapse)
       (not (git-appraise--diff--file-has-comments-p path))))

(defun git-appraise-section--dimmed (text)
  "Return TEXT propertized with the dimmed face."
  (propertize text 'font-lock-face 'git-appraise-dimmed))

(defun git-appraise-section--insert-dimmed (text)
  "Insert TEXT with the dimmed face."
  (insert (git-appraise-section--dimmed text)))

(provide 'git-appraise-section-shared)
;;; git-appraise-section-shared ends here
