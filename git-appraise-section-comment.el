;;; git-appraise-section-comment.el --- the comment section classes and renderers -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski
;;
;; Keywords: git, tools, vc
;; Homepage: https://github.com/corpix/git-appraise.el
;;
;; Split from git-appraise-section.el (phase 15c):
;; git-appraise-section.el, moved verbatim by the split; the fork and

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

;; the comment section classes and renderers (part of the git-appraise section rendering, split from
;; git-appraise-section.el in phase 15c).

;;; Code:

(require 'git-appraise-section-shared)
(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'dash)
(require 'a)
(require 'git-appraise-faces)
(require 'git-appraise-comment-format)
(require 'git-appraise-db)
(require 'git-appraise-utils)
(declare-function git-appraise-wash-hunk "git-appraise-section-wash")
(declare-function git-appraise--patch-line-p "git-appraise-section")




(require 'git-appraise-section-conversation)

(defclass git-appraise-base-comment-section (magit-section)
  ((state      :initarg :state
               :type string)
   (author     :initarg :author
               :type string)
   (msg        :initarg :msg
               :type string)
   (body       :initarg :body
               :initform nil
               :type (or null string)
               :documentation "Raw (markdown) body as written by the author.
nil for local comments rendered before the data was available; the
raw body is what `e' (edit submitted comment) pre-fills the
comment buffer with.")
   (position   :initarg :position
               :initform nil
               :type (or null number))
   (side         :initarg :side
                 :documentation "Side for comment: LEFT or RIGHT")
   (line         :initarg :line
                 :initform nil
                 :documentation "Line number for side")
   (start-side   :initarg :start-side
                 :initform nil
                 :documentation "Start side for multi-line comments")
   (start-line   :initarg :start-line
                 :initform nil
                 :documentation "Start line for multi-line comments")
   (thread-id   :initarg :thread-id
                :initform nil
                :documentation "Review thread id when known.")
   (resolved?   :initarg :resolved?
                :initform nil
                :type boolean
                :documentation "Whether the review thread is resolved.")
   (reactions  :initarg :reactions
               :type (or null
                         (satisfies
                          (lambda (it)
                            (-all-p #'git-appraise-reaction-section-p it)))))
   (path       :initarg :path
               :type string)
   (diffHunk   :initarg :diffHunk
               :type (or null string))
   (id         :initarg :id
               :documentation "ID that identifies the comment in the backend.")
   (internalId :initarg :internalId)
   (amount-loc :initform nil)
   (outdated?  :initform nil
               :type boolean)
   (reply?     :initform nil
               :type boolean)
   (local?     :initform nil
               :type boolean)
   (createdAt  :initarg :createdAt)
   (updatedAt  :initarg :updatedAt)))

;; Comment classes are dual purpose: the same class instantiates the
;; prebuilt data objects AND the inserted magit sections (the section
;; wraps the data object in its `value' slot).  `magit-section-ident'
;; therefore calls this generic twice with the same class: first on
;; the section, then (via magit's fallback) on the data object.
;; Without these methods both calls return nil, every comment
;; section gets the SAME ident, and magit's visibility cache plus
;; old-tree matching treat all comments as one section: hiding one
;; bot comment then re-hides every comment on the next render, human
;; ones included.  The dispatch below covers both roles.
(cl-defmethod magit-section-ident-value ((obj git-appraise-base-comment-section))
  "Identify a comment by its backend id, falling back to author/msg.
When OBJ is the inserted section (its `value' wraps the data
object), delegate to that data object."
  (if (and (slot-boundp obj 'value)
           (eieio-object-p (oref obj value)))
      (magit-section-ident-value (oref obj value))
    (or (and (slot-boundp obj 'id) (oref obj id))
        (and (slot-boundp obj 'author) (oref obj author))
        (and (slot-boundp obj 'msg) (oref obj msg)))))
(cl-defmethod magit-section-ident-value ((obj git-appraise-comment-section))
  "Identify a conversation comment by id, falling back to author.
When OBJ is the inserted section (its `value' wraps the data
object), delegate to that data object."
  (if (and (slot-boundp obj 'value)
           (eieio-object-p (oref obj value)))
      (magit-section-ident-value (oref obj value))
    (or (and (slot-boundp obj 'id) (oref obj id))
        (and (slot-boundp obj 'author) (oref obj author))
        (and (slot-boundp obj 'msg) (oref obj msg)))))
(defclass git-appraise-code-comment-section (git-appraise-base-comment-section)
  ((keymap     :initform 'git-appraise-code-comment-section-map)
   (diffHunk   :initarg :diffHunk)
   (id         :initarg :id
               :documentation "ID that identifies the comment in the backend.")
   (amount-loc :initform nil)
   (outdated?  :initform nil
               :type boolean)
   (reply?     :initform nil
               :type boolean)
   (local?     :initform nil
               :type boolean)))
(defclass git-appraise-local-comment-section (git-appraise-base-comment-section)
  ((keymap       :initform 'git-appraise-local-comment-section-map)
   (fyi?         :initarg :fyi? :initform nil)
   (local?       :initform t)
   (reply?       :initform nil)
   (edit?        :initform nil)
   (send?        :initarg :send?)
   (outdated?    :initform nil)
   (heading-face :initform 'git-appraise-recent-comment-heading)
   (body-face    :initform nil)
   (diffHunk     :initform nil)
   (line-type    :initarg :line-type)
   (render-extra-newline?    :initform nil                :allocation :class)))
(defclass git-appraise-reply-comment-section (git-appraise-base-comment-section)
  ((keymap       :initform 'git-appraise-reply-comment-section-map)
   (resolve?     :initarg :resolve? :initform nil)
   (reply?       :initform t)
   (local?       :initform t)
   (edit?        :initform nil)
   (outdated?    :initform nil)
   (heading-face :initform 'git-appraise-recent-comment-heading)
   (body-face    :initform nil)
   (render-extra-newline?    :initform t               :allocation :class)))
(defclass git-appraise-outdated-comment-section (git-appraise-base-comment-section)
  ((keymap       :initform 'git-appraise-outdated-comment-section-map)
   (local?       :initform t)
   (outdated?    :initform t)))
(defclass git-appraise-check-section (magit-section)
  ((details :initarg :details)))
(defclass git-appraise-binary-file-section (magit-section)
  ((keymap :initform 'git-appraise-binary-file-section-map)))
(defvar git-appraise-code-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'git-appraise-comment-add-or-edit)
    map)
  "Keymaps for code-comment sections.")
(defvar git-appraise-local-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'git-appraise-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'git-appraise-section-delete-comment)
    (define-key map (kbd "C-c C-f") 'git-appraise-toggle-draft-fyi)
    map)
  "Keymaps for local-comment sections.")
(defvar git-appraise-reply-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'git-appraise-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'git-appraise-section-delete-comment)
    map)
  "Keymaps for reply-comment sections.")
(defvar git-appraise-outdated-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'git-appraise-comment-add-or-edit)
    map)
  "Keymaps for outdated-comment sections.")
(defvar git-appraise-binary-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'git-appraise-visit-binary-file)
    (define-key map [mouse-2] 'git-appraise-visit-binary-file)
    (define-key map [follow-link] 'git-appraise-visit-binary-file)
    map)
  "Keymaps for binary files sections.")

(defcustom git-appraise-inline-comment-border-style 'horizontal
  "Border style for comments shown inside diffs.
`horizontal' draws a thin rule above the heading and below the body.
Nil leaves the existing comment background and heading faces alone."
  :group 'git-appraise
  :type '(choice (const :tag "Top and bottom rules" horizontal)
                 (const :tag "No rules" nil)))

(defcustom git-appraise-inline-comment-border-color nil
  "Color of inline comment rules, or nil to use the theme's shadow color."
  :group 'git-appraise
  :type '(choice (const :tag "Theme shadow color" nil)
                 (color :tag "Custom color")))

(defun git-appraise-section--comment-border-line (position edge color)
  "Draw a horizontal COLOR rule at POSITION on EDGE of its line."
  (save-excursion
    (goto-char position)
    (let* ((start (line-beginning-position))
           (end (min (point-max) (1+ (line-end-position))))
           (ov (make-overlay start end)))
      (overlay-put ov 'face
                   (if (eq edge 'top)
                       `(:overline ,color :extend t)
                     `(:underline ,color :extend t)))
      (overlay-put ov 'priority 110)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'git-appraise-comment-border edge))))

(defun git-appraise-section--decorate-inline-comment (body-start body-end)
  "Add rules above and below the diff comment body from BODY-START to BODY-END."
  (when (eq git-appraise-inline-comment-border-style 'horizontal)
    (let* ((theme-color (face-foreground 'shadow nil t))
           (color (or git-appraise-inline-comment-border-color
                      (and (stringp theme-color) theme-color)
                      "gray50")))
      (save-excursion
        (goto-char body-start)
        (forward-line -1)
        (git-appraise-section--comment-border-line (point) 'top color)
        (goto-char body-end)
        (when (and (> (point) body-start) (bolp))
          (forward-line -1))
        (while (and (> (point) body-start)
                    (looking-at-p "[ \t]*$"))
          (forward-line -1))
        (git-appraise-section--comment-border-line (point) 'bottom color)))))

(cl-defmethod git-appraise-insert-comment-lines ((obj git-appraise-comment-section))
  "Insert the comment lines given in the OBJ with colored background."
  (let ((start (point))
        (face (git-appraise-section--comment-bg-face obj)))
    (git-appraise--insert-html (oref obj msg) (* 3 git-appraise-section-indent-width))
    (unless (bolp)
      (insert ?\n))
    (let ((ov (make-overlay start (point))))
      (overlay-put ov 'face face)
      (overlay-put ov 'priority 100))))
(cl-defgeneric git-appraise-comment-insert-lines (obj)
  "Insert comment lines in the code section based on section type denoted by OBJ.")
(cl-defmethod git-appraise-comment-insert-lines ((obj git-appraise-local-comment-section))
  "Insert local comment lines present in the OBJ."
  (git-appraise-comment--insert-you-lines obj 'git-appraise-local-comment-section))
(cl-defmethod git-appraise-comment-insert-lines ((obj git-appraise-reply-comment-section))
  "Insert reply comment lines present in the OBJ."
  (git-appraise-comment--insert-you-lines obj 'git-appraise-reply-comment-section))
(defun git-appraise-comment--insert-you-lines (obj type)
  "Insert the local or reply comment OBJ as a section of TYPE.
The trailing blank line comes from `render-extra-newline?'."
  (magit-insert-section ((eval type) obj)
    (let ((heading (git-appraise-comment-format-heading
                    (if (eq type 'git-appraise-reply-comment-section)
                        "Reply" "Comment")
                    "YOU"
                    (and (slot-boundp obj 'createdAt) (oref obj createdAt))
                    (append
                     (list (if (equal (oref obj state) "SENT")
                               "SENT" "DRAFT"))
                     (when (and (eq type 'git-appraise-local-comment-section)
                                (oref obj fyi?))
                       '("FYI"))
                     (when (and (eq type 'git-appraise-reply-comment-section)
                                (oref obj resolve?))
                       '("RESOLVE"))))))
      (add-face-text-property 0 (length heading) (oref obj heading-face) t heading)
      (magit-insert-heading heading))
    (magit-insert-section ((eval type) obj)
      (let* ((start (point))
             (msg (oref obj msg)))
        (dolist (l (git-appraise-utils--split-comment
                    (git-appraise-utils--wrap-text
                     msg
                     git-appraise-fill-column)))
          (insert l)
          (insert ?\n))
        (when (oref obj render-extra-newline?)
          (insert ?\n))
        (let ((ov (make-overlay start (point))))
          (overlay-put ov 'face 'git-appraise-comment-self-bg)
          (overlay-put ov 'priority 1))
        (git-appraise-section--decorate-inline-comment start (point))))))
(defmacro git-appraise-section--insert-comment-heading-and-body
    (obj section-type bgface heading &optional heading-face
         heading-ov-priority body-ov-priority)
  "Insert comment OBJ as SECTION-TYPE with BGFACE background.
HEADING is the heading string; HEADING-FACE, when non-nil, is applied
before insertion.  HEADING-OV-PRIORITY and BODY-OV-PRIORITY default to 1
and 100."
  (let ((ov (make-symbol "ov"))
        (start (make-symbol "start"))
        (bol (make-symbol "bol"))
        (eol (make-symbol "eol")))
    `(progn
       (when ,heading-face
         (add-face-text-property 0 (length ,heading) ,heading-face t ,heading))
       (magit-insert-heading ,heading)
       (save-excursion
         (forward-line -1)
         (let* ((,bol (point))
                (,eol (line-end-position))
                (,ov (make-overlay ,bol (1+ ,eol))))
           (overlay-put ,ov 'face ,bgface)
           (overlay-put ,ov 'priority ,(or heading-ov-priority 1))))
       (magit-insert-section (,section-type ,obj)
         (let ((,start (point)))
           (git-appraise--insert-html
            (oref ,obj msg)
            (* 3 git-appraise-section-indent-width))
           (unless (bolp)
             (insert ?\n))
           (let ((,ov (make-overlay ,start (point))))
             (overlay-put ,ov 'face ,bgface)
             (overlay-put ,ov 'priority ,(or body-ov-priority 100)))
           (git-appraise-section--decorate-inline-comment ,start (point)))))))

(cl-defmethod git-appraise-comment-insert-lines (obj)
  "Default insert comment lines in the OBJ."
  (magit-insert-section (git-appraise-code-comment-section obj)
    (let ((bgface (git-appraise-section--comment-bg-face obj))
          (heading (git-appraise-comment-format-heading
                    "Comment" (oref obj author) (oref obj createdAt)
                    (append (unless (equal (oref obj state) "COMMENTED")
                              (list (oref obj state)))
                            (when (and (slot-boundp obj 'resolved?)
                                       (oref obj resolved?))
                              '("RESOLVED"))))))
      (git-appraise-section--insert-comment-heading-and-body
       obj 'git-appraise-code-comment-section bgface heading
       'git-appraise-recent-comment-heading 1 100))))
(defun git-appraise-section--comment-bg-face (c)
  "Background face for comment C: self, PR author, or other."
  (let* ((infos (git-appraise-db--pullreq-raw-infos))
         (pr-author (and infos (a-get-in infos '(author login))))
         (author (oref c author))
         (my-login (git-appraise-utils--git-get-user)))
    (cond
     ((and my-login author (string= my-login author))
      'git-appraise-comment-self-bg)
     ((and pr-author author (string= pr-author author))
      'git-appraise-comment-author-bg)
     (t 'git-appraise-comment-other-bg))))
(defun git-appraise-section--insert-outdated-comment-body (c)
  "Insert one outdated comment C: the nested heading and body
sections with their background overlays."
  (let ((bgface (git-appraise-section--comment-bg-face c))
        (heading (git-appraise-comment-format-heading
                  "Comment" (oref c author)
                  (and (slot-boundp c 'createdAt) (oref c createdAt))
                  (append '("OUTDATED")
                          (unless (equal (oref c state) "COMMENTED")
                            (list (oref c state)))
                          (when (and (slot-boundp c 'resolved?)
                                     (oref c resolved?))
                            '("RESOLVED"))))))
    (git-appraise-section--insert-comment-heading-and-body
     c 'git-appraise-outdated-comment-section bgface heading
     nil 100 100)))
(defun git-appraise-section--insert-outdated-hunk (hunk metadata hunk-comments
                                                  amount-loc-cell)
  "Insert one outdated HUNK (METADATA its section value) and its
COMMENT sections.  AMOUNT-LOC-CELL is a list cell holding the
running internal amount-loc, advanced here by the hunk text and
each comment body (list cell: the render helpers cannot return
from inside `magit-insert-section' bodies)."
  (let ((first-path (oref (-first-item hunk-comments) path)))
    (git-appraise-section--hide-if-hidden
     (magit-insert-section (git-appraise-outdated-hunk-section metadata t)
       (let ((heading (format "Reviewed - [OUTDATED]")))
         (add-face-text-property 0 (length heading)
                                 'git-appraise-outdated-comment-heading
                                 t heading)
         (magit-insert-heading heading))
       (magit-insert-section ()
         (save-excursion
           (insert hunk))
         (git-appraise-wash-hunk)
         (insert ?\n)
         (dolist (c hunk-comments)
           (let* ((written-loc (git-appraise--html-written-loc
                                (oref c msg)
                                (* 3 git-appraise-section-indent-width)))
                  (amount-new-loc-outdated-partial (+ 1 written-loc))
                  (amount-new-loc-outdated (if (oref c reactions)
                                               (+ 2 amount-new-loc-outdated-partial)
                                             amount-new-loc-outdated-partial)))
             (setcar amount-loc-cell
                     (+ (car amount-loc-cell) amount-new-loc-outdated))
             (setq git-appraise-section-hold-written-comment-count
                   (git-appraise-utils--comment-update-written-count
                    git-appraise-section-hold-written-comment-count
                    first-path
                    amount-new-loc-outdated))
             (oset c amount-loc (car amount-loc-cell))
             (git-appraise-section--insert-outdated-comment-body c))))))))
(defun git-appraise-section-insert-outdated-comment (comments amount-loc)
  "Insert outdated COMMENTS in the buffer of PULLREQ-ID considering AMOUNT-LOC.
Safeguards against non-outdated/local comments accidentally passed in."
  ;; Only consider true outdated diff comments that have a diff hunk.
  (let* ((outdated-comments
          (-filter (lambda (el)
                     (and (ignore-errors (oref el outdated?))
                          (oref el outdated?)
                          (ignore-errors (slot-exists-p el 'diffHunk))
                          (oref el diffHunk)))
                   comments)))
    (when outdated-comments
      ;; Hunk groups are necessary because we usually have multiple
      ;; reviews about the same original position across different
      ;; commit snapshots; add those hunks and
      ;; its comments.
      (let* ((hunk-groups (-group-by (lambda (el) (oref el diffHunk))
                                    outdated-comments))
             (hunks (a-keys hunk-groups))
             ;; list cell threading the running internal amount-loc
             ;; through the render helper below
             (amount-loc-cell (list amount-loc)))
        (dolist (hunk hunks)
          (when (not hunk)
            (git-appraise-utils--log
             "git-appraise-section-insert-outdated-comment"
             (format "Every outdated comment must have a hunk! Error found for %S"
                     (prin1-to-string hunk)))
            (message "Hunk empty found. A empty string will be used instead. Report this bug please."))
          (let* ((safe-hunk (or hunk ""))
                 (diff-hunk-lines (split-string safe-hunk "\n"))
                 (amount-new-loc (+ 1 (length diff-hunk-lines)))
                 (hunk-comments (alist-get safe-hunk hunk-groups nil nil 'equal))
                 (first-hunk-commit (-first-item hunk-comments))
                 (metadata1 `((comment . ,first-hunk-commit)
                              (amount-loc . ,(+ (car amount-loc-cell) amount-new-loc)))))
            (setcar amount-loc-cell (+ (car amount-loc-cell) amount-new-loc))
            (setq git-appraise-section-hold-written-comment-count
                  (git-appraise-utils--comment-update-written-count
                   git-appraise-section-hold-written-comment-count
                   (oref first-hunk-commit path)
                   amount-new-loc))
            (git-appraise-section--insert-outdated-hunk
             safe-hunk metadata1 hunk-comments amount-loc-cell)))))))
(defun git-appraise-section-insert-outdated-comment-missing (path-name missing-paths grouped-comments)
  "Write missing outdated comments in the end of the current path.
We need PATH-NAME, MISSING-PATHS, and GROUPED-COMMENTS to make this work.
Unlike inline insertion, point is already AFTER the file's hunks.
Confine insertion to an empty region so the comment inserter's
`forward-line' and any outdated-hunk wash cannot consume the next file."
  (save-restriction
    (narrow-to-region (point) (point))
    (dolist (path-pos missing-paths)
      (let* ((comment-written-pos
              (or (alist-get path-name git-appraise-section-hold-written-comment-count nil nil 'equal)
                  0))
             (comments (git-appraise-utils--comment-get grouped-comments path-pos))
             (first (car comments)))
        (when comments
          (if (and (ignore-errors (oref first outdated?))
                   (oref first outdated?))
              (git-appraise-section-insert-outdated-comment comments comment-written-pos)
            (git-appraise-section-insert-comment comments comment-written-pos)))
        (push path-pos git-appraise-section-hold-written-comment-ids)))))
(defun git-appraise-section-insert-comment (comments amount-loc)
  "Insert COMMENTS to PULLREQ-ID keep the AMOUNT-LOC of comments written.
A quite good assumption: every comment in an outdated hunk will be outdated."
  (if (and (oref (-first-item comments) outdated?)
           git-appraise-section--display-diff-comments)
      (git-appraise-section-insert-outdated-comment
       comments
       amount-loc)
    (let ((new-amount-loc amount-loc))
      (forward-line)
      (dolist (c comments)
        (when (or (and (git-appraise-local-comment-section-p c)
                       git-appraise-section--display-diff-comments)
                  (and (git-appraise-reply-comment-section-p c)
                       git-appraise-section--display-diff-comments)
                  (and (git-appraise-code-comment-section-p c)
                       git-appraise-section--display-diff-comments))
          (let* ((written-loc
                  (if (or (git-appraise-local-comment-section-p c)
                          (git-appraise-reply-comment-section-p c))
                      (let ((body-lines
                             (length
                              (git-appraise-utils--split-comment
                               (git-appraise-utils--wrap-text
                                (oref c msg)
                                git-appraise-fill-column)))))
                        (+ body-lines (if (oref c render-extra-newline?) 1 0)))
                    (git-appraise--html-written-loc
                     (oref c msg)
                     (* 3 git-appraise-section-indent-width))))
                 (amount-loc-incr-partial (+ 1 written-loc))
                 (amount-loc-incr (if (oref c reactions)
                                      (+ 2 amount-loc-incr-partial)
                                    amount-loc-incr-partial)))
            (setq new-amount-loc (+ new-amount-loc amount-loc-incr))
            (oset c amount-loc new-amount-loc)

            (setq git-appraise-section-hold-written-comment-count
                  (git-appraise-utils--comment-update-written-count
                   git-appraise-section-hold-written-comment-count
                   (oref c path)
                   amount-loc-incr))
            (git-appraise-comment-insert-lines c)))))))
(defun git-appraise--section-author (section)
  "Return the author string of SECTION, or nil.
Comment sections are created by passing a prebuilt object to
`magit-insert-section', which stores it in the `value' slot, so
the author may live either in the section itself or in its
value object.  Sections of classes with no `author' slot (most
of them) safely return nil instead of signaling
`invalid-slot-name'."
  (or (and (slot-exists-p section 'author)
           (slot-boundp section 'author)
           (let ((author (oref section author)))
             (and (stringp author) author)))
      (let ((val (and (slot-boundp section 'value)
                      (oref section value))))
        (and (eieio-object-p val)
             (slot-exists-p val 'author)
             (slot-boundp val 'author)
             (let ((author (oref val author)))
               (and (stringp author) author))))))
(defun git-appraise--bot-comment-p (section)
  "Non-nil when SECTION is a comment authored by a bot."
  (let ((author (git-appraise--section-author section)))
    (and author
         (string-match-p git-appraise-bot-author-regexp
                         (downcase author))
         t)))
(defun git-appraise--section-tree-any (section pred)
  "Non-nil when PRED holds for SECTION or one of its descendants."
  (or (funcall pred section)
      (let ((found nil))
        (dolist (c (oref section children))
          (unless found
            (setq found (git-appraise--section-tree-any c pred))))
        found)))
(defun git-appraise--collapse-bot-comments (section)
  "Fold bot-authored comment sections under SECTION.
In plain terms: reviews of AI-generated PRs are full of bots
commenting on each other.  Those comments are collapsed to their
one-line \"@author - date\" heading, so what's left reads like a
human conversation.  Threads where a human replied stay expanded,
and TAB unfolds any of them."
  (if (and (git-appraise--bot-comment-p section)
           (not (git-appraise--section-tree-any
                 section
                 (lambda (s)
                   (let ((author (git-appraise--section-author s)))
                     (and author
                          (not (git-appraise--bot-comment-p s))))))))
      (magit-section-hide section)
    (dolist (c (oref section children))
      (git-appraise--collapse-bot-comments c))))


;;; Phase 9: fringe markers linking diff lines to their threads

(defvar git-appraise--fringe-bitmap-defined nil
  "Non-nil after the `git-appraise-comment-marker' bitmap was registered.")
(defun git-appraise--ensure-fringe-bitmap ()
  "Register the thread marker fringe bitmap once (no-op on text frames)."
  (when (and (display-graphic-p)
             (fboundp 'define-fringe-bitmap)
             (not git-appraise--fringe-bitmap-defined))
    (define-fringe-bitmap 'git-appraise-comment-marker
      (vector 0 128 192 224 224 192 128 0))
    (setq git-appraise--fringe-bitmap-defined t)))
(defvar git-appraise-fringe-marker-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'git-appraise-fringe-jump-to-thread)
    map)
  "Keymap for thread-marker lines.
A sparse keymap: only mouse-1 is bound (jump to the thread); any
other key falls through to the buffer's own maps untouched.")
(defun git-appraise--comment-anchor-position (section)
  "Buffer position (BOL) of the diff line SECTION is anchored to.
The wash inserts each thread right AFTER its anchor line, so scan
backwards from the section start, inside the hunk, until a patch
line is found.  nil when SECTION does not sit inside a hunk."
  (when (and (slot-boundp section 'parent)
             (magit-hunk-section-p (oref section parent)))
    (let ((bound (oref (oref section parent) start)))
      (save-excursion
        (goto-char (oref section start))
        (catch 'found
          (while (> (point) bound)
            (forward-line -1)
            (when (git-appraise--patch-line-p)
              (throw 'found (line-beginning-position))))
          nil)))))
(defun git-appraise--mark-comment-line (section)
  "Lay the clickable fringe marker on SECTION's anchor diff line.
Idempotent: any previous marker on that line is removed first."
  (let ((bol (git-appraise--comment-anchor-position section)))
    (when bol
      (remove-overlays bol (1+ bol) 'cr-comment-marker t)
      (let ((ov (make-overlay bol (min (1+ bol) (point-max)))))
        (overlay-put ov 'cr-comment-marker t)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'cr-thread-start (copy-marker (oref section start)))
        (overlay-put ov 'before-string
                     (propertize
                      " " 'display
                      (list 'left-fringe 'git-appraise-comment-marker
                            'git-appraise-fringe-comment-face)
                      'keymap git-appraise-fringe-marker-keymap
                      'help-echo "Review thread on this line (mouse-1: jump)"))))))
(defun git-appraise--mark-comment-lines ()
  "Fringe-mark every review thread anchored inside a hunk.
Sweeps stale marker overlays first (a re-render erases the buffer,
but the walk below is cheap enough to stay idempotent anyway).
Runs from `git-appraise--trigger-hooks' after every render."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'cr-comment-marker)
      (delete-overlay ov)))
  (git-appraise--ensure-fringe-bitmap)
  (magit-map-sections
   (lambda (section)
     (when (cl-typep section 'git-appraise-base-comment-section)
       (git-appraise--mark-comment-line section)))))
;; Remove stale bindings from maps shared with existing review buffers.
(--each '((git-appraise-comment-section-map "e" "C-c C-r" "C-c C-i")
          (git-appraise-code-comment-section-map "e" "C-c C-r" "C-c C-n" "K")
          (git-appraise-local-comment-section-map "K")
          (git-appraise-reply-comment-section-map "K")
          (git-appraise-outdated-comment-section-map "e" "K")
          (git-appraise-binary-file-section-map "<mouse-3>" "C-c C-v"))
  (let ((map (symbol-value (car it))))
    (--each (cdr it)
      (define-key map (kbd it) nil))))
(define-key git-appraise-binary-file-section-map (kbd "RET") #'git-appraise-visit-binary-file)
(define-key git-appraise-code-comment-section-map (kbd "C-c C-r")
  #'git-appraise-resolve-comment-at-point)
(define-key git-appraise-comment-section-map (kbd "C-c C-r")
  #'git-appraise-resolve-comment-at-point)
(define-key git-appraise-outdated-comment-section-map (kbd "C-c C-r")
  #'git-appraise-resolve-comment-at-point)
(define-key git-appraise-local-comment-section-map (kbd "C-c C-f")
  #'git-appraise-toggle-draft-fyi)
(define-key git-appraise-binary-file-section-map [mouse-2] #'git-appraise-visit-binary-file)
(define-key git-appraise-binary-file-section-map [follow-link] #'git-appraise-visit-binary-file)

(provide 'git-appraise-section-comment)
;;; git-appraise-section-comment ends here
