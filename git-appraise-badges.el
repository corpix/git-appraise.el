;;; git-appraise-badges.el --- Compact review status badges -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'git-appraise-faces)
(declare-function svg-lib-tag "svg-lib")

(defcustom git-appraise-badge-styles
  '(("APPROVED" :label "LGTM" :face git-appraise-badge-success-face)
    ("LGTM" :label "LGTM" :face git-appraise-badge-success-face)
    ("RESOLVED" :label "RESOLVED" :face git-appraise-badge-success-face)
    ("CHANGES REQUESTED" :label "NEEDS WORK" :face git-appraise-badge-warning-face)
    ("REQUEST CHANGES" :label "NEEDS WORK" :face git-appraise-badge-warning-face)
    ("REJECTED" :label "NEEDS WORK" :face git-appraise-badge-warning-face)
    ("NEEDS WORK" :label "NEEDS WORK" :face git-appraise-badge-warning-face)
    ("DRAFT" :label "DRAFT" :face git-appraise-badge-warning-face)
    ("EDITED" :label "EDITED" :face git-appraise-badge-muted-face)
    ("OUTDATED" :label "OUTDATED" :face git-appraise-badge-muted-face)
    ("COMMENTED" :label "COMMENT" :face git-appraise-badge-neutral-face))
  "Map native review statuses to badge labels and faces.
Each entry is (STATUS :label STRING :face FACE).  Unknown statuses
use their native text and `git-appraise-badge-neutral-face'."
  :group 'git-appraise :type 'sexp)

(defcustom git-appraise-badge-style 'auto
  "Badge display style.
`auto' generates SVG in graphical frames, `svg' prepares it even
when rendering in a terminal, and `text' always uses text.  SVG
requests fall back to readable text when unsupported."
  :group 'git-appraise
  :type '(choice (const auto) (const svg) (const text)))
(defcustom git-appraise-badge-svg-padding 1
  "Padding passed to svg-lib for graphical review badges."
  :group 'git-appraise :type 'natnum)
(defcustom git-appraise-badge-svg-radius 2
  "Corner radius passed to svg-lib for graphical review badges."
  :group 'git-appraise :type 'natnum)
(defcustom git-appraise-badge-svg-font-size 12
  "Font size passed to svg-lib for graphical review badges."
  :group 'git-appraise :type 'natnum)

(defun git-appraise-badge--key (status)
  "Normalize STATUS for lookup in `git-appraise-badge-styles'."
  (upcase (replace-regexp-in-string "_" " " (or status "COMMENT"))))

(defun git-appraise-badge--label (status)
  "Return the short display label for STATUS."
  (let* ((key (git-appraise-badge--key status))
         (style (cdr (assoc key git-appraise-badge-styles))))
    (or (plist-get style :label) key)))

(defun git-appraise-badge--face (status)
  "Return the configured badge face for STATUS."
  (or (plist-get (cdr (assoc (git-appraise-badge--key status)
                              git-appraise-badge-styles)) :face)
      'git-appraise-badge-neutral-face))

(defun git-appraise-badge--image (label face)
  "Return a graphical badge for LABEL and FACE, or nil."
  (when (and (or (eq git-appraise-badge-style 'svg)
                 (and (eq git-appraise-badge-style 'auto)
                      (display-graphic-p)))
             (image-type-available-p 'svg)
             (require 'svg-lib nil t))
    (condition-case nil
        (let* ((accent (face-foreground face nil t))
               (background (face-background 'default nil t)))
          (when (and (stringp accent) (stringp background))
            (svg-lib-tag label nil
                         :padding git-appraise-badge-svg-padding
                         :radius git-appraise-badge-svg-radius
                         :font-size git-appraise-badge-svg-font-size
                         :foreground background :background accent)))
      (error nil))))

(defun git-appraise-badge (status)
  "Return STATUS as an SVG badge with a readable text fallback.
The text is retained beneath the conditional image display, so a
buffer rendered in a graphical frame is also readable in a terminal."
  (let* ((label (git-appraise-badge--label status))
         (face (git-appraise-badge--face status))
         (fallback (propertize (format "[%s]" label)
                               'font-lock-face face))
         (image (git-appraise-badge--image label face)))
    (if image
        (propertize fallback 'display `(when (display-graphic-p) . ,image))
      fallback)))

(provide 'git-appraise-badges)
;;; git-appraise-badges.el ends here
