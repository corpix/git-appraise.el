;;; git-appraise-badges.el --- Compact review status badges -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'git-appraise-faces)
(declare-function svg-lib-tag "svg-lib")

(defun git-appraise-badge--label (status)
  "Return the short display label for STATUS."
  (pcase (upcase (replace-regexp-in-string "_" " " (or status "COMMENT")))
    ((or "APPROVED" "LGTM") "LGTM")
    ((or "CHANGES REQUESTED" "REQUEST CHANGES" "REJECTED" "NEEDS WORK")
     "NEEDS WORK")
    ("COMMENTED" "COMMENT")
    (other other)))

(defun git-appraise-badge--face (label)
  "Return the badge face for LABEL."
  (pcase label
    ((or "LGTM" "RESOLVED") 'git-appraise-badge-success-face)
    ((or "NEEDS WORK" "DRAFT") 'git-appraise-badge-warning-face)
    ((or "EDITED" "OUTDATED") 'git-appraise-badge-muted-face)
    (_ 'git-appraise-badge-neutral-face)))

(defun git-appraise-badge--image (label face)
  "Return a graphical badge for LABEL and FACE, or nil."
  (when (and (display-graphic-p)
             (image-type-available-p 'svg)
             (require 'svg-lib nil t))
    (condition-case nil
        (let* ((accent (face-foreground face nil t))
               (background (face-background 'default nil t)))
          (when (and (stringp accent) (stringp background))
            (svg-lib-tag label nil :padding 1 :radius 2 :font-size 12
                         :foreground background :background accent)))
      (error nil))))

(defun git-appraise-badge (status)
  "Return STATUS as an SVG badge with a readable text fallback.
The text is retained beneath the conditional image display, so a
buffer rendered in a graphical frame is also readable in a terminal."
  (let* ((label (git-appraise-badge--label status))
         (face (git-appraise-badge--face label))
         (fallback (propertize (format "[%s]" label)
                               'font-lock-face face))
         (image (git-appraise-badge--image label face)))
    (if image
        (propertize fallback 'display `(when (display-graphic-p) . ,image))
      fallback)))

(provide 'git-appraise-badges)
;;; git-appraise-badges.el ends here
