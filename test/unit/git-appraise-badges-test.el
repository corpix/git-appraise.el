;;; git-appraise-badges-test.el --- Review badge display tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'git-appraise-badges)

;;; req-2026-11-27-008: badges stay readable with or without SVG
;; covers: git-appraise-badges.el git-appraise-faces.el
;; covers-fn: git-appraise-badge
;; enforcement: test
(ert-deftest badges/req-2026-11-27-008-svg-and-text-fallback ()
  "WHEN a reviewer reads status badges in a graphical or terminal frame THE SYSTEM SHALL show the same status in readable text."
  (let ((badge (git-appraise-badge "APPROVED")))
    (should (equal "[LGTM]" (substring-no-properties badge)))
    (should (eq 'git-appraise-badge-success-face
                (get-text-property 0 'font-lock-face badge))))
  (let ((original (symbol-function 'git-appraise-badge--image)))
    (unwind-protect
        (progn
          (fset 'git-appraise-badge--image (lambda (&rest _) 'fake-svg))
          (let* ((badge (git-appraise-badge "CHANGES_REQUESTED"))
                 (display (get-text-property 0 'display badge)))
            (should (equal "[NEEDS WORK]" (substring-no-properties badge)))
            (should (equal display
                           '(when (display-graphic-p) . fake-svg)))))
      (fset 'git-appraise-badge--image original))))

(ert-deftest badges/svg-helper-uses-svg-lib-when-available ()
  "Graphical badges pass the status label to svg-lib when it is available."
  (let* ((original-graphic (symbol-function 'display-graphic-p))
        (original-available (symbol-function 'image-type-available-p))
        (original-require (symbol-function 'require))
        (original-foreground (symbol-function 'face-foreground))
        (original-background (symbol-function 'face-background))
        (original-tag (and (fboundp 'svg-lib-tag)
                           (symbol-function 'svg-lib-tag))))
    (unwind-protect
        (progn
          (fset 'display-graphic-p (lambda (&rest _) t))
          (fset 'image-type-available-p (lambda (&rest _) t))
          (fset 'require (lambda (feature &rest args)
                          (if (eq feature 'svg-lib) t
                            (apply original-require feature args))))
          (fset 'face-foreground (lambda (&rest _) "#00aa00"))
          (fset 'face-background (lambda (&rest _) "#ffffff"))
          (fset 'svg-lib-tag (lambda (label &rest _)
                              (list :svg-label label)))
          (should (equal '(:svg-label "LGTM")
                         (git-appraise-badge--image
                          "LGTM" 'git-appraise-badge-success-face))))
      (fset 'display-graphic-p original-graphic)
      (fset 'image-type-available-p original-available)
      (fset 'require original-require)
      (fset 'face-foreground original-foreground)
      (fset 'face-background original-background)
      (if original-tag
          (fset 'svg-lib-tag original-tag)
        (fmakunbound 'svg-lib-tag)))))

;;; git-appraise-badges-test.el ends here
