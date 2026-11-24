;;; git-appraise-lazy-test.el --- ERT tests for lazy diff washing -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for lazy diff washing behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise-lazy)

;;; req-2026-11-23-011: lazy wash preserves point
;; covers: git-appraise-lazy.el
;; covers-fn: git-appraise-lazy--wash-step
;; enforcement: test
(ert-deftest git-appraise-lazy/req-2026-11-23-011-wash-step-preserves-point ()
  "WHEN lazy wash processes a diff block
THE SYSTEM SHALL preserve the user's point in the review buffer."
  (let ((buf (generate-new-buffer " *git-appraise-lazy-test*")))
    (unwind-protect
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "line1\nline2\nline3\nline4\nline5\n")
            (goto-char (point-min))
            (forward-line 1))
          (let ((saved (point))
                (epoch (cons 'git-appraise-lazy t)))
            (setq-local git-appraise-lazy--epoch epoch
                        git-appraise-lazy--queue nil
                        git-appraise-lazy--wash-pos (copy-marker (point-min))
                        git-appraise-lazy--wash-limit (copy-marker (point-max))
                        git-appraise-lazy--insert-parent nil
                        git-appraise-lazy--end-fixups nil)
            (cl-letf (((symbol-function 'git-appraise-lazy--wash-block)
                       (lambda ()
                         (goto-char (point-max))
                         (set-marker git-appraise-lazy--wash-pos (point))
                         t)))
              (git-appraise-lazy--wash-step buf epoch)
              (should (= (point) saved)))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;; req-2026-11-24-004: concurrent reviews retain their own comment context
;; covers: git-appraise-section.el git-appraise-section-shared.el git-appraise-lazy.el
;; covers-fn: git-appraise-lazy--wash-step
;; enforcement: test
(ert-deftest git-appraise-lazy/req-2026-11-24-004-isolate-review-context ()
  "WHEN two reviews render in turn THE SYSTEM SHALL keep each review's comments attached to its own diff."
  (let ((first (generate-new-buffer " *git-appraise-first-review*"))
        (second (generate-new-buffer " *git-appraise-second-review*"))
        seen)
    (unwind-protect
        (progn
          (--each `((,first first) (,second second))
            (with-current-buffer (car it)
              (insert "diff block\n")
              (setq-local git-appraise-lazy--epoch (cons 'epoch nil)
                          git-appraise-lazy--review-id (cadr it)
                          git-appraise-section-grouped-comments (list (cadr it))
                          git-appraise-lazy--wash-pos (copy-marker (point-min))
                          git-appraise-lazy--wash-limit (copy-marker (point-max)))))
          (let ((git-appraise-db--pullreq-id 'second))
            (cl-letf (((symbol-function 'git-appraise-lazy--wash-block)
                       (lambda ()
                         (push (list git-appraise-db--pullreq-id
                                     git-appraise-section-grouped-comments)
                               seen)
                         (goto-char (point-max))
                         t)))
              (git-appraise-lazy--wash-step
               first (buffer-local-value 'git-appraise-lazy--epoch first))
              (git-appraise-lazy--wash-step
               second (buffer-local-value 'git-appraise-lazy--epoch second))))
          (should (equal (nreverse seen)
                         '((first (first)) (second (second))))))
      (--each (list first second)
        (when (buffer-live-p it)
          (with-current-buffer it
            (git-appraise-lazy--invalidate))
          (kill-buffer it))))))

;;; req-2026-11-24-005: background render returns to its own review position
;; covers: git-appraise-lazy.el
;; covers-fn: git-appraise-lazy--expand-step
;; enforcement: test
(ert-deftest git-appraise-lazy/req-2026-11-24-005-restore-own-point ()
  "WHEN a background review finishes THE SYSTEM SHALL return to that review's prior reading position."
  (let ((buf (generate-new-buffer " *git-appraise-point-review*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "first\nsecond\n")
          (setq-local git-appraise-lazy--epoch (cons 'epoch nil)
                      git-appraise-lazy--review-id 'review
                      git-appraise-lazy--pre-render-point (point-min)
                      git-appraise-lazy--queue nil)
          (goto-char (point-max))
          (let ((git-appraise-comment-cursor-pos (point-max)))
            (git-appraise-lazy--expand-step buf git-appraise-lazy--epoch))
          (should (= (point) (point-min))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(provide 'git-appraise-lazy-test)
;;; git-appraise-lazy-test.el ends here
