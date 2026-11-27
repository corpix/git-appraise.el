;;; git-appraise-viewed-performance-test.el --- Bulk viewed work bounds -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'git-appraise-test-viewed-bulk)
(require 'git-appraise-store)
(require 'git-appraise-folding)

;;; req-2026-11-27-001: large region viewed actions remain responsive
;; covers: git-appraise-viewed.el git-appraise-folding.el
;; covers-fn: git-appraise-viewed--write-bulk-events, git-appraise-viewed-mark-and-fold
;; enforcement: test
(ert-deftest viewed-performance/req-2026-11-27-001-bulk-viewed-work-bounds ()
  "WHEN a reviewer toggles 200 selected files THE SYSTEM SHALL update their viewed state with bounded full-review work."
  (git-appraise-test-viewed-bulk--with-review 200
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let ((split-original (symbol-function 'git-appraise--diff--split-by-files))
            (store-original (symbol-function 'git-appraise-store-append-events))
            (project-original (symbol-function 'git-appraise-viewed--reproject))
            (apply-original (symbol-function 'git-appraise-viewed--apply-in-place))
            (splits 0) (writes 0) (projections 0) (visual-updates 0))
        (cl-letf (((symbol-function 'git-appraise--diff--split-by-files)
                   (lambda (&rest args)
                     (cl-incf splits)
                     (apply split-original args)))
                  ((symbol-function 'git-appraise-store-append-events)
                   (lambda (&rest args)
                     (cl-incf writes)
                     (apply store-original args)))
                  ((symbol-function 'git-appraise-viewed--reproject)
                   (lambda (&rest args)
                     (cl-incf projections)
                     (apply project-original args)))
                  ((symbol-function 'git-appraise-viewed--apply-in-place)
                   (lambda (&rest args)
                     (cl-incf visual-updates)
                     (apply apply-original args))))
          (git-appraise-test-viewed-bulk--press-v)
          (should (<= splits 2))
          (should (= writes 1))
          (should (= projections 1))
          (should (= visual-updates 1))
          (should (= 200 (length (git-appraise-store-events
                                   directory revision 'viewed))))
          (should (string-match-p "200/200 viewed" (buffer-string)))
          (setq splits 0 writes 0 projections 0 visual-updates 0)
          (git-appraise-test-viewed-bulk--press-v)
          (should (<= splits 1))
          (should (= writes 1))
          (should (= projections 1))
          (should (= visual-updates 1))
          (should (string-match-p "0/200 viewed" (buffer-string))))))))

;;; git-appraise-viewed-performance-test.el ends here
