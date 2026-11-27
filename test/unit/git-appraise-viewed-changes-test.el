;;; git-appraise-viewed-changes-test.el --- viewed diff comparison tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'git-appraise-viewed-changes)

;;; req-2026-11-26-006: compare the saved and current patch by line
;; covers: git-appraise-viewed-changes.el
;; covers-fn: git-appraise-viewed-changes--line-numbers
;; enforcement: test
(ert-deftest viewed-changes/req-2026-11-26-006-current-line-delta ()
  "WHEN a viewed patch changes THE SYSTEM SHALL identify only its current changed lines."
  (should (equal '(2) (git-appraise-viewed-changes--line-numbers
                       "a\nb\nc\n" "a\nB\nc\n")))
  (should-not (git-appraise-viewed-changes--line-numbers
               "a\nb\nc\n" "a\nb\nc\n"))
  (should (git-appraise-viewed-changes--same-content-p
           "example.txt"
           "@@ -1,2 +1,2 @@\n a\n-b\n+B\n"
           "@@ -5,2 +5,2 @@\n a\n-b\n+B\n"))
  (should (equal '(2)
                 (git-appraise-viewed-changes--patch-ordinals
                  "diff --git a/x b/x\n@@ -1 +1 @@\n+foo\n+foo\n"
                  '(4)))))

;;; req-2026-11-27-008: margin cleanup during redisplay
;; covers: git-appraise-viewed-changes.el
;; covers-fn: git-appraise-viewed-changes--restore-window-margins
;; enforcement: test
(ert-deftest viewed-changes/req-2026-11-27-008-restore-window-margins ()
  "WHEN a review window stops showing a review THE SYSTEM SHALL restore its prior margins without a redisplay error."
  (let* ((window (selected-window))
         (old (window-margins window)))
    (unwind-protect
        (progn
          (set-window-parameter window 'git-appraise-viewed-old-margins old)
          (set-window-margins window 1 (cdr old))
          (git-appraise-viewed-changes--restore-window-margins)
          (should (equal old (window-margins window)))
          (should-not (window-parameter
                       window 'git-appraise-viewed-old-margins)))
      (set-window-margins window (car old) (cdr old))
      (set-window-parameter window 'git-appraise-viewed-old-margins nil))))

;;; git-appraise-viewed-changes-test.el ends here
