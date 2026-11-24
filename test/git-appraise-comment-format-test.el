;;; git-appraise-comment-format-test.el --- Comment display tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify comment dates and headings use one display convention.

;;; Code:

(require 'ert)
(require 'git-appraise-comment-format)

;;; req-2026-11-24-009: comment times keep only relevant date fields
;; covers: git-appraise-comment-format.el
;; covers-fn: git-appraise-comment-format-time
;; enforcement: test
(ert-deftest git-appraise-comment-format/req-2026-11-24-009-contextual-time ()
  "WHEN a reviewer reads comments from different dates THE SYSTEM SHALL show only the date fields needed to identify when each was written."
  (let ((now (encode-time 0 30 12 24 11 2026)))
    (should (equal "04:20"
                   (git-appraise-comment-format-time
                    (encode-time 0 20 4 24 11 2026) now)))
    (should (equal "25 Jul 04:20"
                   (git-appraise-comment-format-time
                    (encode-time 0 20 4 25 7 2026) now)))
    (should (equal "25 Jul 2025 04:20"
                   (git-appraise-comment-format-time
                    (encode-time 0 20 4 25 7 2025) now)))))

;;; req-2026-11-24-010: comment types use a consistent heading
;; covers: git-appraise-comment-format.el git-appraise-section-comment.el
;; covers-fn: git-appraise-comment-format-heading
;; enforcement: test
(ert-deftest git-appraise-comment-format/req-2026-11-24-010-unified-headings ()
  "WHEN a reviewer reads comments and replies THE SYSTEM SHALL present authors, status, and time in a consistent order."
  (let* ((now (encode-time 0 30 12 24 11 2026))
         (timestamp (encode-time 0 20 4 24 11 2026)))
    (cl-letf (((symbol-function 'current-time) (lambda () now)))
      (should (equal "Comment by alice · RESOLVED · 04:20"
                     (substring-no-properties
                      (git-appraise-comment-format-heading
                       "Comment" "alice" timestamp '("RESOLVED")))))
      (should (equal "Reply by YOU · DRAFT · 04:20"
                     (substring-no-properties
                      (git-appraise-comment-format-heading
                       "Reply" "YOU" timestamp '("DRAFT"))))))))

;;; git-appraise-comment-format-test.el ends here
