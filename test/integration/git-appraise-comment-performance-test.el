;;; git-appraise-comment-performance-test.el --- Comment work bounds -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defmacro git-appraise-comment-performance-test--with-review (&rest body)
  "Run BODY with a rendered disposable review."
  (declare (indent 0))
  `(git-appraise-test--with-review-repo "git-appraise-comment-perf-"
     (let ((git-appraise-history-enabled nil))
       (git-appraise-backend-open revision)
       (let ((git-appraise-section-full-refresh? t))
         (git-appraise--build-buffer))
       (git-appraise-test--drain-lazy
        (get-buffer (git-appraise-pr-buffer-name))))
     ,@body))

;;; req-2026-11-25-048: file comments update without rebuilding the diff
;; covers: git-appraise-comment-draft.el git-appraise-comment.el
;; covers-fn: git-appraise-comment--refresh-conversation-in-place, git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest comment-performance/req-2026-11-25-048-file-send-is-targeted ()
  "WHEN a reviewer sends a file comment THE SYSTEM SHALL show it without rebuilding the review diff."
  (git-appraise-comment-performance-test--with-review
    (let ((buf (get-buffer (git-appraise-pr-buffer-name)))
          (builds 0)
          (comment (git-appraise-local-comment-section
                    :state "LOCAL COMMENT" :author "Tester"
                    :path "example.txt" :position 0 :line-type "FILE"
                    :send? t :msg "targeted file comment")))
      (with-current-buffer buf
        (setq git-appraise-comment-uncommitted comment
              git-appraise-comment-commit-buffer? nil)
        (cl-letf (((symbol-function 'git-appraise--build-buffer)
                   (lambda (&rest _) (cl-incf builds))))
          (git-appraise-comment-handler-commit
           comment git-appraise-comment-buffer-msg))
        (should (= builds 0))
        (should (string-match-p
                 "targeted[[:space:]\n]+file[[:space:]\n]+comment"
                 (buffer-string)))))))

;;; req-2026-11-25-049: diff positions reused across comment actions
;; covers: git-appraise-backend.el git-appraise-db.el
;; covers-fn: git-appraise-backend--line-for-comment, git-appraise-backend-refresh-comments
;; enforcement: test
(ert-deftest comment-performance/req-2026-11-25-049-reuse-diff-positions ()
  "WHEN a reviewer adds comments to the same review THE SYSTEM SHALL reuse the current diff mapping until the diff changes."
  (git-appraise-comment-performance-test--with-review
    (let* ((pr (git-appraise-db-get-pullreq))
           (comment (git-appraise-local-comment-section
                     :state "LOCAL COMMENT" :author "Tester"
                     :path "example.txt" :side "RIGHT" :line 2
                     :line-type "ADDED" :msg "mapped"))
           (calls 0)
           (parse (symbol-function 'git-appraise-backend--diff-tables)))
      (cl-letf (((symbol-function 'git-appraise-backend--diff-tables)
                 (lambda (diff)
                   (cl-incf calls)
                   (funcall parse diff))))
        (should (= 2 (git-appraise-backend--line-for-comment pr comment)))
        (should (eq 'ok (git-appraise-backend-refresh-comments pr)))
        (should (= 2 (git-appraise-backend--line-for-comment pr comment)))
        (should (= calls 1))))))

;;; req-2026-11-25-050: failed conversation update preserves review
;; covers: git-appraise-comment-draft.el
;; covers-fn: git-appraise-comment--refresh-conversation-in-place
;; enforcement: test
(ert-deftest comment-performance/req-2026-11-25-050-conversation-update-rollback ()
  "WHEN a comment display update fails THE SYSTEM SHALL keep the previous review readable."
  (git-appraise-comment-performance-test--with-review
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let ((before (buffer-string))
            (root magit-root-section))
        (cl-letf (((symbol-function 'git-appraise-section-insert-top-level-comments)
                   (lambda ()
                     (insert "partial conversation")
                     (error "renderer failed"))))
          (should-error
           (git-appraise-comment--refresh-conversation-in-place
            (buffer-name))))
        (should (equal before (buffer-string)))
        (should (eq root magit-root-section))
        (should (git-appraise-section--find-section
                 (current-buffer)
                 (lambda (s)
                   (eq (oref s type)
                       'git-appraise-comment-header-section))))))))
