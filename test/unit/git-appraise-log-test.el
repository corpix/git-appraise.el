;;; git-appraise-log-test.el --- Review error logging -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'git-appraise-utils)

;;; req-2026-11-25-051: logging cannot break review actions
;; covers: git-appraise-utils.el
;; covers-fn: git-appraise-utils--log
;; enforcement: test
(ert-deftest log/req-2026-11-25-051-log-failure-is-nonfatal ()
  "WHEN the review log is unavailable THE SYSTEM SHALL keep the review action usable."
  (cl-letf (((symbol-function 'write-region)
             (lambda (&rest _) (signal 'file-error '("log unavailable")))))
    (should (stringp (git-appraise-utils--log "test" "failure")))))

;;; req-2026-11-25-052: log entries accumulate
;; covers: git-appraise-utils.el
;; covers-fn: git-appraise-utils--log
;; enforcement: test
(ert-deftest log/req-2026-11-25-052-append-log-entries ()
  "WHEN multiple review errors occur THE SYSTEM SHALL retain every log entry."
  (let ((git-appraise-log-file (make-temp-file "git-appraise-log-test-")))
    (unwind-protect
        (progn
          (git-appraise-utils--log "first" "one")
          (git-appraise-utils--log "second" "two")
          (with-temp-buffer
            (insert-file-contents git-appraise-log-file)
            (should (search-forward "first - one" nil t))
            (should (search-forward "second - two" nil t))))
      (delete-file git-appraise-log-file))))
