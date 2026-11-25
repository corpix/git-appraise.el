;;; run-tests.el --- Batch ERT runner for git-appraise -*- lexical-binding: t; -*-
;;
;; Invoked by `just test' as:
;;   emacs -Q --batch -L . -L test [-L <installed package dirs>...] \
;;     -l test/run-tests.el
;;
;; Loads every test file in this directory and runs all ERT tests.

(let ((default-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  ;; repository root first, so we test the checkout, not an installed copy
  (add-to-list 'load-path (expand-file-name ".."))
  (add-to-list 'load-path default-directory))

;; fail fast if any dependency is missing
(require 'git-appraise)
(require 'cl-lib)

;; render tests assert SYNCHRONOUS content: the deferred analysis
;; (placeholder + background child) has its own dedicated test,
;; which binds the defcustom t around its render.
(setq-default git-appraise-analysis-deferred? nil)

(dolist (file (directory-files
               (file-name-directory (or load-file-name buffer-file-name))
               t "-test\\.el\\'"))
  (load file nil t))

(defun git-appraise-test--print-failure (test result)
  "Print a clean failure report for TEST RESULT."
  (let ((name (ert-test-name test)))
    (condition-case _
        (cl-labels ((strip (x)
                      (cond
                       ((stringp x) (substring-no-properties x))
                       ((consp x) (cons (strip (car x)) (strip (cdr x))))
                       ((vectorp x) (apply #'vector (mapcar #'strip (append x nil))))
                       (t x))))
          (let* ((condition (strip (ert-test-result-with-condition-condition result)))
                 (type (type-of result))
                 (msg (condition-case nil
                          (let ((print-escape-newlines t)
                                (print-length 30)
                                (print-level 8))
                            (format "%S" condition))
                        (error (format "%S" condition)))))
            (when (> (length msg) 12000)
              (setq msg (concat (substring msg 0 12000) "\n...truncated...")))
            (message "----- FAIL %s (%s) -----" name type)
            (message "%s" msg)
            (when (and (fboundp 'ert-test-result-with-condition-backtrace)
                       (ert-test-result-with-condition-p result))
              (let* ((bt (ert-test-result-with-condition-backtrace result))
                     (shown 0))
                (when bt
                  (message "Backtrace (trimmed):")
                  (cl-loop for frame in bt do
                           (when (< shown 12)
                             (let ((s (substring-no-properties (format "%S" (strip frame)))))
                               (unless (string-match-p "ert--\\|ert-run\\|command-line\\|normal-top-level\\|load-with-code-conversion\\|handler-bind" s)
                                 (when (> (length s) 300)
                                   (setq s (concat (substring s 0 300) "...")))
                                 (message "  %s" s)
                                 (cl-incf shown))))))))
            (message "----- END %s -----" name)))
      (error
       (message "----- FAIL %s (could not format condition) -----" name)))))

(let* ((raw (or (getenv "ERT_SELECTOR")
                (car command-line-args-left)))
       (selector (cond
                  ((null raw) t)
                  ((equal raw "t") t)
                  ((string-prefix-p "(" raw) (read raw))
                  ((string-prefix-p "\"" raw) (read raw))
                  (t raw)))
       (quiet (getenv "ERT_QUIET")))
  (when command-line-args-left
    (setq command-line-args-left nil))
  (when (not (eq selector t))
    (message "ERT selector: %S" selector))
  (ert-run-tests
   selector
   (lambda (event-type &rest event-args)
     (cl-case event-type
       (run-started
        (cl-destructuring-bind (stats) event-args
          (message "Running %d tests (%s, selector %S)"
                   (length (ert--stats-tests stats))
                   (current-time-string)
                   selector)))
       (test-ended
        (cl-destructuring-bind (stats test result) event-args
          (let* ((expectedp (ert-test-result-expected-p test result))
                 (name (ert-test-name test))
                 (duration (ert-test-result-duration result))
                 (status (if expectedp "passed" "FAILED")))
            (unless (and quiet expectedp)
              (message "   %s  %s/%s  %s (%.6f sec)%s"
                       status
                       (1+ (ert--stats-test-pos stats test))
                       (length (ert--stats-tests stats))
                       name duration
                       (if expectedp "" "  <<<")))
            (unless expectedp
              (git-appraise-test--print-failure test result)))))
       (run-ended
        (cl-destructuring-bind (stats abortedp) event-args
          (let ((unexpected (ert-stats-completed-unexpected stats))
                (expected (ert-stats-completed-expected stats))
                (total (length (ert--stats-tests stats))))
            (message "\nRan %d tests, %d results as expected, %d unexpected%s (%s, %.6f sec)"
                     total expected unexpected
                     (if abortedp " (aborted)" "")
                     (current-time-string)
                     (float-time (time-subtract (ert--stats-end-time stats)
                                                (ert--stats-start-time stats))))
            (unless (zerop unexpected)
              (message "\nUnexpected results:")
              (cl-loop for test across (ert--stats-tests stats)
                       for result = (ert-test-most-recent-result test) do
                       (unless (ert-test-result-expected-p test result)
                         (message "   FAILED  %s" (ert-test-name test)))))
            (kill-emacs (if (zerop unexpected) 0 1)))))))))
