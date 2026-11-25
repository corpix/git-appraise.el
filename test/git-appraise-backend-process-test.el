;;; git-appraise-backend-process-test.el --- Parallel fetch tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Guard process lifetime and temporary buffers during review loading.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dash)
(require 'git-appraise-backend-process)

(defun git-appraise-backend-process-test--buffers ()
  "Return temporary buffers left by parallel backend commands."
  (-filter (lambda (buffer)
             (string-match-p "\\` \\*ga-par-" (buffer-name buffer)))
           (buffer-list)))

;;; req-2026-11-25-053: review load setup failure releases resources
;; covers: git-appraise-backend-process.el
;; covers-fn: git-appraise-backend--run-parallel
;; enforcement: test
(ert-deftest git-appraise-backend-process/req-2026-11-25-053-clean-setup-failure ()
  "WHEN opening a review cannot start THE SYSTEM SHALL report the failure without leaving loading active."
  (let ((before (git-appraise-backend-process-test--buffers))
        (original (symbol-function 'make-process)))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (if (equal (plist-get args :name) "ga-par-invalid")
                     (progn
                       (should (process-live-p (get-process "ga-par-waiting")))
                       (signal 'file-missing '("Cannot start command")))
                   (apply original args)))))
      (should-error
       (git-appraise-backend--run-parallel
        temporary-file-directory
        '((waiting "cat") (invalid "cat")))
       :type 'file-missing))
    (should (equal (git-appraise-backend-process-test--buffers) before))
    (should-not (get-process "ga-par-waiting"))))

;;; req-2026-11-25-054: failed review fetch ends promptly
;; covers: git-appraise-backend-process.el
;; covers-fn: git-appraise-backend--run-parallel
;; enforcement: test
(ert-deftest git-appraise-backend-process/req-2026-11-25-054-stop-on-failure ()
  "WHEN opening a review fails THE SYSTEM SHALL return control promptly."
  (let ((before (git-appraise-backend-process-test--buffers)))
    (with-timeout (2 (ert-fail "Parallel fetch waited for a blocked command"))
      (let ((err (should-error
                  (git-appraise-backend--run-parallel
                   temporary-file-directory
                   '((waiting "cat")
                     (failed "sh" "-c" "printf fetch-failed >&2; exit 7")))
                  :type 'user-error)))
        (should (string-match-p "fetch-failed" (error-message-string err)))))
    (should (equal (git-appraise-backend-process-test--buffers) before))
    (should-not (get-process "ga-par-waiting"))))

;;; req-2026-11-25-055: parallel fetch retains command order
;; covers: git-appraise-backend-process.el
;; covers-fn: git-appraise-backend--run-parallel
;; enforcement: test
(ert-deftest git-appraise-backend-process/req-2026-11-25-055-ordered-output ()
  "WHEN opening a review reads its data THE SYSTEM SHALL preserve all content in its original order."
  (should (equal (git-appraise-backend--run-parallel
                  temporary-file-directory
                  '((first "sh" "-c" "printf 'first\\nsecond\\n'")
                    (second "sh" "-c" "printf second")))
                 '((first . "first\nsecond\n") (second . "second")))))
