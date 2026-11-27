;;; git-appraise-land-test.el --- git-appraise submit/land e2e tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'cl-lib)
(require 'a)
(require 'git-appraise-backend)
(require 'git-appraise-test-helpers)

(defun git-appraise-land-test--json (dir &rest args)
  "Run git-appraise ARGS in DIR and parse JSON output."
  (json-parse-string
   (apply #'git-appraise-test--run dir "git-appraise" args)
   :object-type 'alist :array-type 'list :null-object nil))

;;; req-2026-11-25-020:
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend-land-review
;; enforcement: test
(ert-deftest land/req-2026-11-25-020-land-accepted-review-fast-forward ()
  "WHEN an accepted review is landed,
THE SYSTEM SHALL integrate the review into its target and mark it submitted."
  (git-appraise-test--with-review-repo "git-appraise-land-"
    (let ((feature-head (git-appraise-test--run directory "git" "rev-parse" "feature")))
      (git-appraise-test--run directory "git-appraise" "accept" "-m" "ok" revision)
      (git-appraise-backend-land-review directory revision "fast-forward")
      (let ((master-head (git-appraise-test--run directory "git" "rev-parse" "master")))
        (should (equal master-head feature-head)))
      (let* ((reviews (git-appraise-land-test--json directory "list" "-a" "-json"))
             (review (cl-find-if (lambda (r) (equal (a-get r 'revision) revision))
                                 reviews)))
        (should review)
        (should (eq (a-get review 'submitted) t))))))

;;; req-2026-11-25-021:
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend-land-review
;; enforcement: test
(ert-deftest land/req-2026-11-25-021-land-unaccepted-review-errors ()
  "WHEN a user lands an unaccepted review,
THE SYSTEM SHALL report the git-appraise error."
  (git-appraise-test--with-review-repo "git-appraise-land-err-"
    (should-error (git-appraise-backend-land-review directory revision "fast-forward"))))
