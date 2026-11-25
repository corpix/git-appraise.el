;;; git-appraise-rebase-test.el --- git-appraise rebase e2e tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'cl-lib)
(require 'a)
(require 'git-appraise-backend)
(require 'git-appraise-test-helpers)

(defun git-appraise-rebase-test--json (dir &rest args)
  "Run git-appraise ARGS in DIR and parse JSON output."
  (json-parse-string
   (apply #'git-appraise-test--run dir "git-appraise" args)
   :object-type 'alist :array-type 'list :null-object nil))

;;; req-2026-11-25-010:
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend-rebase
;; enforcement: test
(ert-deftest rebase/req-2026-11-25-010-rebase-updates-review-after-target-advance ()
  "WHEN a review target advances and the user rebases the review,
THE SYSTEM SHALL rebase the review onto the new target and keep the review open."
  (git-appraise-test--with-review-repo "git-appraise-rebase-"
    (let ((process-environment
           (append '("GIT_AUTHOR_DATE=2026-01-01T00:00:01Z"
                     "GIT_COMMITTER_DATE=2026-01-01T00:00:01Z")
                   process-environment)))
      (git-appraise-test--run directory "git" "checkout" "-q" "master")
      (with-temp-file (expand-file-name "master.txt" directory)
        (insert "master advance\n"))
      (git-appraise-test--run directory "git" "add" ".")
      (git-appraise-test--run directory "git" "commit" "-qm" "master advance")
      (let ((master-tip (git-appraise-test--run directory "git" "rev-parse" "HEAD")))
        (git-appraise-test--run directory "git" "checkout" "-q" "feature")
        (git-appraise-backend-rebase directory revision)
        (let ((feature-head (git-appraise-test--run directory "git" "rev-parse" "HEAD")))
          (should (zerop (process-file "git" nil nil nil
                                       "-C" directory
                                       "merge-base" "--is-ancestor"
                                       master-tip feature-head)))
          (let* ((reviews (git-appraise-rebase-test--json directory "list" "-a" "-json"))
                 (review (cl-find-if (lambda (r) (equal (a-get r 'revision) revision))
                                     reviews)))
            (should review)
            (should-not (eq (a-get review 'submitted) t))))))))

;;; req-2026-11-25-011:
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend-rebase
;; enforcement: test
(ert-deftest rebase/req-2026-11-25-011-rebase-abandoned-review-errors ()
  "WHEN a user rebases an abandoned review,
THE SYSTEM SHALL report the git-appraise error."
  (git-appraise-test--with-review-repo "git-appraise-rebase-err-"
    (git-appraise-test--run directory "git-appraise" "abandon" "-m" "done" revision)
    (should-error (git-appraise-backend-rebase directory revision))))
