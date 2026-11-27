;;; git-appraise-test-actions-fixture.el --- Action test review fixture -*- lexical-binding: t; -*-

(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defvar git-appraise-actions-test-before-request nil
  "Optional fixture callback run before creating a review request.")

(defmacro git-appraise-actions-test-with-repo (&rest body)
  "Run BODY in a disposable git-appraise repository with one review."
  (declare (indent 0))
  `(let ((git-appraise-test--before-request-hook
          git-appraise-actions-test-before-request))
     (git-appraise-test--with-review-repo "git-appraise-actions-test-"
       ,@body)))

(defun git-appraise-actions-test--show (directory revision)
  "Return native git-appraise JSON for REVISION in DIRECTORY."
  (git-appraise-backend--json
   (git-appraise-test--run directory "git-appraise" "show" "-json" revision)
   "test show"))


(provide 'git-appraise-test-actions-fixture)
;;; git-appraise-test-actions-fixture.el ends here
