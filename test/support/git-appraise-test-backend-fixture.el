;;; git-appraise-test-backend-fixture.el --- Backend review fixture -*- lexical-binding: t; -*-

(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defmacro git-appraise-backend-test-with-repo (&rest body)
  "Run BODY in a disposable repository with an existing review."
  (declare (indent 0))
  `(git-appraise-test--with-review-repo "git-appraise-backend-test-"
					,@body))

(defun git-appraise-backend-test--show (directory revision)
  "Return native review JSON for REVISION in DIRECTORY."
  (git-appraise-backend--json
   (git-appraise-test--run directory "git-appraise" "show" "-json" revision)
   "test show"))

(defun git-appraise-backend-test--first-file-and-hunk ()
  "Return the first rendered file and real hunk sections."
  (let (file)
    (magit-map-sections
     (lambda (s)
       (when (and (not file) (eq (oref s type) 'file))
         (setq file s)))
     magit-root-section)
    (list file (--first (and (magit-hunk-section-p it)
                           (git-appraise-fold-state--hunk-identity it))
                        (oref file children)))))


(provide 'git-appraise-test-backend-fixture)
;;; git-appraise-test-backend-fixture.el ends here
