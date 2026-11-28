;;; git-appraise-test-submit-fixture.el --- Large review submission fixture -*- lexical-binding: t; -*-

(require 'a)
(require 'git-appraise-test-helpers)
(require 'git-appraise-test-viewed-bulk)
(require 'git-appraise-backend-read)

(defun git-appraise-test-submit--make-historical-finding (directory revision)
  "Add a finding on a changed file, then remove that file from the diff.
Return the published comment hash.  The review keeps its other
changed files, and the historical finding remains in Git notes."
  (git-appraise-test--run directory "git-appraise" "comment"
                          "-nmw" "-f" "file-0000.txt" "-l" "1"
                          "-m" "Historical finding on reverted file" revision)
  (let* ((review (git-appraise-backend--read-review directory revision))
         (hash (a-get (car (a-get review 'comments)) 'hash)))
    (with-temp-file (expand-file-name "file-0000.txt" directory)
      (dotimes (line git-appraise-test-viewed-bulk--lines-per-file)
        (insert (format "before-%04d\n" line))))
    (git-appraise-test--run directory "git" "commit" "-qam" "restore one file")
    hash))

(provide 'git-appraise-test-submit-fixture)
;;; git-appraise-test-submit-fixture.el ends here
