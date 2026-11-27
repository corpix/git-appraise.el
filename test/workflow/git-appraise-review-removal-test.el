;;; git-appraise-review-removal-test.el --- Review recreation journey -*- lexical-binding: t; -*-

(require 'a)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-remove)
(require 'git-appraise-test-helpers)
(require 'git-appraise-viewed)
(require 'git-appraise-pr-state)

(defun git-appraise-review-removal-test--show (directory revision)
  "Return git-appraise's review JSON for REVISION in DIRECTORY."
  (git-appraise-backend--json
   (git-appraise-test--run directory "git-appraise" "show" "-json" revision)
   "test show"))

;;; req-2026-11-27-002: removing a review clears its discussion and state
;; covers: git-appraise-remove.el git-appraise-store.el git-appraise.el
;; covers-fn: git-appraise-remove-review-notes, git-appraise-store-clear-review
;; enforcement: test
(ert-deftest review-removal/req-2026-11-27-002-recreate-without-old-state ()
  "WHEN a reviewer removes and recreates a review THE SYSTEM SHALL start it without old comments, decisions, or personal marks."
  (git-appraise-test--with-review-repo "git-appraise-remove-"
    (let ((review-buffer (git-appraise-test--open-and-render revision)))
      (should (transient-get-suffix 'git-appraise-transient-api "D"))
      (should (transient-get-suffix 'git-appraise-magit-dispatch "D"))
      (with-current-buffer review-buffer
        (goto-char (point-min))
        (search-forward "example.txt")
        (git-appraise-viewed-toggle-file)
        (let ((pr (git-appraise-db-get-pullreq)))
          (oset pr feedback "Old private feedback")
          (git-appraise-pr-state-persist pr t)))
      (should (= 1 (length (git-appraise-store-events
                            directory revision 'viewed))))
      (should (git-appraise-store-pr-state
               directory revision "tester@example.com"))
      (git-appraise-test--run directory "git-appraise" "comment"
                              "-m" "Old inline finding" "-nmw"
                              "-f" "example.txt" "-l" "2" revision)
      (git-appraise-test--run directory "git-appraise" "reject"
                              "-m" "Old rejection" revision)
      (let ((old (git-appraise-review-removal-test--show directory revision)))
        (should (a-get old 'comments))
        (should (eq :false (a-get old 'resolved))))
      ;; Deleting only the request note leaves git-appraise's separate
      ;; discussion note.  Re-requesting the same commit inherits it.
      (let ((old-tip (git-appraise-test--run
                      directory "git" "rev-parse"
                      "refs/notes/devtools/reviews")))
        (git-appraise-test--run directory "git" "notes"
                                "--ref=refs/notes/devtools/reviews" "remove" revision)
        (should (equal "commit"
                       (git-appraise-test--run directory "git" "cat-file"
                                               "-t" old-tip))))
      (git-appraise-test--run directory "git-appraise" "request" "-quiet"
                              "-m" "Second review" "-target" "refs/heads/master")
      (let ((inherited (git-appraise-review-removal-test--show
                        directory revision)))
        (should (a-get inherited 'comments))
        (should (eq :false (a-get inherited 'resolved))))
      (with-current-buffer review-buffer
        (git-appraise-reload))
      (git-appraise-test--drain-lazy review-buffer)
      (with-current-buffer review-buffer
        (should (string-match-p
                 "Old[[:space:]\n]+inline[[:space:]\n]+finding"
                 (buffer-string)))
        (should (string-match-p
                 "Old[[:space:]\n]+rejection" (buffer-string))))
      ;; The package's removal action clears both notes refs and its
      ;; personal store, leaving the commit available for a clean review.
      (with-current-buffer review-buffer
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) t)))
          (git-appraise-remove-current-review)))
      (should-not (buffer-live-p review-buffer))
      (should-not (git-appraise-store-events directory revision 'viewed))
      (should-not (git-appraise-store-pr-state
                   directory revision "tester@example.com"))
      (should (equal revision
                     (git-appraise-test--run directory "git" "rev-parse" "HEAD")))
      (git-appraise-test--run directory "git-appraise" "request" "-quiet"
                              "-m" "Clean review" "-target" "refs/heads/master")
      (let ((fresh (git-appraise-review-removal-test--show directory revision)))
        (should-not (a-get fresh 'comments))
        (should-not (a-get fresh 'resolved)))
      (git-appraise-test--open-and-render revision)
      (with-current-buffer (git-appraise-pr-buffer-name)
        (should (string-match-p "0/1 viewed" (buffer-string)))
        (should-not (oref (git-appraise-db-get-pullreq) feedback))
        (should-not (string-match-p "Old inline finding\\|Old rejection"
                                    (buffer-string)))))))

(ert-deftest review-removal/failed-state-clear-restores-notes ()
  "A failed local-state deletion restores the review's Git notes refs."
  (git-appraise-test--with-review-repo "git-appraise-remove-rollback-"
    (git-appraise-test--run directory "git-appraise" "comment"
                            "-m" "Keep this finding" "-nmw" revision)
    (let ((request-tip (git-appraise-test--run
                        directory "git" "rev-parse"
                        "refs/notes/devtools/reviews"))
          (discussion-tip (git-appraise-test--run
                           directory "git" "rev-parse"
                           "refs/notes/devtools/discuss")))
      (cl-letf (((symbol-function 'git-appraise-store-clear-review)
                 (lambda (&rest _) (error "Simulated store failure"))))
        (should-error (git-appraise-remove-review-notes directory revision)))
      (should (equal request-tip
                     (git-appraise-test--run
                      directory "git" "rev-parse"
                      "refs/notes/devtools/reviews")))
      (should (equal discussion-tip
                     (git-appraise-test--run
                      directory "git" "rev-parse"
                      "refs/notes/devtools/discuss")))
      (should (a-get (git-appraise-review-removal-test--show
                      directory revision) 'comments)))))

;;; git-appraise-review-removal-test.el ends here
