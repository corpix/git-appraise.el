;;; git-appraise-pr-state-test.el --- Tests for persisted review state -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify unsent review state persists in the store across restart.

;;; Code:

(require 'ert)
(require 'dash)
(require 'a)
(require 'git-appraise-db)
(require 'git-appraise-backend)
(require 'git-appraise-pr-state)
(require 'git-appraise-test-helpers)

;;; req-2026-11-24-142: feedback and local drafts persist across restart
;; covers: git-appraise-pr-state.el git-appraise-db.el
;; covers-fn: git-appraise-pr-state-persist, git-appraise-pr-state-hydrate, git-appraise-db--pullreq-feedback-update, git-appraise-db--pullreq-raw-comments-update
;; enforcement: test
(ert-deftest pr-state/req-2026-11-24-142-feedback-and-drafts-persist-across-restart ()
  "WHEN feedback and local draft comments are saved and Emacs state is reset THE SYSTEM SHALL restore them from the store on reopen."
  (let ((git-appraise-store-file
         (make-temp-file "git-appraise-pr-state-reopen-" nil ".sqlite")))
    (unwind-protect
        (git-appraise-test--with-git-repo
         (let* ((owner default-directory)
                (pr (git-appraise-db--pullreq-create
                     (git-appraise-backend-repo
                      :id "test-pr-1"
                      :owner owner
                      :repo "repo"
                      :number "abc123"
                      :url "appraise://abc123")))
                (draft `((author (login . "tester"))
                         (state . "LOCAL COMMENT")
                         (comments (nodes ((internal-id . "draft-1")
                                           (bodyText . "draft body")
                                           (bodyHTML . "<p>draft body</p>")
                                           (path . "f.txt")
                                           (position . 1)
                                           (databaseId . "local-draft-1")
                                           (createdAt . "2026-01-01T00:00:00Z")))))))
           ;; Save feedback and a local draft.
           (git-appraise-db--pullreq-feedback-update "Needs work")
           (git-appraise-db--pullreq-raw-comments-update draft)
           (should (equal (git-appraise-db--pullreq-feedback) "Needs work"))
           (should (= 1 (length (oref pr raw-comments))))

           ;; Simulate restart: clear in-memory DB, reopen same review.
           (git-appraise-test--reset-db)
           (should-not (git-appraise-db-get-pullreq))
           (let ((pr2 (git-appraise-db--pullreq-create
                       (git-appraise-backend-repo
                        :id "test-pr-2"
                        :owner owner
                        :repo "repo"
                        :number "abc123"
                        :url "appraise://abc123"))))
             (should (equal (oref pr2 feedback) "Needs work"))
             (let* ((locals (--filter (git-appraise-db--local-comment-entry-p it)
                                      (oref pr2 raw-comments)))
                    (node (car (a-get-in (car locals) '(comments nodes)))))
               (should (= 1 (length locals)))
               (should (equal (a-get node 'internal-id) "draft-1"))
               (should (equal (a-get node 'bodyText) "draft body"))))))
      (ignore-errors (delete-file git-appraise-store-file))
      (ignore-errors (delete-file
                      (concat git-appraise-store-file "-wal")))
      (ignore-errors (delete-file
                      (concat git-appraise-store-file "-shm"))))))

;;; req-2026-11-25-046: draft edits persist as one recoverable change
;; covers: git-appraise-db.el git-appraise-pr-state.el
;; covers-fn: git-appraise-db--upsert-local-comment, git-appraise-pr-state-persist
;; enforcement: test
(ert-deftest pr-state/req-2026-11-25-046-draft-edit-is-atomic ()
  "WHEN a reviewer edits a draft THE SYSTEM SHALL persist one replacement and keep the previous draft if saving fails."
  (git-appraise-test--with-git-repo
    (let* ((pr (git-appraise-db--pullreq-create
                (git-appraise-backend-repo
                 :id "test-pr-edit" :owner default-directory
                 :repo "repo" :number "abc123")))
           (old '((state . "LOCAL COMMENT")
                  (comments (nodes ((internal-id . "draft-1")
                                    (bodyText . "before")
                                    (path . "f.txt"))))))
           (new '((state . "LOCAL COMMENT")
                  (comments (nodes ((internal-id . "draft-1")
                                    (bodyText . "after")
                                    (path . "f.txt"))))))
           (writes 0)
           (persist (symbol-function 'git-appraise-pr-state-persist)))
      (git-appraise-db--pullreq-raw-comments-update old)
      (cl-letf (((symbol-function 'git-appraise-pr-state-persist)
                 (lambda (obj &optional strict)
                   (cl-incf writes)
                   (funcall persist obj strict))))
        (git-appraise-db--upsert-local-comment new "draft-1"))
      (should (= writes 1))
      (should (equal (oref pr raw-comments) (list new)))
      (cl-letf (((symbol-function 'git-appraise-pr-state-persist)
                 (lambda (&rest _) (error "note write failed"))))
        (should-error (git-appraise-db--upsert-local-comment old "draft-1")))
      (should (equal (oref pr raw-comments) (list new))))))

;;; git-appraise-pr-state-test.el ends here
