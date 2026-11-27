;;; git-appraise-comment-session-test.el --- Composer isolation -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

;;; req-2026-11-25-047: composers retain their original review
;; covers: git-appraise-comment.el
;; covers-fn: git-appraise-comment--composer-buffer, git-appraise-comment-commit
;; enforcement: test
(ert-deftest comment-session/req-2026-11-25-047-two-reviews ()
  "WHEN a reviewer has composers open for two reviews THE SYSTEM SHALL save each comment to its original review."
  (git-appraise-test--with-git-repo
    (let* ((one (git-appraise-db--pullreq-create
                 (git-appraise-backend-repo
                  :id "one" :owner default-directory :repo "repo" :number "one")))
           (two (git-appraise-db--pullreq-create
                 (git-appraise-backend-repo
                  :id "two" :owner default-directory :repo "repo" :number "two")))
           (review-one (generate-new-buffer " *review-one*"))
           (review-two (generate-new-buffer " *review-two*"))
           (seen nil)
           composer-one composer-two)
      (unwind-protect
          (progn
            (with-current-buffer review-one
              (setq-local git-appraise-review-buffer-pr-id (oref one id)))
            (with-current-buffer review-two
              (setq-local git-appraise-review-buffer-pr-id (oref two id)))
            (setq composer-one (git-appraise-comment--composer-buffer review-one))
            (with-current-buffer composer-one
              (git-appraise-comment-mode)
              (setq git-appraise-comment-review-buffer review-one
                    git-appraise-comment-uncommitted 'first)
              (git-appraise-comment--capture-session)
              (insert "first body"))
            (setq composer-two (git-appraise-comment--composer-buffer review-two))
            (should-not (eq composer-one composer-two))
            (with-current-buffer composer-two
              (git-appraise-comment-mode)
              (setq git-appraise-comment-review-buffer review-two
                    git-appraise-comment-uncommitted 'second)
              (git-appraise-comment--capture-session)
              (insert "second body"))
            (cl-letf (((symbol-function 'git-appraise-comment--commit-text)
                       (lambda (pr body)
                         (push (list (oref pr id)
                                     git-appraise-comment-uncommitted body)
                               seen))))
              (setq git-appraise-db--pullreq-id (oref two id))
              (with-current-buffer composer-one
                (git-appraise-comment-commit))
              (setq git-appraise-db--pullreq-id (oref one id))
              (with-current-buffer composer-two
                (git-appraise-comment-commit)))
            (should (equal (nreverse seen)
                           '(("one" first "first body")
                             ("two" second "second body")))))
        (dolist (buf (list composer-one composer-two review-one review-two))
          (when (buffer-live-p buf) (kill-buffer buf)))))))
