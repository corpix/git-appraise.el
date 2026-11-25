;;; git-appraise-active-dev-e2e-test.el --- E2E tests for active-development reviews -*- lexical-binding: t; -*-

;;; Commentary:
;; E2E scenarios for comments/drafts while the reviewed feature gains commits.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defmacro git-appraise-active-dev-e2e-test-with-repo (&rest body)
  "Run BODY in a disposable git-appraise repository with one review."
  (declare (indent 0))
  `(git-appraise-test--with-review-repo "git-appraise-active-dev-e2e-"
     ,@body))

(defun git-appraise-active-dev-e2e-test--open-and-render (revision)
  "Open REVISION and render it fully."
  (let ((git-appraise-history-enabled nil))
    (git-appraise-backend-open revision)
    (let ((git-appraise-section-full-refresh? t))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))))

;;; req-2026-11-24-129: local draft survives head change refresh
;; covers: git-appraise-backend.el git-appraise-comment-draft.el
;; covers-fn: git-appraise-backend-refresh, git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest git-appraise-active-dev-e2e/req-2026-11-24-129-draft-survives-head-change ()
  "WHEN new commits appear during review THE SYSTEM SHALL preserve unsent local drafts across refresh."
  (git-appraise-active-dev-e2e-test-with-repo
    (git-appraise-active-dev-e2e-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (git-appraise-section-full-refresh? nil))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "^\\+changed" nil t)
        (let ((pos (line-beginning-position))
              (comment (git-appraise-local-comment-section
                        :state "LOCAL COMMENT"
                        :author "Tester"
                        :path "example.txt"
                        :side "RIGHT"
                        :line 2
                        :line-type "ADDED"
                        :send? nil
                        :msg "draft across commits")))
          (setq git-appraise-comment-cursor-pos pos
                git-appraise-comment-uncommitted comment
                git-appraise-comment-commit-buffer? nil)
          (git-appraise-comment-handler-commit comment git-appraise-comment-buffer-msg)))
      (with-temp-file (expand-file-name "example.txt" directory)
        (insert "first\nchanged again\nthird\nfourth\n"))
      (git-appraise-test--run directory "git" "commit" "-qam" "followup")
      (with-current-buffer buf
        (git-appraise-reload))
      (git-appraise-test--drain-lazy buf)
      (should (cl-find-if #'git-appraise-db--local-comment-entry-p
                          (git-appraise-db--pullreq-raw-comments)))
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "draft across commits" nil t))))))

;;; req-2026-11-24-130: line comment send preserves anchor safety after head change
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend--line-for-comment, git-appraise-backend-send-code-comment
;; enforcement: test
(ert-deftest git-appraise-active-dev-e2e/req-2026-11-24-130-send-remaps-after-head-change ()
  "WHEN a reviewer sends a line comment after new commits appear THE SYSTEM SHALL retain the draft until its location is checked."
  (git-appraise-active-dev-e2e-test-with-repo
    (git-appraise-active-dev-e2e-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (pr (git-appraise-db-get-pullreq)))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "^\\+changed" nil t))
      (with-temp-file (expand-file-name "example.txt" directory)
        (insert "first\nchanged\nthird\nfourth\n"))
      (git-appraise-test--run directory "git" "commit" "-qam" "followup")
      ;; Make the cached sha stale so the send path detects head change.
      (oset pr sha revision)
      (let ((comment (git-appraise-local-comment-section
                      :state "LOCAL COMMENT"
                      :author "Tester"
                      :path "example.txt"
                      :position 3
                      :line-type "ADDED"
                      :send? t
                      :msg "remapped comment")))
        (should-error
         (git-appraise-backend-send-code-comment pr comment (lambda () nil))
         :type 'user-error)
        (should-not (a-get (git-appraise-backend--read-review directory revision)
                           'comments))))))

;;; req-2026-11-25-067: integration changes invalidate old inline positions
;; covers: git-appraise-backend.el git-appraise-backend-read.el
;; covers-fn: git-appraise-backend--line-for-comment
;; enforcement: test
(ert-deftest git-appraise-active-dev-e2e/req-2026-11-25-067-integration-keeps-draft ()
  "WHEN the target branch incorporates a review THE SYSTEM SHALL keep an old inline draft for re-anchoring."
  (git-appraise-active-dev-e2e-test-with-repo
    (git-appraise-active-dev-e2e-test--open-and-render revision)
    (let ((pr (git-appraise-db-get-pullreq))
          (comment (git-appraise-local-comment-section
                    :state "LOCAL COMMENT" :author "Tester"
                    :msg "Old location" :path "example.txt"
                    :position 3 :line-type "ADDED")))
      (git-appraise-test--run directory "git" "update-ref"
                              "refs/heads/master" revision)
      (should-not
       (equal (oref (oref pr native-review) base)
              (git-appraise-backend--git directory "merge-base"
                                         "refs/heads/master" revision)))
      (should-error (git-appraise-backend--line-for-comment pr comment)
                    :type 'user-error)
      (should-not (a-get (git-appraise-backend--read-review directory revision)
                         'comments)))))

;;; req-2026-11-24-131: file draft visible after head change refresh
;; covers: git-appraise-comment-draft.el git-appraise-backend.el
;; covers-fn: git-appraise-comment-handler-commit, git-appraise-backend-refresh
;; enforcement: test
(ert-deftest git-appraise-active-dev-e2e/req-2026-11-24-131-file-draft-visible-after-head-change ()
  "WHEN new commits appear and a file draft exists THE SYSTEM SHALL keep the draft visible after refresh."
  (git-appraise-active-dev-e2e-test-with-repo
    (git-appraise-active-dev-e2e-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (git-appraise-section-full-refresh? nil))
      (with-current-buffer buf
        (let ((comment (git-appraise-local-comment-section
                        :state "LOCAL COMMENT"
                        :author "Tester"
                        :path "example.txt"
                        :position 0
                        :line-type "FILE"
                        :send? nil
                        :msg "file draft survives commits")))
          (setq git-appraise-comment-cursor-pos (point-min)
                git-appraise-comment-uncommitted comment
                git-appraise-comment-commit-buffer? nil)
          (git-appraise-comment-handler-commit comment git-appraise-comment-buffer-msg)))
      (with-temp-file (expand-file-name "example.txt" directory)
        (insert "first\nchanged again\nthird\nfourth\n"))
      (git-appraise-test--run directory "git" "commit" "-qam" "followup")
      (with-current-buffer buf
        (git-appraise-reload))
      (git-appraise-test--drain-lazy buf)
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "file draft survives commits" nil t))))))

(provide 'git-appraise-active-dev-e2e-test)
;;; git-appraise-active-dev-e2e-test.el ends here
