;;; git-appraise-sync-journey-test.el --- Review sync workflow -*- lexical-binding: t; -*-

(require 'ert)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

;;; req-2026-11-27-016: pulled discussion updates an open review
;; covers: git-appraise-backend.el git-appraise-store.el git-appraise-section.el
;; covers-fn: git-appraise-push-notes, git-appraise-pull-notes, git-appraise-reload
;; enforcement: test
(ert-deftest sync-journey/req-2026-11-27-016-open-review-after-pull ()
  "WHEN a reviewer pulls new notes into an open review THE SYSTEM SHALL show the new discussion and retain private progress."
  (git-appraise-test--with-review-repo "git-appraise-sync-journey-"
    (let ((origin (make-temp-file "git-appraise-sync-origin-" t))
          (clone (make-temp-file "git-appraise-sync-clone-" t))
          clone-buffer)
      (unwind-protect
          (progn
            (git-appraise-test--run origin "git" "init" "-q" "--bare")
            (git-appraise-test--run directory "git" "remote" "add" "origin" origin)
            (git-appraise-test--run directory "git" "push" "-q" "origin" "master" "feature")
            (let ((default-directory (file-name-as-directory directory)))
              (git-appraise-push-notes "origin"))
            (git-appraise-test--run clone "git" "clone" "-q" origin ".")
            (git-appraise-test--run clone "git" "config" "user.name" "Tester")
            (git-appraise-test--run clone "git" "config" "user.email" "tester@example.com")
            (git-appraise-backend-pull clone "origin")
            (let ((default-directory (file-name-as-directory clone)))
              (setq clone-buffer (git-appraise-test--open-and-render revision)))
            (with-current-buffer clone-buffer
              (goto-char (point-min))
              (search-forward "+changed")
              (beginning-of-line)
              (call-interactively (key-binding (kbd "RET"))))
            (with-current-buffer git-appraise-comment-buffer-name
              (erase-buffer)
              (insert "Private clone draft")
              (call-interactively (key-binding (kbd "C-c C-c"))))
            (with-current-buffer clone-buffer
              (goto-char (point-min))
              (search-forward "example.txt")
              (beginning-of-line)
              (call-interactively (key-binding (kbd "V"))))
            (should (git-appraise-store-events clone revision 'viewed))
            (should-not (git-appraise-store-events directory revision 'viewed))
            (should (git-appraise-store-pr-state
                     clone revision "tester@example.com"))
            (should-not (git-appraise-store-pr-state
                         directory revision "tester@example.com"))
            (git-appraise-test--run
             directory "git-appraise" "comment" "-m" "Upstream discussion" revision)
            (let ((default-directory (file-name-as-directory directory)))
              (git-appraise-push-notes "origin"))
            (with-current-buffer clone-buffer
              (git-appraise-pull-notes "origin")
              (call-interactively (key-binding (kbd "g"))))
            (git-appraise-test--drain-lazy clone-buffer)
            (with-current-buffer clone-buffer
              (should (string-match-p "Upstream[[:space:]\n]+discussion"
                                      (substring-no-properties (buffer-string))))
              (should (string-match-p "Private[[:space:]\n]+clone[[:space:]\n]+draft"
                                      (substring-no-properties (buffer-string)))))
            (should (git-appraise-store-events clone revision 'viewed)))
        (when (buffer-live-p clone-buffer)
          (kill-buffer clone-buffer))
        (when (get-buffer git-appraise-comment-buffer-name)
          (kill-buffer git-appraise-comment-buffer-name))
        (delete-directory clone t)
        (delete-directory origin t)))))

;;; git-appraise-sync-journey-test.el ends here
