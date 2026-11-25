;;; git-appraise-sync-test.el --- git-appraise push/pull e2e tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'cl-lib)
(require 'a)
(require 'git-appraise-backend)
(require 'git-appraise-test-helpers)

;;; req-2026-11-25-001:
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend-push, git-appraise-backend-pull
;; enforcement: test
(ert-deftest sync/req-2026-11-25-001-push-pull-roundtrip ()
  "WHEN a user pushes git-appraise notes to a remote and another repo pulls them,
THE SYSTEM SHALL make the same reviews visible in the pulling repository."
  (git-appraise-test--with-review-repo "git-appraise-sync-"
    (let* ((origin (make-temp-file "git-appraise-origin-" t))
           (clone (make-temp-file "git-appraise-clone-" t)))
      (unwind-protect
          (progn
            (git-appraise-test--run origin "git" "init" "-q" "--bare")
            (git-appraise-test--run directory "git" "remote" "add" "origin" origin)
            (git-appraise-test--run directory "git" "push" "-q" "origin" "master")
            (git-appraise-test--run directory "git" "push" "-q" "origin" "feature")

            (git-appraise-backend-push directory "origin")

            (git-appraise-test--run clone "git" "clone" "-q" origin ".")
            (git-appraise-test--run clone "git" "config" "user.name" "Tester")
            (git-appraise-test--run clone "git" "config" "user.email" "tester@example.com")
            (let ((before (json-parse-string
                           (git-appraise-test--run clone "git-appraise" "list" "-a" "-json")
                           :object-type 'alist :array-type 'list :null-object nil)))
              (should (null before)))

            (git-appraise-backend-pull clone "origin")

            (let* ((after (json-parse-string
                           (git-appraise-test--run clone "git-appraise" "list" "-a" "-json")
                           :object-type 'alist :array-type 'list :null-object nil))
                   (revisions (mapcar (lambda (r) (a-get r 'revision)) after)))
              (should (member revision revisions))))
        (ignore-errors (delete-directory origin t))
        (ignore-errors (delete-directory clone t))))))

;;; req-2026-11-25-002:
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-push-notes, git-appraise-pull-notes
;; enforcement: test
(ert-deftest sync/req-2026-11-25-002-interactive-push-pull-commands ()
  "WHEN a user runs the push and pull commands,
THE SYSTEM SHALL sync git-appraise notes through the selected remote."
  (git-appraise-test--with-review-repo "git-appraise-sync-cmd-"
    (let* ((origin (make-temp-file "git-appraise-origin-" t))
           (clone (make-temp-file "git-appraise-clone-" t)))
      (unwind-protect
          (progn
            (git-appraise-test--run origin "git" "init" "-q" "--bare")
            (git-appraise-test--run directory "git" "remote" "add" "origin" origin)
            (git-appraise-test--run directory "git" "push" "-q" "origin" "master")
            (git-appraise-test--run directory "git" "push" "-q" "origin" "feature")
            (let ((default-directory (file-name-as-directory directory)))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) "origin"))
                        ((symbol-function 'git-appraise-backend--root)
                         (lambda () directory)))
                (git-appraise-push-notes "origin")))
            (git-appraise-test--run clone "git" "clone" "-q" origin ".")
            (git-appraise-test--run clone "git" "config" "user.name" "Tester")
            (git-appraise-test--run clone "git" "config" "user.email" "tester@example.com")
            (let ((default-directory (file-name-as-directory clone)))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) "origin"))
                        ((symbol-function 'git-appraise-backend--root)
                         (lambda () clone))
                        ((symbol-function 'git-appraise--build-buffer)
                         #'ignore))
                (git-appraise-pull-notes "origin")))
            (let* ((after (json-parse-string
                           (git-appraise-test--run clone "git-appraise" "list" "-a" "-json")
                           :object-type 'alist :array-type 'list :null-object nil))
                   (revisions (mapcar (lambda (r) (a-get r 'revision)) after)))
              (should (member revision revisions))))
        (ignore-errors (delete-directory origin t))
        (ignore-errors (delete-directory clone t))))))
