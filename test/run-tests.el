;;; run-tests.el --- Batch ERT runner for git-appraise -*- lexical-binding: t; -*-
;;
;; Invoked by `just test' as:
;;   emacs -Q --batch -L . -L test [-L <installed package dirs>...] \
;;     -l test/run-tests.el
;;
;; Loads every test file in this directory and runs all ERT tests.

;; When loaded by Buttercup discovery, skip: this is an ERT batch runner
;; and loading all *-test.el files again would redefine tests / exit.
(unless (featurep 'buttercup)
  (let ((default-directory
         (file-name-directory (or load-file-name buffer-file-name))))
    ;; repository root first, so we test the checkout, not an installed copy
    (add-to-list 'load-path (expand-file-name ".."))
    (add-to-list 'load-path default-directory))

  ;; fail fast if any dependency is missing
  (require 'git-appraise)

  ;; render tests assert SYNCHRONOUS content: the deferred analysis
  ;; (placeholder + background child) has its own dedicated test,
  ;; which binds the defcustom t around its render.
  (setq-default git-appraise-analysis-deferred? nil)

  (dolist (file (directory-files
                 (file-name-directory (or load-file-name buffer-file-name))
                 t "-test\\.el\\'"))
    (load file nil t))

  (ert-run-tests-batch-and-exit))
