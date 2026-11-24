;;; git-appraise-fold-interactive-e2e-test.el --- E2E fold interaction tests -*- lexical-binding: t; -*-

;;; Commentary:
;; E2E coverage for fold state across interactive actions, reloads and
;; buffer reopen, using fixtures for expected state.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defun git-appraise-fold-interactive-e2e-test--build (directory revision)
  "Render REVISION in DIRECTORY and drain lazy work."
  (let ((default-directory (file-name-as-directory directory))
        (git-appraise-history-enabled nil))
    (git-appraise-backend-open revision)
    (let ((git-appraise-section-full-refresh? t))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))))

(defun git-appraise-fold-interactive-e2e-test--file-states ()
  "Return (path . folded/unfolded) for file sections."
  (git-appraise-viewed-e2e-test--file-fold-states))

;;; req-2026-11-24-135: fold state survives conversation toggle and reload
;; covers: git-appraise-folding.el git-appraise-section-comment.el git-appraise-lazy.el
;; covers-fn: git-appraise-fold-toggle-at-point, git-appraise-reload
;; enforcement: test
(ert-deftest fold-interactive-e2e/req-2026-11-24-135-conversation-toggle-preserves-folds ()
  "WHEN a reviewer toggles conversation while files have fold state THE SYSTEM SHALL preserve file fold state across reload."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-fold-interactive-e2e-test--build directory revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (big mode other)
        (magit-map-sections
         (lambda (s)
           (when (eq (oref s type) 'file)
             (pcase (oref s value)
               ("big.txt" (setq big s))
               ("mode.sh" (setq mode s))
               ("other.txt" (setq other s)))))
         magit-root-section)
        (should big) (should mode) (should other)
        (git-appraise-folding--set-file big t)
        (git-appraise-folding--set-file mode nil)
        (git-appraise-folding--set-file other nil))
      ;; interact with conversation section
      (let (conv)
        (magit-map-sections
         (lambda (s)
           (when (eq (oref s type) 'git-appraise-comment-header-section)
             (setq conv s)))
         magit-root-section)
        (when conv
          (magit-section-toggle conv)
          (magit-section-toggle conv))))
    (let* ((fixture (expand-file-name "viewed-reload-fold-state.sexp"
                                      git-appraise-viewed-e2e-test--fixtures-dir))
           (expected (with-temp-buffer
                       (insert-file-contents fixture)
                       (read (current-buffer)))))
      (should (equal (git-appraise-fold-interactive-e2e-test--file-states) expected))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (git-appraise-reload))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (should (equal (git-appraise-fold-interactive-e2e-test--file-states) expected)))))

;;; req-2026-11-24-136: file fold state survives close reopen and reload
;; covers: git-appraise-folding.el git-appraise-fold-state.el
;; covers-fn: git-appraise-folding--set-file, git-appraise-reload
;; enforcement: test
(ert-deftest fold-interactive-e2e/req-2026-11-24-136-file-folds-survive-close-reopen-reload ()
  "WHEN a reviewer folds files, closes, reopens, and reloads THE SYSTEM SHALL preserve file fold state."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-fold-interactive-e2e-test--build directory revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (big mode other)
        (magit-map-sections
         (lambda (s)
           (when (eq (oref s type) 'file)
             (pcase (oref s value)
               ("big.txt" (setq big s))
               ("mode.sh" (setq mode s))
               ("other.txt" (setq other s)))))
         magit-root-section)
        (git-appraise-folding--set-file big t)
        (git-appraise-folding--set-file mode nil)
        (git-appraise-folding--set-file other t)))
    (let* ((fixture (expand-file-name "viewed-reload-fold-state.sexp"
                                      git-appraise-viewed-e2e-test--fixtures-dir))
           (expected '(("big.txt" . folded) ("mode.sh" . unfolded) ("other.txt" . folded))))
      (should (equal (git-appraise-fold-interactive-e2e-test--file-states) expected))
      (kill-buffer (git-appraise-pr-buffer-name))
      (git-appraise-fold-interactive-e2e-test--build directory revision)
      (should (equal (git-appraise-fold-interactive-e2e-test--file-states) expected))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (git-appraise-reload))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (should (equal (git-appraise-fold-interactive-e2e-test--file-states) expected)))))

;;; req-2026-11-24-137: hunk fold state survives reload
;; covers: git-appraise-folding.el git-appraise-fold-state.el
;; covers-fn: git-appraise-fold-toggle-at-point, git-appraise-reload
;; enforcement: test
(ert-deftest fold-interactive-e2e/req-2026-11-24-137-hunk-folds-survive-reload ()
  "WHEN a reviewer toggles hunk folds and reloads THE SYSTEM SHALL preserve hunk fold state."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-fold-interactive-e2e-test--build directory revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (hunks)
        (magit-map-sections
         (lambda (s) (when (eq (oref s type) 'hunk) (push s hunks)))
         magit-root-section)
        (setq hunks (nreverse hunks))
        (should (>= (length hunks) 2))
        ;; persistently set first hunk unfolded, second folded
        (dolist (spec (list (cons (nth 0 hunks) nil)
                            (cons (nth 1 hunks) t)))
          (let* ((h (car spec))
                 (folded (cdr spec))
                 (id (git-appraise-fold-state--hunk-identity h)))
            (when id
              (pcase-let ((`(,path ,fp ,range) id))
                (git-appraise-fold-state--write
                 (list (list "hunk" path fp range)) folded)))
            (if folded (magit-section-hide h) (magit-section-show h))))
        (let ((states (mapcar (lambda (h) (oref h hidden)) hunks)))
          (git-appraise-reload)
          (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
          (let (hunks2)
            (magit-map-sections
             (lambda (s) (when (eq (oref s type) 'hunk) (push s hunks2)))
             magit-root-section)
            (setq hunks2 (nreverse hunks2))
            (should (equal (mapcar (lambda (h) (oref h hidden)) hunks2)
                           states))))))))

;;; req-2026-11-24-138: fold-hunks does not punch hidden file overlays
;; covers: git-appraise-folding.el git-appraise-lazy.el
;; covers-fn: git-appraise-section--fold-hunks, git-appraise-section--fold-and-mark
;; enforcement: test
(ert-deftest fold-interactive-e2e/req-2026-11-24-138-fold-hunks-preserves-hidden-file-overlay ()
  "WHEN fold-hunks runs while a file is hidden THE SYSTEM SHALL keep the file overlay intact."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-fold-interactive-e2e-test--build directory revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (file hunk)
        (magit-map-sections
         (lambda (s)
           (when (and (eq (oref s type) 'file)
                      (string= (oref s value) "big.txt"))
             (setq file s))
           (when (and (eq (oref s type) 'hunk) (not hunk))
             (setq hunk s)))
         magit-root-section)
        (should file) (should hunk)
        (git-appraise-folding--set-file file t)
        ;; Mark hunk as unfolded in fold state so fold-hunks tries to show it.
        (let ((id (git-appraise-fold-state--hunk-identity hunk)))
          (when id
            (pcase-let ((`(,path ,fp ,range) id))
              (git-appraise-fold-state--write
               (list (list "hunk" path fp range)) nil))))
        (oset hunk hidden nil)
        (let ((before (cl-loop for ov in (overlays-in (oref file content) (oref file end))
                               when (overlay-get ov 'invisible)
                               collect (list (overlay-start ov) (overlay-end ov)))))
          (should (= (length before) 1))
          (should (= (caar before) (marker-position (oref file content))))
          (should (= (cadar before) (marker-position (oref file end))))
          ;; Simulate lazy drain completion.
          (git-appraise-section--fold-and-mark nil nil)
          (let ((after (cl-loop for ov in (overlays-in (oref file content) (oref file end))
                                when (overlay-get ov 'invisible)
                                collect (list (overlay-start ov) (overlay-end ov)))))
            (should (oref file hidden))
            (should (= (length after) 1))
            (should (= (caar after) (marker-position (oref file content))))
            (should (= (cadar after) (marker-position (oref file end))))))))))

(provide 'git-appraise-fold-interactive-e2e-test)
;;; git-appraise-fold-interactive-e2e-test.el ends here
