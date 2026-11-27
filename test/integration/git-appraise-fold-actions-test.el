;;; git-appraise-fold-actions-test.el --- Review UI integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)
(require 'git-appraise-test-actions-fixture)

;;; req-2026-11-23-020: fold file at point toggles file section
;; covers: git-appraise-folding.el
;; covers-fn: git-appraise-fold-file-at-point
;; enforcement: test
(ert-deftest git-appraise-fold-actions/req-2026-11-23-020-fold-file-at-point ()
  "WHEN a reviewer folds a file at point THE SYSTEM SHALL toggle the file section visibility."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let ((file (cl-find-if (lambda (s) (eq (oref s type) 'file))
                              (let (all) (magit-map-sections (lambda (s) (push s all))) all))))
        (should file)
        (goto-char (oref file start))
        (should (oref file hidden))
        (git-appraise-fold-file-at-point)
        (should-not (oref file hidden))
        (git-appraise-fold-file-at-point)
        (should (oref file hidden))))))

;;; req-2026-11-24-111: opening a file reveals unchosen hunks
;; covers: git-appraise-folding.el git-appraise-section-wash.el git-appraise-lazy.el
;; covers-fn: git-appraise-section--fold-hunks, git-appraise-wash-insert-file-section
;; enforcement: test
(ert-deftest git-appraise-fold-actions/req-2026-11-24-111-folded-file-layout ()
  "WHEN a reviewer opens a diff THE SYSTEM SHALL keep files collapsed until opened, then reveal code changes without saved fold choices."
  (let ((git-appraise-actions-test-before-request
         (lambda (directory)
           (with-temp-file (expand-file-name "other.txt" directory)
             (insert "another file\n"))
           (git-appraise-test--run directory "git" "add" "other.txt")
           (git-appraise-test--run
            directory "git" "commit" "--amend" "--no-edit" "-q"))))
    (git-appraise-actions-test-with-repo
      (let ((git-appraise-lazy-enabled nil))
        (git-appraise-test--open-and-render revision))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (let (files)
          (magit-map-sections
           (lambda (s) (when (eq (oref s type) 'file) (push s files)))
           magit-root-section)
          (setq files (nreverse files))
          (should (= 2 (length files)))
          (should (--all? (oref it hidden) files))
          (goto-char (oref (cadr files) start))
          (forward-line -1)
          (should (or (not (looking-at-p "^[[:space:]]*$"))
                      (invisible-p (point))))
          (goto-char (oref (car files) start))
          (should (eq (key-binding (kbd "TAB"))
                      'git-appraise-fold-toggle-at-point))
          (call-interactively (key-binding (kbd "TAB")))
          (should-not (oref (car files) hidden))
          (goto-char (oref (car files) content))
          (should (looking-at-p "^[[:space:]]*$"))
          (should-not (invisible-p (point)))
          (should (--any? (magit-hunk-section-p it)
                         (oref (car files) children)))
          (should-not (--any? (and (magit-hunk-section-p it)
                                  (oref it hidden))
                             (oref (car files) children))))))))

;;; req-2026-11-24-112: fold choices survive reopening
;; covers: git-appraise-fold-state.el git-appraise-folding.el
;; covers-fn: git-appraise-fold-state--write, git-appraise-fold-state--prepare
;; enforcement: test
(ert-deftest git-appraise-fold-actions/req-2026-11-24-112-persistent-folds ()
  "WHEN a reviewer changes a file fold THE SYSTEM SHALL restore that choice on a later review opening."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (file)
        (magit-map-sections
         (lambda (s) (when (eq (oref s type) 'file) (setq file s)))
         magit-root-section)
        (should file)
        (goto-char (oref file start))
        (git-appraise-fold-file-at-point)
        (should-not (oref file hidden))
        (should (git-appraise-fold-state--read directory revision))))
    (kill-buffer (git-appraise-pr-buffer-name))
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (file)
        (magit-map-sections
         (lambda (s) (when (eq (oref s type) 'file) (setq file s)))
         magit-root-section)
        (should file)
        (should-not (oref file hidden))))))

;;; req-2026-11-24-113: hunk folds and file-wide actions persist
;; covers: git-appraise-folding.el git-appraise-fold-state.el
;; covers-fn: git-appraise-fold-toggle-at-point, git-appraise-unfold-hunks-in-file
;; enforcement: test
(ert-deftest git-appraise-fold-actions/req-2026-11-24-113-hunk-fold-actions ()
  "WHEN a reviewer opens hunks from a file THE SYSTEM SHALL retain those choices across a reload."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (file hunk)
        (magit-map-sections
         (lambda (s)
           (pcase (oref s type)
             ('file (setq file s))
             ('hunk (setq hunk s))))
         magit-root-section)
        (should (and file hunk))
        (goto-char (oref hunk start))
        (git-appraise-unfold-hunks-in-file)
        (should-not (oref hunk hidden))
        (should-not (oref file hidden))
        (git-appraise-fold-all-files)
        (should (oref file hidden))
        (git-appraise-unfold-all-files)
        (should-not (oref file hidden))))
    (git-appraise-reload)
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (hunk)
        (magit-map-sections
         (lambda (s) (when (eq (oref s type) 'hunk) (setq hunk s)))
         magit-root-section)
        (should hunk)
        (should-not (oref hunk hidden))))))

;;; req-2026-11-24-127: fold toggle repairs hidden overlay drift
;; covers: git-appraise-folding.el
;; covers-fn: git-appraise-fold-toggle-at-point, git-appraise-folding--effective-hidden-p
;; enforcement: test
(ert-deftest git-appraise-fold-actions/req-2026-11-24-127-fold-toggle-repairs-drift ()
  "WHEN a file section is slot-hidden without an invisible overlay THE SYSTEM SHALL fold it on first toggle."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (file)
        (magit-map-sections
         (lambda (s) (when (eq (oref s type) 'file) (setq file s)))
         magit-root-section)
        (should file)
        ;; simulate drift: hidden slot set, but invisible overlay missing
        (oset file hidden t)
        (let ((content (oref file content))
              (end (oref file end)))
          (dolist (ov (overlays-in content end))
            (when (overlay-get ov 'invisible)
              (delete-overlay ov))))
        (should (oref file hidden))
        (should-not (cl-some (lambda (ov) (overlay-get ov 'invisible))
                             (overlays-at (oref file content))))
        (goto-char (oref file start))
        (git-appraise-fold-toggle-at-point)
        ;; first press should fold (visually hidden), not "show hunks"
        (should (oref file hidden))
        (should (cl-some (lambda (ov) (overlay-get ov 'invisible))
                         (overlays-at (oref file content))))))))

;;; git-appraise-fold-actions-test.el ends here
