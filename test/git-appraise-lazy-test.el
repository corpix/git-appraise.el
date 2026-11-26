;;; git-appraise-lazy-test.el --- ERT tests for lazy diff washing -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for lazy diff washing behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise-lazy)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

;;; req-2026-11-23-011: lazy wash preserves point
;; covers: git-appraise-lazy.el
;; covers-fn: git-appraise-lazy--wash-step
;; enforcement: test
(ert-deftest git-appraise-lazy/req-2026-11-23-011-wash-step-preserves-point ()
  "WHEN lazy wash processes a diff block
THE SYSTEM SHALL preserve the user's point in the review buffer."
  (let ((buf (generate-new-buffer " *git-appraise-lazy-test*")))
    (unwind-protect
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "line1\nline2\nline3\nline4\nline5\n")
            (goto-char (point-min))
            (forward-line 1))
          (let ((saved (point))
                (epoch (cons 'git-appraise-lazy t)))
            (setq-local git-appraise-lazy--epoch epoch
                        git-appraise-lazy--queue nil
                        git-appraise-lazy--wash-pos (copy-marker (point-min))
                        git-appraise-lazy--wash-limit (copy-marker (point-max))
                        git-appraise-lazy--insert-parent nil
                        git-appraise-lazy--end-fixups nil)
            (cl-letf (((symbol-function 'git-appraise-lazy--wash-block)
                       (lambda ()
                         (goto-char (point-max))
                         (set-marker git-appraise-lazy--wash-pos (point))
                         t)))
              (git-appraise-lazy--wash-step buf epoch)
              (should (= (point) saved)))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;; req-2026-11-24-004: concurrent reviews retain their own comment context
;; covers: git-appraise-section.el git-appraise-section-shared.el git-appraise-lazy.el
;; covers-fn: git-appraise-lazy--wash-step
;; enforcement: test
(ert-deftest git-appraise-lazy/req-2026-11-24-004-isolate-review-context ()
  "WHEN two reviews render in turn THE SYSTEM SHALL keep each review's comments attached to its own diff."
  (let ((first (generate-new-buffer " *git-appraise-first-review*"))
        (second (generate-new-buffer " *git-appraise-second-review*"))
        seen)
    (unwind-protect
        (progn
          (--each `((,first first) (,second second))
            (with-current-buffer (car it)
              (insert "diff block\n")
              (setq-local git-appraise-lazy--epoch (cons 'epoch nil)
                          git-appraise-lazy--review-id (cadr it)
                          git-appraise-section-grouped-comments (list (cadr it))
                          git-appraise-lazy--wash-pos (copy-marker (point-min))
                          git-appraise-lazy--wash-limit (copy-marker (point-max)))))
          (let ((git-appraise-db--pullreq-id 'second))
            (cl-letf (((symbol-function 'git-appraise-lazy--wash-block)
                       (lambda ()
                         (push (list git-appraise-db--pullreq-id
                                     git-appraise-section-grouped-comments)
                               seen)
                         (goto-char (point-max))
                         t)))
              (git-appraise-lazy--wash-step
               first (buffer-local-value 'git-appraise-lazy--epoch first))
              (git-appraise-lazy--wash-step
               second (buffer-local-value 'git-appraise-lazy--epoch second))))
          (should (equal (nreverse seen)
                         '((first (first)) (second (second))))))
      (--each (list first second)
        (when (buffer-live-p it)
          (with-current-buffer it
            (git-appraise-lazy--invalidate))
          (kill-buffer it))))))

;;; req-2026-11-24-005: background render leaves the cursor alone
;; covers: git-appraise-lazy.el
;; covers-fn: git-appraise-lazy--expand-step
;; enforcement: test
(ert-deftest git-appraise-lazy/req-2026-11-24-005-drain-leaves-point ()
  "WHEN a background review finishes
THE SYSTEM SHALL leave point where the render or the reviewer put it."
  (let ((buf (generate-new-buffer " *git-appraise-point-review*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "first\nsecond\n")
          (setq-local git-appraise-lazy--epoch (cons 'epoch nil)
                      git-appraise-lazy--review-id 'review
                      git-appraise-lazy--queue nil)
          (goto-char (point-max))
          (let ((git-appraise-comment-cursor-pos (point-min)))
            (git-appraise-lazy--expand-step buf git-appraise-lazy--epoch))
          (should (= (point) (point-max))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;; req-2026-11-25-073: navigation during background loading wins
;; covers: git-appraise-lazy.el
;; covers-fn: git-appraise-lazy--expand-at
;; enforcement: test
(ert-deftest git-appraise-lazy/req-2026-11-25-073-background-load-keeps-navigation ()
  "WHEN the reviewer moves while files load in the background
THE SYSTEM SHALL leave the cursor on the line they navigated to."
  (git-appraise-viewed-e2e-test--with-review
    (let ((git-appraise-history-enabled nil)
          (git-appraise-lazy-enabled t)
          (git-appraise-lazy-threshold 10))
      (git-appraise-backend-open revision)
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise--build-buffer))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (re-search-forward "mode\\.sh" nil t)
        (beginning-of-line))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (should (bolp))
        (should (looking-at-p "modified[ \t]+.*mode\\.sh"))))))

;;; req-2026-11-25-074: re-render returns to the same logical line
;; covers: git-appraise-lazy.el git-appraise-section.el git-appraise-actions.el
;; covers-fn: git-appraise-lazy--position-chain, git-appraise-lazy--restore-position, git-appraise-lazy--settle-position
;; enforcement: test
(ert-deftest git-appraise-lazy/req-2026-11-25-074-rerender-returns-to-line ()
  "WHEN a review re-renders while the reviewer keeps still
THE SYSTEM SHALL return the cursor to the same line, even when that
file is still background-loading or another file shifted the layout."
  (git-appraise-viewed-e2e-test--with-review
    (let ((git-appraise-history-enabled nil)
          (git-appraise-lazy-enabled t)
          (git-appraise-lazy-threshold 10))
      (git-appraise-backend-open revision)
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise--build-buffer))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      ;; reload onto a line inside a file that loads in the background
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (re-search-forward "^ l4" nil t)
        (beginning-of-line)
        (git-appraise-reload))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (should (bolp))
        (should (looking-at-p " l4")))
      ;; a new file above shifts every byte offset; the line survives
      (with-temp-file (expand-file-name "aaa.txt" directory)
        (insert "a\nb\nc\n"))
      (git-appraise-test--run directory "git" "add" "aaa.txt")
      (git-appraise-test--run directory "git" "commit" "-qm" "add aaa")
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (re-search-forward "^ l4" nil t)
        (beginning-of-line)
        (git-appraise-reload))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (should (bolp))
        (should (looking-at-p " l4"))))))

(provide 'git-appraise-lazy-test)
;;; git-appraise-lazy-test.el ends here
