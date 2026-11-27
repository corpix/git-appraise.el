;;; git-appraise-review-presentation-test.el --- Review UI integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)
(require 'git-appraise-test-actions-fixture)

;;; req-2026-11-24-117: special diff headers do not block review rendering
;; covers: git-appraise-fold-state.el
;; covers-fn: git-appraise-fold-state--hunk-identity, git-appraise-fold-state--hunk-folded-p
;; enforcement: test
(ert-deftest git-appraise-review-presentation/req-2026-11-24-117-special-hunk-folds ()
  "WHEN a review contains rename or mode headers THE SYSTEM SHALL open without a fold error."
  (--each '(rename chmod (rename) (chmod))
    (let ((section (magit-hunk-section)))
      (oset section value it)
      (should-not (git-appraise-fold-state--hunk-identity section))
      (should (git-appraise-fold-state--hunk-folded-p section)))))

;;; req-2026-11-24-118: author names use Git presentation
;; covers: git-appraise-section-header.el git-appraise-comment-format.el
;; covers-fn: git-appraise-section-insert-author, git-appraise-comment-format-heading
;; enforcement: test
(ert-deftest git-appraise-review-presentation/req-2026-11-24-118-plain-author-name ()
  "WHEN a reviewer opens a review THE SYSTEM SHALL show author names without a mention prefix."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let ((author (a-get-in (git-appraise-db--pullreq-raw-infos)
                              '(author login))))
        (goto-char (point-min))
        (should (re-search-forward
                 (concat "^Author:[ \t]+" (regexp-quote author) "$")
                 nil t))))))

;;; req-2026-11-24-120: file change counts are consistent and optional
;; covers: git-appraise-viewed.el git-appraise-section-wash.el git-appraise-lazy.el
;; covers-fn: git-appraise-viewed--block-line-counts, git-appraise-wash--insert-file-heading
;; enforcement: test
(ert-deftest git-appraise-review-presentation/req-2026-11-24-120-file-change-summary ()
  "WHEN a reviewer scans changed files THE SYSTEM SHALL show consistent optional addition and deletion totals."
  (let ((git-appraise-actions-test-before-request
         (lambda (directory)
           (with-temp-file (expand-file-name "other.txt" directory)
             (insert "another file\n"))
           (git-appraise-test--run directory "git" "add" "other.txt")
           (git-appraise-test--run
            directory "git" "commit" "--amend" "--no-edit" "-q"))))
    (git-appraise-actions-test-with-repo
      (let ((git-appraise-lazy-enabled t)
            (git-appraise-lazy-threshold 1)
            (git-appraise-show-file-change-summary t))
        (git-appraise-test--open-and-render revision)
        (with-current-buffer (git-appraise-pr-buffer-name)
          (let ((text (buffer-string)))
            (should (string-match-p "^modified[ \t]+\\+2 -1[ \t]+example\\.txt" text))
            (should (string-match-p "^new file[ \t]+\\+1 -0[ \t]+other\\.txt" text))
            (should-not (string-match-p "load with TAB" text))))
        (let ((git-appraise-show-file-change-summary nil))
          (git-appraise-test--open-and-render revision)
          (with-current-buffer (git-appraise-pr-buffer-name)
            (should-not (string-match-p (regexp-quote "+2 -1")
                                        (buffer-string)))))))))

;;; req-2026-11-24-121: classification precedes aligned file names
;; covers: git-appraise-section-wash.el git-appraise-lazy.el
;; covers-fn: git-appraise-wash--insert-file-heading
;; enforcement: test
(ert-deftest git-appraise-review-presentation/req-2026-11-24-121-file-heading-columns ()
  "WHEN a review mixes classified and ordinary files THE SYSTEM SHALL align their names after plain classification and change columns."
  (let ((git-appraise-actions-test-before-request
         (lambda (directory)
           (with-temp-file (expand-file-name "flake.lock" directory)
             (insert "generated\n"))
           (git-appraise-test--run directory "git" "add" "flake.lock")
           (git-appraise-test--run
            directory "git" "commit" "--amend" "--no-edit" "-q"))))
    (git-appraise-actions-test-with-repo
      (let ((git-appraise-lazy-enabled t)
            (git-appraise-lazy-threshold 1)
            (git-appraise-show-file-change-summary t))
        (git-appraise-test--open-and-render revision))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (let (ordinary-column generated-column)
          (goto-char (point-min))
          (should (re-search-forward
                   "^modified[ \t]+\\+2 -1[ \t]+example\\.txt" nil t))
          (setq ordinary-column (- (current-column) (length "example.txt")))
          (goto-char (point-min))
          (should (re-search-forward
                   "^new file[ \t]+GEN[ \t]+\\+1 -0[ \t]+flake\\.lock" nil t))
          (setq generated-column (- (current-column) (length "flake.lock")))
          (should (= ordinary-column generated-column))
          (should-not (string-match-p "\\[GEN\\]" (buffer-string))))))))

;;; req-2026-11-24-124: change totals use diff coloring
;; covers: git-appraise-section-wash.el
;; covers-fn: git-appraise-wash--insert-file-heading
;; enforcement: test
(ert-deftest git-appraise-review-presentation/req-2026-11-24-124-plain-change-totals ()
  "WHEN a reviewer scans file change totals THE SYSTEM SHALL color additions and deletions distinctly, and de-emphasize zero counts."
  (with-temp-buffer
    (let ((git-appraise-viewed--buffer-inventory
           (make-hash-table :test #'equal))
          (git-appraise-show-file-change-summary t))
      (puthash "example.txt" '(:added 2 :deleted 1)
               git-appraise-viewed--buffer-inventory)
      (cl-letf (((symbol-function 'git-appraise-history--file-count)
                 (lambda (_) 0)))
        (git-appraise-wash--insert-file-heading
         "modified" "example.txt" "example.txt" nil))
      (goto-char (point-min))
      (search-forward "+2")
      (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                  'magit-diff-added))
      (search-forward "-1")
      (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                  'magit-diff-removed))))
  (with-temp-buffer
    (let ((git-appraise-viewed--buffer-inventory
           (make-hash-table :test #'equal))
          (git-appraise-show-file-change-summary t))
      (puthash "example.txt" '(:added 0 :deleted 3)
               git-appraise-viewed--buffer-inventory)
      (cl-letf (((symbol-function 'git-appraise-history--file-count)
                 (lambda (_) 0)))
        (git-appraise-wash--insert-file-heading
         "modified" "example.txt" "example.txt" nil))
      (goto-char (point-min))
      (search-forward "+0")
      (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                  'git-appraise-dimmed))
      (search-forward "-3")
      (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                  'magit-diff-removed)))))

;;; req-2026-11-24-126: file report metadata is readable on separate lines
;; covers: git-appraise-section-wash.el git-appraise-section-shared.el
;; covers-fn: git-appraise-section-insert-files-changed, git-appraise-section--focus-summary
;; enforcement: test
(ert-deftest git-appraise-review-presentation/req-2026-11-24-126-file-report-lines ()
  "WHEN a review has hidden files and viewed progress THE SYSTEM SHALL show each summary on a separate plain line."
  (let ((git-appraise-actions-test-before-request
         (lambda (directory)
           (with-temp-file (expand-file-name "README.md" directory)
             (insert "documentation\n"))
           (git-appraise-test--run directory "git" "add" "README.md")
           (git-appraise-test--run
            directory "git" "commit" "--amend" "--no-edit" "-q"))))
    (git-appraise-actions-test-with-repo
      (let ((git-appraise-focus-mode t))
        (git-appraise-test--open-and-render revision))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (should (re-search-forward "^Files changed.*$" nil t))
        (should-not (string-match-p "hidden\\|viewed"
                                    (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
        (forward-line 1)
        (should (looking-at "^  [0-9]+ files? hidden"))
        (re-search-forward "files? hidden")
        (should-not (get-text-property (match-beginning 0) 'font-lock-face))
        (should-not (invisible-p (point)))
        (forward-line 1)
        (should (looking-at "^  [0-9]+/[0-9]+ viewed"))
        (re-search-forward "viewed")
        (should-not (get-text-property (match-beginning 0) 'font-lock-face))
        (should-not (invisible-p (point)))))))

;;; req-2026-11-24-114: failed fold writes preserve display
;; covers: git-appraise-fold-state.el git-appraise-folding.el
;; covers-fn: git-appraise-fold-state--write, git-appraise-fold-file-at-point
;; enforcement: test
(ert-deftest git-appraise-review-presentation/req-2026-11-24-114-fold-write-failure ()
  "WHEN a fold choice cannot be saved THE SYSTEM SHALL keep its previous visibility."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (file)
        (magit-map-sections
         (lambda (s) (when (eq (oref s type) 'file) (setq file s)))
         magit-root-section)
        (goto-char (oref file start))
        (should (oref file hidden))
        (cl-letf (((symbol-function 'git-appraise-store-append-events)
                   (lambda (&rest _) (error "write failed"))))
          (should-error (git-appraise-fold-file-at-point) :type 'user-error))
        (should (oref file hidden))))))

;;; req-2026-11-23-021: comment add-or-edit opens composer on hunk
;; covers: git-appraise-comment.el
;; covers-fn: git-appraise-comment-add-or-edit
;; enforcement: test
(ert-deftest git-appraise-review-presentation/req-2026-11-23-021-comment-add-or-edit-on-hunk ()
  "WHEN a reviewer adds a comment at a hunk THE SYSTEM SHALL open the comment composer."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (unwind-protect
        (with-current-buffer (git-appraise-pr-buffer-name)
          (magit-section-show-level-4)
          (goto-char (point-min))
          (search-forward "+changed")
          (beginning-of-line)
          (git-appraise-comment-add-or-edit)
          (should (get-buffer git-appraise-comment-buffer-name)))
      (when (get-buffer git-appraise-comment-buffer-name)
        (kill-buffer git-appraise-comment-buffer-name))
      (git-appraise-comment-reset-global-vars))))

;;; git-appraise-review-presentation-test.el ends here
