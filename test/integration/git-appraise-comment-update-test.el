;;; git-appraise-comment-update-test.el --- Review comment integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defun git-appraise-comment-inplace-test--commentable-hunk-lines (buf)
  "Return (POS SIDE LINE LINE-TYPE PATH) for commentable hunk lines in BUF."
  (with-current-buffer buf
    (let (items)
      (magit-map-sections
       (lambda (sec)
         (when (magit-hunk-section-p sec)
           (let* ((file (oref sec parent))
                  (path (and (magit-file-section-p file)
                             (oref file value)))
                  (beg (oref sec start))
                  (end (oref sec end)))
             (save-excursion
               (goto-char beg)
               (while (< (point) end)
                 (let* ((line (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
                        (prefix (and (> (length line) 0) (aref line 0)))
                        (line-type (pcase prefix
                                     (?+ "ADDED")
                                     (?- "REMOVED")
                                     (?  "UNCHANGED")
                                     (_ nil))))
                   (when (and line-type path)
                     (save-excursion
                       (goto-char (line-beginning-position))
                       (let* ((loc (git-appraise--compute-line-side-and-range))
                              (side (plist-get loc :side))
                              (num (plist-get loc :line)))
                         (when (and side num)
                           (push (list (line-beginning-position) side num line-type path line)
                                 items))))))
                 (forward-line 1))))))
       magit-root-section)
      (nreverse items))))

;;; req-2026-11-25-040:
;; covers: git-appraise-comment-draft.el git-appraise-comment.el git-appraise-section-comment.el
;; covers-fn: git-appraise-comment-handler-commit, git-appraise-comment--insert-local-comment-in-place, git-appraise-comment-add-or-edit
;; enforcement: test
(ert-deftest git-appraise-comment-update/req-2026-11-25-040-dynamic-add-no-reload-many-positions ()
  "WHEN a reviewer adds draft comments dynamically at various hunk positions
THE SYSTEM SHALL insert each comment in place without rebuilding or reloading the review."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    ;; Build a clean multi-hunk, multi-file review for corner-case coverage.
    (git-appraise-test--run directory "git" "checkout" "-q" "master")
    (with-temp-file (expand-file-name "example.txt" directory)
      (insert "l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8\nl9\nl10\nl11\nl12\n"))
    (with-temp-file (expand-file-name "other.txt" directory)
      (insert "a\nb\nc\n"))
    (git-appraise-test--run directory "git" "add" ".")
    (git-appraise-test--run directory "git" "commit" "-qm" "base2")
    (git-appraise-test--run directory "git" "checkout" "-qb" "feature2")
    (with-temp-file (expand-file-name "example.txt" directory)
      (insert "l1\nL2\nl3\nl4\nl5\nl6\nl7\nl8\nl9\nL10\nl11\nl12\n"))
    (with-temp-file (expand-file-name "other.txt" directory)
      (insert "a\nB\nc\nD\n"))
    (git-appraise-test--run directory "git" "commit" "-qam" "change2")
    (setq revision (git-appraise-test--run directory "git" "rev-parse" "HEAD"))
    (git-appraise-test--run
     directory "git-appraise" "request" "-quiet" "-m" "Multi hunk" "-target" "refs/heads/master")
    (git-appraise-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (lines (git-appraise-comment-inplace-test--commentable-hunk-lines buf))
           (build-calls 0)
           (full-refreshes 0)
           (orig-build (symbol-function 'git-appraise--build-buffer)))
      (should (> (length lines) 5))
      (cl-letf (((symbol-function 'git-appraise--build-buffer)
                 (lambda (&rest args)
                   (cl-incf build-calls)
                   (when git-appraise-section-full-refresh?
                     (cl-incf full-refreshes))
                   (apply orig-build args)))
                ((symbol-function 'git-appraise-comment-add)
                 (lambda (&optional _msg) nil))
                ((symbol-function 'switch-to-buffer-other-window)
                 (lambda (buf &rest _) (set-buffer buf))))
        (dolist (item (reverse lines))
          (pcase-let ((`(,pos ,side ,num ,line-type ,path ,_text) item))
            (with-current-buffer buf
              (goto-char pos)
              (setq git-appraise-comment-cursor-pos pos
                    git-appraise-comment-commit-buffer? nil
                    git-appraise-comment-send? nil)
              ;; Dynamic add: create comment through the same shape the UI produces.
              (let ((comment (git-appraise-local-comment-section
                              :state "LOCAL COMMENT"
                              :author "Tester"
                              :path path
                              :side side
                              :line num
                              :line-type line-type
                              :send? nil
                              :msg (format "dyn %s:%s:%s" path num line-type))))
                (setq git-appraise-comment-uncommitted comment)
                (git-appraise-comment-handler-commit
                 comment git-appraise-comment-buffer-msg)))))
        (should (= build-calls 0))
        (should (= full-refreshes 0)))
      (with-current-buffer buf
        (dolist (item lines)
          (pcase-let ((`(,_pos ,_side ,num ,line-type ,path ,_text) item))
            (goto-char (point-min))
            (should (search-forward (format "dyn %s:%s:%s" path num line-type) nil t))))))))

;;; req-2026-11-25-041:
;; covers: git-appraise-comment-draft.el git-appraise-comment.el git-appraise-section.el
;; covers-fn: git-appraise-comment-handler-commit, git-appraise-comment-add-or-edit, git-appraise-comment-commit, git-appraise-section--mark-comment-sent-in-place
;; enforcement: test
(ert-deftest git-appraise-comment-update/req-2026-11-25-041-send-flow-no-reload-multi-hunk ()
  "WHEN a reviewer adds an immediately-sent comment through the real UI flow in a multi-hunk file
THE SYSTEM SHALL render the comment in place without rebuilding the review buffer."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--run directory "git" "checkout" "-q" "master")
    (with-temp-file (expand-file-name "example.txt" directory)
      (insert "l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8\nl9\nl10\nl11\nl12\n"))
    (git-appraise-test--run directory "git" "add" ".")
    (git-appraise-test--run directory "git" "commit" "-qm" "base-mh")
    (git-appraise-test--run directory "git" "checkout" "-qb" "feature-mh")
    (with-temp-file (expand-file-name "example.txt" directory)
      (insert "l1\nL2\nl3\nl4\nl5\nl6\nl7\nl8\nl9\nL10\nl11\nl12\n"))
    (git-appraise-test--run directory "git" "commit" "-qam" "change-mh")
    (setq revision (git-appraise-test--run directory "git" "rev-parse" "HEAD"))
    (git-appraise-test--run
     directory "git-appraise" "request" "-quiet" "-m" "Two hunks" "-target" "refs/heads/master")
    (git-appraise-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (build-calls 0)
           (full-refreshes 0)
           (orig-build (symbol-function 'git-appraise--build-buffer)))
      (cl-letf (((symbol-function 'git-appraise--build-buffer)
                 (lambda (&rest args)
                   (cl-incf build-calls)
                   (when git-appraise-section-full-refresh?
                     (cl-incf full-refreshes))
                   (apply orig-build args)))
                ((symbol-function 'switch-to-buffer-other-window)
                 (lambda (b &rest _) (set-buffer b)))
                ((symbol-function 'delete-window) #'ignore)
                ((symbol-function 'delete-frame) #'ignore))
        ;; Comment in middle of first hunk - the reported reload case.
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "^ l4" nil t)
          (git-appraise-comment-add-or-edit))
        (with-current-buffer (get-buffer git-appraise-comment-buffer-name)
          (erase-buffer)
          (insert "send-flow first hunk"))
        (git-appraise-comment-commit)
        ;; Comment in second hunk to cover multiple positions.
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "^+L10" nil t)
          (git-appraise-comment-add-or-edit))
        (with-current-buffer (get-buffer git-appraise-comment-buffer-name)
          (erase-buffer)
          (insert "send-flow second hunk"))
        (git-appraise-comment-commit)
        (should (= build-calls 0))
        (should (= full-refreshes 0))
        (with-current-buffer buf
          (goto-char (point-min))
          (should (search-forward "send-flow first hunk" nil t))
          (goto-char (point-min))
          (should (search-forward "send-flow second hunk" nil t)))))))

;;; req-2026-11-25-044:
;; covers: git-appraise-comment.el
;; covers-fn: git-appraise-comment--commit-text
;; enforcement: test
(ert-deftest git-appraise-comment-update/req-2026-11-25-044-single-comment-no-full-refresh ()
  "WHEN a single conversation comment is sent
THE SYSTEM SHALL show it without a full diff reload."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (pr (git-appraise-db-get-pullreq))
           (full-refreshes 0)
           (orig-build (symbol-function 'git-appraise--build-buffer)))
      (cl-letf (((symbol-function 'git-appraise--build-buffer)
                 (lambda (&rest args)
                   (when git-appraise-section-full-refresh?
                     (cl-incf full-refreshes))
                   (apply orig-build args))))
        (setq git-appraise-comment-single-comment? t)
        (unwind-protect
            (progn
              (git-appraise-comment--commit-text pr "A single issue comment")
              (should (= full-refreshes 0))
              (with-current-buffer buf
                (goto-char (point-min))
                (should (re-search-forward "single[[:space:]]+issue[[:space:]]+comment" nil t))))
          (git-appraise-comment-reset-global-vars))))))

;;; req-2026-11-25-045: draft save does not drain unrelated files
;; covers: git-appraise-lazy.el git-appraise-comment-draft.el
;; covers-fn: git-appraise-lazy--schedule, git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest git-appraise-comment-update/req-2026-11-25-045-save-during-lazy-render ()
  "WHEN a reviewer saves a draft while files are still loading
THE SYSTEM SHALL show it immediately and keep unrelated rendering in the background."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (let ((git-appraise-history-enabled nil)
          (git-appraise-lazy-enabled t))
      (git-appraise-backend-open revision)
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise--build-buffer)))
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (builds 0)
           (expanded 0)
           (original-expand (symbol-function 'git-appraise-lazy--expand-at)))
      (with-current-buffer buf
        (let ((placeholder
               (git-appraise-comment--file-section
                (buffer-name buf) "example.txt")))
          (should (git-appraise-lazy-placeholder-section-p placeholder))
          (should git-appraise-lazy--queue)
          (setq git-appraise-comment-cursor-pos (oref placeholder start)
                git-appraise-comment-commit-buffer? nil)
          (let ((comment (git-appraise-local-comment-section
                          :state "LOCAL COMMENT" :author "Tester"
                          :path "example.txt" :side "RIGHT" :line 2
                          :line-type "ADDED" :send? nil
                          :msg "pending file draft")))
            (cl-letf (((symbol-function 'git-appraise--build-buffer)
                       (lambda (&rest _) (cl-incf builds)))
                      ((symbol-function 'git-appraise-lazy--expand-at)
                       (lambda (section)
                         (cl-incf expanded)
                         (funcall original-expand section))))
              (git-appraise-comment-handler-commit
               comment git-appraise-comment-buffer-msg))
            (should (= builds 0))
            (should (= expanded 0))
            (should git-appraise-lazy--queue)
            (should (string-match-p "pending file draft" (buffer-string)))
            (should (eq (oref (git-appraise-comment--find-section-by-internal-id
                               (buffer-name buf) (oref comment internalId)) parent)
                        placeholder))
            (git-appraise-test--drain-lazy buf)
            (should (= 1 (how-many "pending file draft"
                                    (point-min) (point-max))))
            (should (git-appraise-comment--hunk-ancestor
                     (git-appraise-comment--find-section-by-internal-id
                      (buffer-name buf) (oref comment internalId))))))))))

;;; req-2026-11-25-075: composer helper text never reaches a comment
;; covers: git-appraise-comment.el git-appraise-comment-draft.el
;; covers-fn: git-appraise-comment-strip-known-helpers, git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest git-appraise-comment-update/req-2026-11-25-075-commit-strips-composer-helper ()
  "WHEN a reviewer commits a comment typed into a composer
THE SYSTEM SHALL save it without any composer helper text, whichever
flow opened the composer."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    ;; every known seed strips, in any combination
    (should (equal "real text"
                   (git-appraise-comment-strip-known-helpers
                    (concat git-appraise-comment-buffer-msg "\n"
                            git-appraise-comment-single-comment-msg "\n"
                            git-appraise-comment-feedback-msg "\n"
                            "real text"))))
    (git-appraise-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (git-appraise-section-full-refresh? nil))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "^\\+changed" nil t)
        (setq git-appraise-comment-cursor-pos (line-beginning-position)
              git-appraise-comment-uncommitted
              (git-appraise-local-comment-section
               :state "LOCAL COMMENT"
               :author "Tester"
               :path "example.txt"
               :side "RIGHT"
               :line 2
               :line-type "ADDED"
               :send? nil
               :msg (concat git-appraise-comment-buffer-msg
                            "\ntake a look at line 2"))
              git-appraise-comment-commit-buffer? nil)
        ;; a mismatched flow: the composer holds the standard seed while
        ;; the committing pass strips against the single-comment seed
        (git-appraise-comment-handler-commit
         git-appraise-comment-uncommitted
         git-appraise-comment-single-comment-msg)
        (goto-char (point-min))
        (should (search-forward "take a look at line 2" nil t))
        (goto-char (point-min))
        (should-not (search-forward "Type C-c C-c" nil t))
        (should-not (search-forward "check the transient menu" nil t)))
      (git-appraise-comment-reset-global-vars))))

;;; git-appraise-comment-update-test.el ends here
