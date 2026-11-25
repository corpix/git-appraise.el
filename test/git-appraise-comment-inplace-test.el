;;; git-appraise-comment-inplace-test.el --- ERT tests for in-place comment updates -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify local comment mutations update the rendered review in place.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defmacro git-appraise-comment-inplace-test-with-repo (&rest body)
  "Run BODY in a disposable git-appraise repository with one review."
  (declare (indent 0))
  `(git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
     ,@body))

(defun git-appraise-comment-inplace-test--open-and-render (revision)
  "Open REVISION and render it fully."
  (let ((git-appraise-history-enabled nil))
    (git-appraise-backend-open revision)
    (let ((git-appraise-section-full-refresh? t))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))))

;;; req-2026-11-24-116: historical comments retain original context
;; covers: git-appraise-backend.el git-appraise-section-history.el git-appraise-section-wash.el
;; covers-fn: git-appraise-backend--normalize, git-appraise-history-insert-for-file
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-116-historical-comment-context ()
  "WHEN an earlier line comment remains after a review changes
THE SYSTEM SHALL show its original location and content beside the affected file."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Earlier line"
     "-f" "example.txt" "-l" "2" revision)
    (with-temp-file (expand-file-name "example.txt" directory)
      (insert "first\nchanged again\nthird\n"))
    (git-appraise-test--run directory "git" "commit" "-qam" "followup")
    (git-appraise-comment-inplace-test--open-and-render revision)
    (with-current-buffer (get-buffer (git-appraise-pr-buffer-name))
      (should (string-match-p "💬1" (buffer-string)))
      (should (string-match-p "Earlier comment by .*example.txt:2" (buffer-string)))
      (should (string-match-p "Earlier line" (buffer-string)))
      (should-not (string-match-p "^  changed$" (buffer-string)))
      (should (eq (git-appraise-history--marker-face
                   '((state . "CHANGES_REQUESTED")))
                  'git-appraise-historical-rejected-face))
      (should (eq (git-appraise-history--marker-face
                   '((state . "APPROVED")))
                  'git-appraise-historical-resolved-face)))))

;;; req-2026-11-23-012: local comment add updates in place
;; covers: git-appraise-comment-draft.el git-appraise-section-comment.el
;; covers-fn: git-appraise-comment-handler-commit, git-appraise-comment--insert-local-comment-in-place
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-23-012-add-local-comment-in-place ()
  "WHEN a reviewer commits a local draft comment
THE SYSTEM SHALL show the comment in the review buffer without rebuilding the diff."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (build-calls 0)
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
                        :msg "in-place comment")))
          (setq git-appraise-comment-cursor-pos pos
                git-appraise-comment-uncommitted comment
                git-appraise-comment-commit-buffer? nil)
          (cl-letf (((symbol-function 'git-appraise--build-buffer)
                     (lambda (&rest _) (cl-incf build-calls))))
            (git-appraise-comment-handler-commit
             comment git-appraise-comment-buffer-msg))
          (should (= build-calls 0))
          (goto-char (point-min))
          (should (search-forward "in-place comment" nil t)))))))

;;; req-2026-11-23-013: local comment edit updates in place
;; covers: git-appraise-comment-draft.el git-appraise-section-comment.el
;; covers-fn: git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-23-013-edit-local-comment-in-place ()
  "WHEN a reviewer edits a local draft comment
THE SYSTEM SHALL update the comment in place without rebuilding the diff."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
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
                        :msg "before edit")))
          (setq git-appraise-comment-cursor-pos pos
                git-appraise-comment-uncommitted comment
                git-appraise-comment-commit-buffer? nil)
          (git-appraise-comment-handler-commit comment git-appraise-comment-buffer-msg)
          (let ((build-calls 0))
            (oset comment msg "after edit")
            (oset comment edit? t)
            (setq git-appraise-comment-cursor-pos pos
                  git-appraise-comment-uncommitted comment)
            (cl-letf (((symbol-function 'git-appraise--build-buffer)
                       (lambda (&rest _) (cl-incf build-calls))))
              (git-appraise-comment-handler-commit
               comment git-appraise-comment-buffer-msg))
            (should (= build-calls 0))
            (goto-char (point-min))
            (should (search-forward "after edit" nil t))
            (goto-char (point-min))
            (should-not (search-forward "before edit" nil t))))))))

;;; req-2026-11-23-014: reply draft updates in place
;; covers: git-appraise-comment-draft.el git-appraise-section-comment.el
;; covers-fn: git-appraise-comment-handler-commit, git-appraise-comment--insert-reply-in-place
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-23-014-reply-draft-in-place ()
  "WHEN a reviewer commits a draft reply
THE SYSTEM SHALL show the reply in place without rebuilding the diff."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
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
                        :msg "parent comment")))
          (oset comment id 42)
          (setq git-appraise-comment-cursor-pos pos
                git-appraise-comment-uncommitted comment
                git-appraise-comment-commit-buffer? nil)
          (git-appraise-comment-handler-commit comment git-appraise-comment-buffer-msg)
          (let ((build-calls 0)
                (reply (git-appraise-reply-comment-section
                        :state "COMMENTED"
                        :author "Tester"
                        :path "example.txt"
                        :position 3
                        :diffHunk "@@ -1,2 +1,3 @@"
                        :id 42
                        :msg "reply in place")))
            (setq git-appraise-comment-uncommitted reply)
            (cl-letf (((symbol-function 'git-appraise--build-buffer)
                       (lambda (&rest _) (cl-incf build-calls)))
                      ((symbol-function 'git-appraise-backend-send-reply)
                       (lambda (&rest _) nil)))
              (git-appraise-comment-handler-commit
               reply git-appraise-comment-buffer-msg))
            (should (= build-calls 0))
            (goto-char (point-min))
            (should (search-forward "reply in place" nil t))))))))

;;; req-2026-11-23-018: repeated local comment edit does not duplicate heading
;; covers: git-appraise-comment-draft.el git-appraise-section-comment.el
;; covers-fn: git-appraise-comment-handler-commit, git-appraise-comment--replace-local-comment-in-place
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-23-018-repeated-edit-no-duplicate-heading ()
  "WHEN a reviewer edits a local draft comment multiple times
THE SYSTEM SHALL keep a single comment heading in the review buffer."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
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
                        :msg "first")))
          (setq git-appraise-comment-cursor-pos pos
                git-appraise-comment-uncommitted comment
                git-appraise-comment-commit-buffer? nil)
          (git-appraise-comment-handler-commit comment git-appraise-comment-buffer-msg)
          (dotimes (i 3)
            (oset comment msg (format "edit-%d" i))
            (oset comment edit? t)
            (setq git-appraise-comment-cursor-pos pos
                  git-appraise-comment-uncommitted comment)
            (git-appraise-comment-handler-commit comment git-appraise-comment-buffer-msg))
          (goto-char (point-min))
          (let ((count 0))
            (while (search-forward "Comment by YOU" nil t)
              (cl-incf count))
            (should (= count 1)))
          (goto-char (point-min))
          (should (search-forward "edit-2" nil t)))))))

;;; req-2026-11-23-019: comment helper text is stripped literally
;; covers: git-appraise-utils.el
;; covers-fn: git-appraise-utils--comment-clean-msg
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-23-019-clean-msg-strips-helper-literally ()
  "WHEN a comment buffer contains the helper text with regexp metacharacters
THE SYSTEM SHALL remove the helper text from the saved comment."
  (let ((msg (concat git-appraise-comment-buffer-msg "\n\nhello world")))
    (should (equal "hello world"
                   (git-appraise-utils--comment-clean-msg
                    msg git-appraise-comment-buffer-msg)))))

;;; req-2026-11-23-020: comment bodies render as stored
;; covers: git-appraise-section-comment.el
;; covers-fn: git-appraise-comment--insert-you-lines
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-23-020-render-strips-known-helpers ()
  "WHEN a comment is placed in the review buffer THE SYSTEM SHALL render its stored body without stripping comment helper text."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (obj (git-appraise-local-comment-section
                :author "YOU"
                :state "LOCAL"
                :createdAt "2026-11-24T10:01:00Z"
                :msg (concat git-appraise-comment-buffer-msg "\nactual comment")
                :path "example.txt"
                :position 1
                :line-type 'new
                :send? nil
                :internalId "test-id"
                :id "test-id")))
      (magit-insert-section (root)
        (git-appraise-comment--insert-you-lines obj 'git-appraise-local-comment-section))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Type C-c C-c to include your comment locally" text))
        (should (string-match-p "actual comment" text))))))

;;; req-2026-11-24-008: inline comments remain visually distinct
;; covers: git-appraise-section-comment.el
;; covers-fn: git-appraise-section--decorate-inline-comment
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-008-inline-borders ()
  "WHEN a reviewer reads comments in a diff THE SYSTEM SHALL distinguish them from code and allow that separation to be disabled."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Sent inline"
     "-f" "example.txt" "-l" "2" revision)
    (let ((git-appraise-lazy-enabled nil)
          (git-appraise-inline-comment-border-color "#ff0000"))
      (git-appraise-comment-inplace-test--open-and-render revision)
      (let ((buf (get-buffer (git-appraise-pr-buffer-name))))
        (with-current-buffer buf
          (let ((rules (cl-remove-if-not
                        (lambda (ov)
                          (overlay-get ov 'git-appraise-comment-border))
                        (overlays-in (point-min) (point-max)))))
            (should (= (length rules) 2))
            (let ((top (cl-find-if
                        (lambda (ov)
                          (eq (overlay-get ov 'git-appraise-comment-border)
                              'top))
                        rules))
                  (bottom (cl-find-if
                           (lambda (ov)
                             (eq (overlay-get ov 'git-appraise-comment-border)
                                 'bottom))
                           rules)))
              (should (string-match-p
                       "Comment by"
                       (buffer-substring-no-properties
                        (overlay-start top) (overlay-end top))))
              (should (string-match-p
                       "inline"
                       (buffer-substring-no-properties
                        (overlay-start bottom) (overlay-end bottom)))))
            (should (cl-some (lambda (ov)
                               (equal (plist-get (overlay-get ov 'face)
                                                 :overline)
                                      "#ff0000"))
                             rules))))
        (git-appraise-db--pullreq-raw-comments-update
         '((author (login . "Tester"))
           (state . "LOCAL COMMENT")
           (comments (nodes ((internal-id . "draft-border")
                             (bodyText . "Draft inline")
                             (path . "example.txt")
                             (position . 3)
                             (line-type . "ADDED")
                             (local? . t))))))
        (let ((git-appraise-section-full-refresh? nil))
          (git-appraise--build-buffer))
        (with-current-buffer buf
          (should (= (--count (overlay-get it 'git-appraise-comment-border)
                               (overlays-in (point-min) (point-max)))
                     4)))
        (let ((git-appraise-inline-comment-border-style nil)
              (git-appraise-section-full-refresh? nil))
          (git-appraise--build-buffer))
        (with-current-buffer buf
          (should-not (--any? (overlay-get it 'git-appraise-comment-border)
                              (overlays-in (point-min) (point-max)))))))))

;;; req-2026-11-24-011: folded hunks reveal hidden comments
;; covers: git-appraise-folding.el git-appraise-faces.el
;; covers-fn: git-appraise-folding--mark-commented-hunk
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-011-folded-hunk-comment-marker ()
  "WHEN a reviewer folds a hunk with comments THE SYSTEM SHALL signal the hidden discussion and allow that signal to be disabled."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Sent inline"
     "-f" "example.txt" "-l" "2" revision)
    (let ((git-appraise-lazy-enabled nil)
          (git-appraise-fold-hunks-by-default t))
      (git-appraise-comment-inplace-test--open-and-render revision)
      (with-current-buffer (get-buffer (git-appraise-pr-buffer-name))
        (let (hunk)
          (magit-map-sections
           (lambda (s)
             (when (eq (oref s type) 'hunk)
               (setq hunk s))))
          (should hunk)
          (should (oref hunk hidden))
          (should (= 1 (--count (overlay-get it 'git-appraise-commented-hunk)
                                (overlays-in (point-min) (point-max)))))
          (magit-section-show hunk)
          (should-not (--any? (overlay-get it 'git-appraise-commented-hunk)
                              (overlays-in (point-min) (point-max))))
          (let ((git-appraise-commented-hunk-indicator nil))
            (magit-section-hide hunk)
            (should-not (--any? (overlay-get it 'git-appraise-commented-hunk)
                                (overlays-in (point-min) (point-max))))))))))

;;; req-2026-11-24-101: last-line comment stays inside a folded hunk
;; covers: git-appraise-comment-draft.el git-appraise-folding.el
;; covers-fn: git-appraise-comment--absorb-in-hunk, git-appraise-comment--insert-local-comment-in-place
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-101-last-line-comment-folds ()
  "WHEN a draft comment is committed on a hunk's final line and the hunk is folded THE SYSTEM SHALL conceal the comment heading and body inside the hunk's fold range."
  (git-appraise-comment-inplace-test-with-repo
    (let ((git-appraise-lazy-enabled nil))
      (git-appraise-comment-inplace-test--open-and-render revision)
      (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
             (git-appraise-section-full-refresh? nil)
             (build-calls 0)
             (comment nil)
             (pos nil)
             (heading-positions
              (lambda (comment)
                (with-current-buffer buf
                  (let ((sec (--first (and (slot-boundp it 'value)
                                           (eq (oref it value) comment)
                                           (magit-hunk-section-p
                                            (oref it parent)))
                                      (let (found)
                                        (magit-map-sections
                                         (lambda (s)
                                           (when (and (slot-boundp s 'value)
                                                      (eq (oref s value) comment))
                                             (push s found)))
                                         magit-root-section)
                                        found))))
                    (and sec
                         (list (progn (goto-char (oref sec start))
                                      (skip-chars-forward "\n")
                                      (point))
                               (progn (goto-char (oref sec start))
                                      (search-forward
                                       (oref comment msg) (oref sec end))
                                      (match-beginning 0))))))))
             (fold-check
              (lambda ()
                (with-current-buffer buf
                  (let (hunk)
                    (magit-map-sections
                     (lambda (s)
                       (when (eq (oref s type) 'hunk) (setq hunk s)))
                     magit-root-section)
                    (should hunk)
                    (magit-section-hide hunk)
                    (let ((pos (funcall heading-positions comment)))
                      (should pos)
                      (should (invisible-p (nth 0 pos)))
                      (should (invisible-p (nth 1 pos))))
                    (magit-section-show hunk)
                    (let ((pos (funcall heading-positions comment)))
                      (should (not (invisible-p (nth 0 pos))))
                      (should (not (invisible-p (nth 1 pos))))))))))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "^\\+third" nil t)
          (setq pos (line-beginning-position)
                comment (git-appraise-local-comment-section
                         :state "LOCAL COMMENT"
                         :author "Tester"
                         :path "example.txt"
                         :side "RIGHT"
                         :line 3
                         :line-type "ADDED"
                         :send? nil
                         :msg "in-place comment")
                git-appraise-comment-cursor-pos pos
                git-appraise-comment-uncommitted comment
                git-appraise-comment-commit-buffer? nil)
          (cl-letf (((symbol-function 'git-appraise--build-buffer)
                     (lambda (&rest _) (cl-incf build-calls))))
            (git-appraise-comment-handler-commit
             comment git-appraise-comment-buffer-msg))
          ;; the in-place path succeeded (no rebuild)
          (should (= build-calls 0))
          ;; the draft is a child of the intended hunk, entirely
          ;; inside the hunk's content/end range
          (with-current-buffer buf
            (let (hunk csec)
              (magit-map-sections
               (lambda (s)
                 (when (eq (oref s type) 'hunk) (setq hunk s))
                 (when (and (slot-boundp s 'value) (eq (oref s value) comment)
                            (magit-hunk-section-p (oref s parent))
                            (not csec))
                   (setq csec s)))
               magit-root-section)
              (should hunk)
              (should csec)
              (should (>= (oref csec start) (oref hunk content)))
              (should (<= (oref csec end) (oref hunk end)))))
          ;; fold conceals the whole comment, opening reveals it
          (funcall fold-check)
          ;; regression control: an interior-line comment folds too
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "^\\+changed" nil t)
            (setq pos (line-beginning-position)
                  comment (git-appraise-local-comment-section
                           :state "LOCAL COMMENT"
                           :author "Tester"
                           :path "example.txt"
                           :side "RIGHT"
                           :line 2
                           :line-type "ADDED"
                           :send? nil
                           :msg "interior comment")
                  git-appraise-comment-cursor-pos pos
                  git-appraise-comment-uncommitted comment
                  git-appraise-comment-commit-buffer? nil)
            (cl-letf (((symbol-function 'git-appraise--build-buffer)
                       (lambda (&rest _) (cl-incf build-calls))))
              (git-appraise-comment-handler-commit
               comment git-appraise-comment-buffer-msg)))
          (should (= build-calls 0))
          (funcall fold-check)
          ;; a fresh render must not reintroduce the drift: reload (not
          ;; reopen, which discards unsent drafts) rebuilds the tree
          ;; with NEW comment objects, so locate the draft by text
          (with-current-buffer buf
            (let ((git-appraise-history-enabled nil))
              (git-appraise-reload)))
          (git-appraise-test--drain-lazy buf)
          (with-current-buffer buf
            (let (hunk)
              (magit-map-sections
               (lambda (s) (when (eq (oref s type) 'hunk) (setq hunk s)))
               magit-root-section)
              (should hunk)
              (magit-section-hide hunk)
              (goto-char (point-min))
              (search-forward "in-place comment")
              (let ((body (match-beginning 0)))
                (goto-char body)
                (forward-line -1)
                (should (invisible-p (line-beginning-position)))
                (should (invisible-p body)))
              (magit-section-show hunk)
              (goto-char (point-min))
              (search-forward "in-place comment")
              (let ((body (match-beginning 0)))
                (goto-char body)
                (forward-line -1)
                (should (not (invisible-p (line-beginning-position))))
                (should (not (invisible-p body)))))))))))

;;; req-2026-11-24-102: comment added while the hunk is already folded
;; covers: git-appraise-comment-draft.el git-appraise-folding.el
;; covers-fn: git-appraise-comment--absorb-in-hunk
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-102-folded-hunk-conceals-new-comment ()
  "WHEN a draft is committed on a hunk's final line while the hunk is already folded THE SYSTEM SHALL conceal the new comment immediately and keep the collapsed-comment indicator."
  (git-appraise-comment-inplace-test-with-repo
    (let ((git-appraise-lazy-enabled nil)
          (git-appraise-fold-hunks-by-default t))
      (git-appraise-comment-inplace-test--open-and-render revision)
      (let ((buf (get-buffer (git-appraise-pr-buffer-name)))
            pos
            comment)
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "^\\+third" nil t)
          (setq pos (match-beginning 0)
                comment (git-appraise-local-comment-section
                         :state "LOCAL COMMENT"
                         :author "Tester"
                         :path "example.txt"
                         :side "RIGHT"
                         :line 3
                         :line-type "ADDED"
                         :send? nil
                         :msg "folded comment"
                         )
                git-appraise-comment-cursor-pos pos
                git-appraise-comment-uncommitted comment
                git-appraise-comment-commit-buffer? nil)
          (git-appraise-comment-handler-commit
           comment git-appraise-comment-buffer-msg))
        (with-current-buffer buf
          (let (hunk csec)
            (magit-map-sections
             (lambda (s)
               (when (eq (oref s type) 'hunk) (setq hunk s))
               (when (and (slot-boundp s 'value) (eq (oref s value) comment)
                          (magit-hunk-section-p (oref s parent))
                          (not csec))
                 (setq csec s)))
             magit-root-section)
            (should hunk)
            (should (oref hunk hidden))
            (should csec)
            ;; the new comment is inside the fold range ...
            (should (>= (oref csec start) (oref hunk content)))
            (should (<= (oref csec end) (oref hunk end)))
            ;; ... and invisible right away, without a rebuild
            (should (invisible-p (oref csec start)))
            (should (invisible-p (progn (goto-char (oref csec start))
                                        (search-forward "folded comment"
                                                        (oref csec end))
                                        (match-beginning 0))))
            ;; the collapsed-comment indicator reflects the new comment
            (should (= 1 (--count (overlay-get it 'git-appraise-commented-hunk)
                                  (overlays-in (point-min) (point-max)))))))))))

;;; req-2026-11-24-103: file comment sends without a full refresh
;; covers: git-appraise-comment-draft.el git-appraise-backend.el git-appraise-section-comment.el
;; covers-fn: git-appraise-backend-refresh-comments
;; enforcement: test
(defun git-appraise-comment-inplace-test--flatten-comment-nodes ()
  "The conversation nodes currently stored for the review."
  (a-get-in (git-appraise-db--pullreq-raw-infos) '(comments nodes)))
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-103-file-comment-no-refresh ()
  "WHEN a file comment is sent from an ordinary or renamed file heading THE SYSTEM SHALL store it once and show it in Conversation without a full refresh or diff reload."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (build-calls 0)
           (full-refresh-calls 0)
           (diff-reads 0)
           (point-before 0))
      (with-current-buffer buf
        (setq point-before (point)))
      (let ((orig-run (symbol-function 'git-appraise-backend--run))
            (orig-build (symbol-function 'git-appraise--build-buffer)))
        (fset 'git-appraise-backend--run
              (lambda (directory &rest args)
                (when (member "-diff" args) (cl-incf diff-reads))
                (apply orig-run directory args)))
        ;; count full refreshes; still run the real build so the local
        ;; re-render actually happens
        (fset 'git-appraise--build-buffer
              (lambda (&optional buf-name &rest _)
                (when git-appraise-section-full-refresh?
                  (cl-incf build-calls))
                (funcall orig-build buf-name)))
        (unwind-protect
            (cl-letf (((symbol-function 'git-appraise-backend-refresh)
                       (lambda (&rest _) (cl-incf full-refresh-calls))))
              ;; the object `RET' on an ordinary file heading creates
              (let ((comment (git-appraise-local-comment-section
                              :state "LOCAL COMMENT"
                              :author "tester@example.com"
                              :path "example.txt"
                              :position 0
                              :line-type "FILE"
                              :send? t
                              :msg "file level note")))
                (setq git-appraise-comment-uncommitted comment
                      git-appraise-comment-commit-buffer? nil)
                (with-current-buffer buf
                  (git-appraise-comment-handler-commit
                   comment git-appraise-comment-buffer-msg)))
              (git-appraise-test--drain-lazy buf)
              ;; no full backend refresh, no diff reload
              (should (= build-calls 0))
              (should (= full-refresh-calls 0))
              (should (= diff-reads 0))
              ;; exactly one canonical node: a file-level comment is a
              ;; conversation entry whose body carries the path, and
              ;; whose databaseId is the canonical thread hash
              (let* ((nodes (git-appraise-comment-inplace-test--flatten-comment-nodes))
                     (node (car nodes)))
                (should (= (length nodes) 1))
                (should (string-match-p "example.txt" (a-get node 'bodyHTML)))
                (should (string-match-p "file level note" (a-get node 'bodyHTML)))
                (should (a-get node 'databaseId)))
              ;; shown in Conversation; the hunks re-folded as usual
              (with-current-buffer buf
                (goto-char (point-min))
                (should (search-forward "file" nil t))
                (should (search-forward "level" nil t))
                (should (search-forward "note" nil t))
                (should
                 (--any? (and (eq (oref it type) 'hunk) (oref it hidden))
                         (let (all)
                           (magit-map-sections
                            (lambda (s) (push s all)) magit-root-section)
                           all)))))
          (fset 'git-appraise-backend--run orig-run)
          (fset 'git-appraise--build-buffer orig-build))))))

;;; req-2026-11-24-104: folded conversation stays folded until opened
;; covers: git-appraise-section-comment.el
;; covers-fn: git-appraise-backend-refresh-comments
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-104-folded-conversation ()
  "WHEN a file comment is sent while Conversation is folded THE SYSTEM SHALL conceal it until the section is opened, and a reload shall keep one copy with the same identity."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (git-appraise-section-full-refresh? nil)
           (hash-before nil))
      (with-current-buffer buf
        (let (header)
          (magit-map-sections
           (lambda (s)
             (when (eq (oref s type) 'git-appraise-comment-header-section)
               (setq header s)))
           magit-root-section)
          (should header)
          (magit-section-hide header))
        (let ((comment (git-appraise-local-comment-section
                        :state "LOCAL COMMENT"
                        :author "tester@example.com"
                        :path "example.txt"
                        :position 0
                        :line-type "FILE"
                        :send? t
                        :msg "folded conversation note")))
          (setq git-appraise-comment-uncommitted comment
                git-appraise-comment-commit-buffer? nil)
          (git-appraise-comment-handler-commit
           comment git-appraise-comment-buffer-msg))
        ;; concealed while folded ...
        (goto-char (point-min))
        (search-forward "folded conversation" nil t)
        (search-forward "note" nil t)
        (should (invisible-p (match-beginning 0)))
        ;; ... visible once opened ...
        (let (header)
          (magit-map-sections
           (lambda (s)
             (when (eq (oref s type) 'git-appraise-comment-header-section)
               (setq header s)))
           magit-root-section)
          (magit-section-show header)
          (dolist (child (oref header children))
            (magit-section-show child)))
        (goto-char (point-min))
        (search-forward "folded conversation" nil t)
        (search-forward "note" nil t)
        (should-not (invisible-p (match-beginning 0)))
        (setq hash-before
              (a-get (car (git-appraise-comment-inplace-test--flatten-comment-nodes))
                     'databaseId))
        ;; an explicit reload keeps exactly one copy, same identity
        (let ((git-appraise-history-enabled nil))
          (git-appraise-reload))
        (git-appraise-test--drain-lazy buf)
        (goto-char (point-min))
        (should (= (how-many "folded[[:space:]
]+conversation" (point-min) (point-max)) 1))
        (should (equal (a-get (car (git-appraise-comment-inplace-test--flatten-comment-nodes))
                              'databaseId)
                       hash-before))))))

;;; req-2026-11-24-108: conversation summary and navigation
;; covers: git-appraise-section-comment.el
;; covers-fn: git-appraise-section-insert-top-level-comments, git-appraise-comment-open-at-point
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-108-conversation-summary-and-open ()
  "WHEN a reviewer sees a folded conversation THE SYSTEM SHALL show its message count and latest date and open the message with its context."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "First file note" "-f" "example.txt" revision)
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Latest file note" "-f" "example.txt" revision)
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let* ((infos (git-appraise-db--pullreq-raw-infos))
           (nodes (a-get-in infos '(comments nodes))))
      (setf (alist-get 'createdAt (car nodes)) "2025-01-01T00:00:00Z"
            (alist-get 'createdAt (cadr nodes)) "2030-01-01T00:00:00Z")
      (git-appraise-db--pullreq-raw-infos-update infos)
      (git-appraise--build-buffer)
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name))))
    (let ((buf (get-buffer (git-appraise-pr-buffer-name))))
      (with-current-buffer buf
        (let (header)
          (magit-map-sections
           (lambda (section)
             (when (eq (oref section type) 'git-appraise-comment-header-section)
               (setq header section)))
           magit-root-section)
          (should header)
          (should (eq (lookup-key git-appraise-comment-header-section-map
                                  (kbd "RET"))
                      'git-appraise-comment-open-at-point))
          (goto-char (oref header start))
          (let* ((nodes (a-get-in (git-appraise-db--pullreq-raw-infos)
                                  '(comments nodes)))
                 (expected-time (git-appraise-comment-format-time
                                 (a-get (car (last nodes)) 'createdAt))))
            (should (= 2 (length nodes)))
            (should (equal
                     (format "Conversation (2) · last %s" expected-time)
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (magit-section-hide header)
          (git-appraise-comment-open-at-point)
          (should-not (invisible-p (point)))
          (should (search-forward "example.txt" nil t))
          (should (re-search-forward "file[[:space:]\n]+note" nil t))
          (let ((child (car (oref header children))))
            (should child)
            (goto-char (oref child start))
            ;; Groups are foldable; open them and reach a comment.
            (magit-section-show child)
            (let (comment-sec)
              (magit-map-sections
               (lambda (s)
                 (when (and (not comment-sec)
                            (cl-typep s 'git-appraise-comment-section))
                   (setq comment-sec s)))
               child)
              (when comment-sec
                (goto-char (oref comment-sec start))
                (should (eq (key-binding (kbd "RET"))
                            'git-appraise-comment-open-at-point))
                (call-interactively (key-binding (kbd "RET")))
                (should-not (invisible-p (point)))))))))))

;;; req-2026-11-24-105: concurrent comments and equal timestamps
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend-refresh-comments
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-105-concurrent-comments ()
  "WHEN another comment appears before the metadata read with equal timestamps THE SYSTEM SHALL show every canonical comment exactly once and keep unsent drafts."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
    ;; an unsent draft must survive the metadata refresh
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (git-appraise-section-full-refresh? nil)
           (draft (git-appraise-local-comment-section
                   :state "LOCAL COMMENT"
                   :author "tester@example.com"
                   :path "example.txt"
                   :side "RIGHT"
                   :line 2
                   :line-type "ADDED"
                   :send? nil
                   :msg "local draft")))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "^\\+changed" nil t)
        (setq git-appraise-comment-cursor-pos (line-beginning-position)
              git-appraise-comment-uncommitted draft
              git-appraise-comment-commit-buffer? nil)
        (git-appraise-comment-handler-commit draft git-appraise-comment-buffer-msg))
      ;; a file-level comment is sent ...
      (let ((comment (git-appraise-local-comment-section
                      :state "LOCAL COMMENT"
                      :author "tester@example.com"
                      :path "example.txt"
                      :position 0
                      :line-type "FILE"
                      :send? t
                      :msg "concurrent note")))
        (setq git-appraise-comment-uncommitted comment
              git-appraise-comment-commit-buffer? nil)
        ;; ... while another process' comment lands before the read:
        ;; simulate it on the CLI before the callback's metadata read
        (let ((orig-read (symbol-function 'git-appraise-backend--read-review)))
          (fset 'git-appraise-backend--read-review
                (lambda (directory rev)
                  (let* ((review (apply orig-read directory (list rev)))
                         ;; a competing thread with the same timestamp
                         ;; lands before the callback's metadata read
                         (threads (a-get review 'comments))
                         (extra `((hash . "feedfacefeedfacefeedfacefeedfacefeedface")
                                  (comment (timestamp . "1795515662")
                                           (author . "someone-else")
                                           (location ((commit . ,rev) (path . "example.txt")))
                                           (description . "race comment")))))
                    (a-assoc-in review '(comments)
                                (append threads (list extra))))))
          (unwind-protect
              (with-current-buffer buf
                (git-appraise-comment-handler-commit
                 comment git-appraise-comment-buffer-msg))
            (fset 'git-appraise-backend--read-review orig-read)))
      (git-appraise-test--drain-lazy buf)
      (with-current-buffer buf
        ;; both canonical comments, each exactly once
        (should (= (how-many "concurrent" (point-min) (point-max)) 1))
        (should (= (how-many "note" (point-min) (point-max)) 1))
        (should (= (how-many "race" (point-min) (point-max)) 1))
        ;; and the unsent draft is still there
        (should (= (how-many "local draft" (point-min) (point-max)) 1)))))))

;;; req-2026-11-24-106: send failure and recovery paths
;; covers: git-appraise-comment-draft.el git-appraise-backend.el
;; covers-fn: git-appraise-backend-refresh-comments, git-appraise-comment-handler-commit
;;; req-2026-11-24-106: send failure and recovery paths
;; covers: git-appraise-comment-draft.el git-appraise-backend.el
;; covers-fn: git-appraise-backend-refresh-comments, git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-106-send-failures ()
  "WHEN the CLI write fails, the metadata read fails after a successful send, or the head changed THE SYSTEM SHALL keep the composer, never resend, and use the recovery paths."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (git-appraise-section-full-refresh? nil)
           (build-calls 0)
           (messages nil)
           (file-comment (lambda (msg)
                           (git-appraise-local-comment-section
                            :state "LOCAL COMMENT"
                            :author "tester@example.com"
                            :path "example.txt"
                            :position 0
                            :line-type "FILE"
                            :send? t
                            :msg msg))))
      ;; CLI failure: the composer stays and nothing is stored
      (let ((orig-send (symbol-function 'git-appraise-backend--send-comment)))
        (fset 'git-appraise-backend--send-comment
              (lambda (&rest _) (error "cli down")))
        (unwind-protect
            (let ((comment (funcall file-comment "first attempt")))
              (setq git-appraise-comment-uncommitted comment
                    git-appraise-comment-commit-buffer? nil)
              (let ((err (should-error
                          (with-current-buffer buf
                            (git-appraise-comment-handler-commit
                             comment git-appraise-comment-buffer-msg)))))
                (should (string-match-p "cli down" (error-message-string err))))
              (should (eq git-appraise-comment-uncommitted comment))
              (should (null (git-appraise-comment-inplace-test--flatten-comment-nodes)))
              (should (-any? #'git-appraise-db--local-comment-entry-p
                             (git-appraise-db--pullreq-raw-comments))))
          (fset 'git-appraise-backend--send-comment orig-send)))
      ;; JSON-read failure after a successful send: no resend, no build
      (let ((orig-read (symbol-function 'git-appraise-backend--read-review))
            (orig-msg (symbol-function 'message))
            (comment (funcall file-comment "second attempt")))
        (fset 'git-appraise-backend--read-review
              (lambda (&rest _) (error "json broken")))
        (fset 'message
              (lambda (format-string &rest args)
                (push (apply #'format (or format-string "") args) messages)
                (apply orig-msg format-string args)))
        (unwind-protect
            (progn
              (setq git-appraise-comment-uncommitted comment
                    git-appraise-comment-commit-buffer? nil)
              (with-current-buffer buf
                (git-appraise-comment-handler-commit
                 comment git-appraise-comment-buffer-msg))
              (should (string-match-p
                       "local refresh failed"
                       (string-join (nreverse messages) "\n")))
              (should (= build-calls 0)))
          (fset 'git-appraise-backend--read-review orig-read)
          (fset 'message orig-msg))
        ;; the stored set DID update once the read works again
        (should (eq (git-appraise-backend-refresh-comments
                     (git-appraise-db-get-pullreq))
                    'ok))
        (should (= (length (git-appraise-comment-inplace-test--flatten-comment-nodes))
                   1)))
      ;; head change: the full refresh path is legitimate
      (let ((orig-head (symbol-function 'git-appraise-backend--head))
            (comment (funcall file-comment "third attempt")))
        (fset 'git-appraise-backend--head
              (lambda (&rest _) "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"))
        (unwind-protect
            (progn
              (setq git-appraise-comment-uncommitted comment
                    git-appraise-comment-commit-buffer? nil)
              (cl-letf (((symbol-function 'git-appraise--build-buffer)
                         (lambda (&rest _) (cl-incf build-calls))))
                (with-current-buffer buf
                  (git-appraise-comment-handler-commit
                   comment git-appraise-comment-buffer-msg)))
              (should (= build-calls 1)))
          (fset 'git-appraise-backend--head orig-head))))))

;;; req-2026-11-24-115: completed comments survive without a verdict
;; covers: git-appraise-comment.el git-appraise-comment-draft.el git-appraise-backend.el
;; covers-fn: git-appraise-comment-handler-add-or-edit, git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-115-immediate-comment ()
  "WHEN a reviewer finishes a line comment THE SYSTEM SHALL save it in git-appraise before any accept or reject decision."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let ((buf (get-buffer (git-appraise-pr-buffer-name)))
          comment)
      (with-current-buffer buf
        (goto-char (point-min))
        (search-forward "+changed")
        (cl-letf (((symbol-function 'git-appraise-comment-add)
                   (lambda (&rest _) (setq comment git-appraise-comment-uncommitted))))
          (git-appraise-comment-handler-add-or-edit
           (oref (magit-current-section) value)))
        (should comment)
        (should (oref comment send?))
        (oset comment msg "Saved before verdict")
        (setq git-appraise-comment-commit-buffer? nil)
        (git-appraise-comment-handler-commit
         comment git-appraise-comment-buffer-msg))
      (let ((review (git-appraise-backend--read-review directory revision)))
        (should (= 1 (length (a-get review 'comments))))
        (should-not (a-get review 'resolved)))
      (should-not (-any? #'git-appraise-db--local-comment-entry-p
                         (git-appraise-db--pullreq-raw-comments))))))

;;; req-2026-11-24-128: file draft is visible during lazy render
;; covers: git-appraise-comment-draft.el
;; covers-fn: git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-128-file-draft-visible-with-lazy ()
  "WHEN a reviewer saves a file draft while lazy rendering is active THE SYSTEM SHALL show the draft in the review buffer."
  (git-appraise-comment-inplace-test-with-repo
    (let ((git-appraise-history-enabled nil)
          (git-appraise-lazy-enabled t))
      (git-appraise-backend-open revision)
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise--build-buffer))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name))))
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
                        :msg "lazy file draft")))
          (setq git-appraise-comment-cursor-pos (point-min)
                git-appraise-comment-uncommitted comment
                git-appraise-comment-commit-buffer? nil)
          (git-appraise-comment-handler-commit comment git-appraise-comment-buffer-msg)
          (goto-char (point-min))
          (should (search-forward "lazy file draft" nil t)))))))

;;; req-2026-11-24-139: conversation timeline groups comments by review
;; covers: git-appraise-section-comment.el
;; covers-fn: git-appraise-section-insert-top-level-comments, git-appraise-comment--collect-conversation-groups
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-139-conversation-timeline-groups ()
  "WHEN a review has top-level and review comments THE SYSTEM SHALL show them chronologically in foldable review groups with inline diff links."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let* ((infos (git-appraise-db--pullreq-raw-infos))
           (raw-comments
            (list
             `((author (login . "alice"))
               (state . "COMMENTED")
               (submittedAt . "2026-01-02T10:00:00Z")
               (bodyHTML . "<p>Review body</p>")
               (comments (nodes ((author (login . "alice"))
                                 (bodyHTML . "<p>Inline note</p>")
                                 (path . "example.txt")
                                 (line . 2)
                                 (side . "RIGHT")
                                 (databaseId . 101)
                                 (createdAt . "2026-01-02T11:00:00Z")))))
             `((author (login . "bob"))
               (state . "APPROVED")
               (submittedAt . "2026-01-03T09:00:00Z")
               (comments (nodes ((author (login . "bob"))
                                 (bodyHTML . "<p>LGTM inline</p>")
                                 (path . "example.txt")
                                 (line . 3)
                                 (side . "RIGHT")
                                 (databaseId . 102)
                                 (createdAt . "2026-01-03T09:05:00Z"))))))))
      (setf (alist-get 'comments infos)
            `((nodes . (((author (login . "carol"))
                         (bodyHTML . "<p>Stray top-level</p>")
                         (databaseId . 201)
                         (createdAt . "2026-01-01T08:00:00Z"))))))
      (setf (alist-get 'historicalComments infos)
            `((nodes . (((author (login . "dave"))
                         (bodyText . "Earlier note")
                         (path . "example.txt")
                         (line . 1)
                         (commit . "abc1234")
                         (databaseId . 301)
                         (state . "CHANGES_REQUESTED")
                         (createdAt . "2025-12-20T08:00:00Z"))))))
      (git-appraise-db--pullreq-raw-infos-update infos)
      (let ((pr (git-appraise-db-get-pullreq)))
        (oset pr raw-comments raw-comments)
        (git-appraise-db-update pr))
      (let ((git-appraise-section-full-refresh? nil))
        (git-appraise--build-buffer))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (let* ((fixture (expand-file-name
                         "fixtures/conversation-timeline.txt"
                         (file-name-directory
                          (or (locate-library "git-appraise-comment-inplace-test")
                              load-file-name buffer-file-name default-directory))))
               (expected (with-temp-buffer
                           (insert-file-contents fixture)
                           (buffer-string)))
               (start (progn (goto-char (point-min))
                             (search-forward "Conversation")
                             (line-beginning-position)))
               (end (progn (goto-char start)
                           (if (search-forward "Files changed" nil t)
                               (line-beginning-position)
                             (point-max))))
               (actual (buffer-substring-no-properties start end)))
          (should (equal actual expected))
          ;; Groups are foldable sections.
          (let (groups)
            (magit-map-sections
             (lambda (s)
               (when (eq (oref s type) 'git-appraise-conversation-group)
                 (push s groups)))
             magit-root-section)
            (should (= (length groups) 3))
            (dolist (g groups)
              (should (oref g hidden)))))))))

;;; req-2026-11-24-140: conversation diff link jumps to diff comment
;; covers: git-appraise-section-comment.el
;; covers-fn: git-appraise-comment--goto-diff-comment, git-appraise-comment--insert-link
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-140-conversation-link-jumps ()
  "WHEN a reviewer activates a conversation diff link THE SYSTEM SHALL jump to the diff comment."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let* ((infos (git-appraise-db--pullreq-raw-infos))
           (raw-comments (list
                          `((author (login . "alice"))
                            (state . "COMMENTED")
                            (submittedAt . "2026-01-02T10:00:00Z")
                            (comments (nodes ((author (login . "alice"))
                                              (bodyHTML . "<p>Inline note</p>")
                                              (path . "example.txt")
                                              (line . 2)
                                              (side . "RIGHT")
                                              (databaseId . 101)
                                              (createdAt . "2026-01-02T11:00:00Z"))))))))
      (git-appraise-db--pullreq-raw-infos-update infos)
      (let ((pr (git-appraise-db-get-pullreq)))
        (oset pr raw-comments raw-comments)
        (git-appraise-db-update pr))
      (let ((git-appraise-section-full-refresh? nil))
        (git-appraise--build-buffer))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (search-forward "↳ diff")
        (push-button)
        (let ((sec (magit-current-section)))
          (should sec)
          (should (slot-boundp sec 'value))
          (should (eieio-object-p (oref sec value)))
          (should (equal (oref (oref sec value) id) 101)))))))

;;; req-2026-11-24-141: conversation diff link tolerates rename-shaped sections
;; covers: git-appraise-section-comment.el
;; covers-fn: git-appraise-comment--goto-diff-comment
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-141-conversation-link-handles-rename-sections ()
  "WHEN a conversation diff link is activated in a buffer containing rename-shaped section values THE SYSTEM SHALL jump to the comment."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let* ((infos (git-appraise-db--pullreq-raw-infos))
           (raw-comments (list
                          `((author (login . "alice"))
                            (state . "COMMENTED")
                            (submittedAt . "2026-01-02T10:00:00Z")
                            (comments (nodes ((author (login . "alice"))
                                              (bodyHTML . "<p>Inline note</p>")
                                              (path . "example.txt")
                                              (line . 2)
                                              (side . "RIGHT")
                                              (databaseId . 101)
                                              (createdAt . "2026-01-02T11:00:00Z"))))))))
      (git-appraise-db--pullreq-raw-infos-update infos)
      (let ((pr (git-appraise-db-get-pullreq)))
        (oset pr raw-comments raw-comments)
        (git-appraise-db-update pr))
      (let ((git-appraise-section-full-refresh? nil))
        (git-appraise--build-buffer))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        ;; Inject a rename-shaped section value that previously broke scanning.
        (let ((inhibit-read-only t)
              (magit-insert-section--parent magit-root-section))
          (goto-char (point-min))
          (magit-insert-section (dummy '(rename "old.txt" "new.txt"))
            (insert " ")))
        (goto-char (point-min))
        (search-forward "↳ diff")
        (push-button)
        (let ((sec (magit-current-section)))
          (should sec)
          (should (eieio-object-p (oref sec value)))
          (should (equal (oref (oref sec value) id) 101)))))))

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
(ert-deftest git-appraise-comment-inplace/req-2026-11-25-040-dynamic-add-no-reload-many-positions ()
  "WHEN a reviewer adds draft comments dynamically at various hunk positions
THE SYSTEM SHALL insert each comment in place without rebuilding or reloading the review."
  (git-appraise-comment-inplace-test-with-repo
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
    (git-appraise-comment-inplace-test--open-and-render revision)
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
(ert-deftest git-appraise-comment-inplace/req-2026-11-25-041-send-flow-no-reload-multi-hunk ()
  "WHEN a reviewer adds an immediately-sent comment through the real UI flow in a multi-hunk file
THE SYSTEM SHALL render the comment in place without rebuilding the review buffer."
  (git-appraise-comment-inplace-test-with-repo
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
    (git-appraise-comment-inplace-test--open-and-render revision)
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

;;; req-2026-11-25-042:
;; covers: git-appraise-comment-draft.el git-appraise-section-comment.el
;; covers-fn: git-appraise-comment--update-remote-comment-in-place
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-25-042-edit-remote-code-comment-in-place ()
  "WHEN a remote code comment is edited
THE SYSTEM SHALL update the rendered comment body in place without rebuilding the review."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Original body"
     "-f" "example.txt" "-l" "2" revision)
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (build-calls 0)
           (orig-build (symbol-function 'git-appraise--build-buffer))
           comment-id)
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "Original" nil t))
        (magit-map-sections
         (lambda (s)
           (when (and (not comment-id)
                      (memq (oref s type)
                            '(git-appraise-code-comment-section
                              git-appraise-outdated-comment-section)))
             (let ((v (oref s value)))
               (when (and (slot-boundp v 'id) (oref v id))
                 (setq comment-id (oref v id)))))))
        (should comment-id))
      (cl-letf (((symbol-function 'git-appraise--build-buffer)
                 (lambda (&rest args)
                   (cl-incf build-calls)
                   (apply orig-build args))))
        (with-current-buffer buf
          (should (git-appraise-comment--update-remote-comment-in-place
                   (buffer-name buf) comment-id "Edited body")))
        (should (= build-calls 0)))
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "Edited" nil t))
        (goto-char (point-min))
        (should-not (search-forward "Original body" nil t))
        (goto-char (point-min))
        (should-not (search-forward "Original\nbody" nil t))))))

;;; req-2026-11-25-043:
;; covers: git-appraise-comment.el git-appraise-comment-draft.el
;; covers-fn: git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-25-043-edit-remote-comment-no-manual-refresh ()
  "WHEN a remote comment edit succeeds
THE SYSTEM SHALL update the review without asking the user to refresh manually."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Original body"
     "-f" "example.txt" "-l" "2" revision)
    (git-appraise-comment-inplace-test--open-and-render revision)
    (let* ((buf (get-buffer (git-appraise-pr-buffer-name)))
           (pr (git-appraise-db-get-pullreq))
           (full-refreshes 0)
           (orig-build (symbol-function 'git-appraise--build-buffer))
           comment-id)
      (with-current-buffer buf
        (magit-map-sections
         (lambda (s)
           (when (and (not comment-id)
                      (eq (oref s type) 'git-appraise-code-comment-section))
             (let ((v (oref s value)))
               (when (and (slot-boundp v 'id) (oref v id))
                 (setq comment-id (oref v id))))))))
      (should comment-id)
      (cl-letf (((symbol-function 'git-appraise--build-buffer)
                 (lambda (&rest args)
                   (when git-appraise-section-full-refresh?
                     (cl-incf full-refreshes))
                   (apply orig-build args)))
                ((symbol-function 'git-appraise-update-comment)
                 (lambda (_pr _kind _id _body callback)
                   (funcall callback)))
                ((symbol-function 'git-appraise-backend-refresh-comments)
                 (lambda (_pr) 'ok)))
        (let ((obj (git-appraise-editable-remote-comment
                    :kind "review-comment"
                    :id comment-id
                    :body "Original body")))
          (oset obj msg "Edited body")
          (git-appraise-comment-handler-commit obj git-appraise-comment-edit-remote-msg)))
      (should (= full-refreshes 0))
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "Edited" nil t))
        (goto-char (point-min))
        (should-not (search-forward "refresh display" nil t))))))

;;; req-2026-11-25-044:
;; covers: git-appraise-comment.el
;; covers-fn: git-appraise-comment--commit-text
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-25-044-single-comment-no-full-refresh ()
  "WHEN a single conversation comment is sent
THE SYSTEM SHALL show it without a full diff reload."
  (git-appraise-comment-inplace-test-with-repo
    (git-appraise-comment-inplace-test--open-and-render revision)
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

(provide 'git-appraise-comment-inplace-test)
;;; git-appraise-comment-inplace-test.el ends here
