;;; git-appraise-comment-inplace-test.el --- ERT tests for in-place comment updates -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify local comment mutations update the rendered review in place.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

;;; req-2026-11-24-116: historical comments retain original context
;; covers: git-appraise-backend.el git-appraise-section-history.el git-appraise-section-wash.el
;; covers-fn: git-appraise-backend--normalize, git-appraise-history-insert-for-file
;; enforcement: test
(ert-deftest git-appraise-comment-inplace/req-2026-11-24-116-historical-comment-context ()
  "WHEN an earlier line comment remains after a review changes
THE SYSTEM SHALL show its original location and content beside the affected file."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Earlier line"
     "-f" "example.txt" "-l" "2" revision)
    (with-temp-file (expand-file-name "example.txt" directory)
      (insert "first\nchanged again\nthird\n"))
    (git-appraise-test--run directory "git" "commit" "-qam" "followup")
    (git-appraise-test--open-and-render revision)
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
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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
            (while (search-forward "[DRAFT] YOU" nil t)
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
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Sent inline"
     "-f" "example.txt" "-l" "2" revision)
    (let ((git-appraise-lazy-enabled nil)
          (git-appraise-inline-comment-border-color "#ff0000"))
      (git-appraise-test--open-and-render revision)
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
              (should (string-search
                       "[COMMENT]"
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
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Sent inline"
     "-f" "example.txt" "-l" "2" revision)
    (let ((git-appraise-lazy-enabled nil))
      (git-appraise-test--open-and-render revision)
      (with-current-buffer (get-buffer (git-appraise-pr-buffer-name))
        (let (hunk)
          (magit-map-sections
           (lambda (s)
             (when (eq (oref s type) 'hunk)
               (setq hunk s))))
          (should hunk)
          ;; unsaved hunk state follows the file: fold it explicitly
          (git-appraise-folding--set-file (oref hunk parent) nil)
          (git-appraise-folding--set-hunk hunk t)
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
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (let ((git-appraise-lazy-enabled nil))
      (git-appraise-test--open-and-render revision)
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
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (let ((git-appraise-lazy-enabled nil))
      (git-appraise-test--open-and-render revision)
      (let ((buf (get-buffer (git-appraise-pr-buffer-name)))
            pos
            comment)
        (with-current-buffer buf
          ;; unsaved hunk state follows the file: fold the hunk explicitly
          (let (hunk)
            (magit-map-sections
             (lambda (s) (when (eq (oref s type) 'hunk) (setq hunk s)))
             magit-root-section)
            (should hunk)
            (git-appraise-folding--set-file (oref hunk parent) nil)
            (git-appraise-folding--set-hunk hunk t))
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

;;; git-appraise-comment-inplace-test.el ends here
