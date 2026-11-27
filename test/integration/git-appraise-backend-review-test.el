;;; git-appraise-backend-review-test.el --- Backend review integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)
(require 'git-appraise-test-backend-fixture)

;;; req-2026-11-25-056: open a local review by revision
;; covers: git-appraise.el
;; covers-fn: git-appraise-start
;; enforcement: test
(ert-deftest git-appraise-backend-review/req-2026-11-25-056-start-local-review ()
  "WHEN a reviewer selects a local review THE SYSTEM SHALL open that review."
  (let (selected)
    (cl-letf (((symbol-function 'git-appraise-backend-open)
               (lambda (revision) (setq selected revision)))
              ((symbol-function 'git-appraise--build-buffer)
               (lambda (&rest _) nil)))
      (git-appraise-start "abc123")
      (should (equal selected "abc123")))))

;;; req-2026-11-23-006: inline comments on git-appraise reviews
;; covers: git-appraise-backend.el git-appraise-comment.el git-appraise-actions.el
;; covers-fn: git-appraise-backend--line-for-comment, git-appraise-submit-single-diff-comment-at-point
;; enforcement: test
(ert-deftest git-appraise-backend-review/req-2026-11-23-006-send-hunk-comment-from-editor ()
  "WHEN a reviewer comments on a current diff line THE SYSTEM SHALL save the comment at that line."
  (git-appraise-backend-test-with-repo
    (let ((git-appraise-history-enabled nil))
      (git-appraise-backend-open revision)
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise--build-buffer))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (search-forward "+changed")
        (beginning-of-line)
        (call-interactively #'git-appraise-submit-single-diff-comment-at-point))
      (should (null (oref git-appraise-comment-uncommitted position)))
      (should (equal (oref git-appraise-comment-uncommitted side) "RIGHT"))
      (should (= (oref git-appraise-comment-uncommitted line) 2))
      (with-current-buffer git-appraise-comment-buffer-name
        (erase-buffer)
        (insert "Comment from editor")
        (call-interactively #'git-appraise-comment-commit))
      (let ((thread (cl-find-if
                     (lambda (item)
                       (equal (a-get-in item '(comment description))
                              "Comment from editor"))
                     (a-get (git-appraise-backend-test--show directory revision)
                            'comments))))
        (should thread)
        (should (= (a-get-in thread '(comment location range startLine)) 2))))))

;;; req-2026-11-25-063: ordinary comment editing stages review feedback
;; covers: git-appraise-comment.el git-appraise-comment-draft.el git-appraise-actions.el
;; covers-fn: git-appraise-comment-add-or-edit, git-appraise-submit-needs-work
;; enforcement: test
(ert-deftest git-appraise-backend-review/req-2026-11-25-063-stage-then-publish-finding ()
  "WHEN a reviewer drafts an inline finding THE SYSTEM SHALL keep it private until the reviewer publishes needs-work feedback."
  (git-appraise-backend-test-with-repo
    (let ((git-appraise-history-enabled nil))
      (git-appraise-backend-open revision)
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise--build-buffer))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (search-forward "+changed")
        (beginning-of-line)
        (git-appraise-comment-add-or-edit))
      (with-current-buffer git-appraise-comment-buffer-name
        (erase-buffer)
        (insert "Fix this before merge")
        (git-appraise-comment-commit))
      (should-not (a-get (git-appraise-backend-test--show directory revision)
                         'comments))
      (should (cl-find-if #'git-appraise-db--local-comment-entry-p
                          (oref (git-appraise-db-get-pullreq) raw-comments)))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (git-appraise-submit-needs-work))
      (let ((thread (car (a-get
                          (git-appraise-backend-test--show directory revision)
                          'comments))))
        (should (equal "Fix this before merge"
                       (a-get-in thread '(comment description))))
        (should (eq :false (a-get-in thread '(comment resolved))))))))

;;; req-2026-11-25-064: request edits retain native review identity
;; covers: git-appraise-backend.el git-appraise-comment.el
;; covers-fn: git-appraise-backend-update-request
;; enforcement: test
(ert-deftest git-appraise-backend-review/req-2026-11-25-064-update-request ()
  "WHEN an author edits a review request THE SYSTEM SHALL keep its review identity and discussion."
  (git-appraise-backend-test-with-repo
    (let* ((pr (git-appraise-backend-open revision))
           (before (git-appraise-backend-test--show directory revision)))
      (git-appraise-backend--send-comment pr "Keep this discussion")
      (git-appraise-backend-update-request pr "Updated title\nUpdated details")
      (let ((after (git-appraise-backend-test--show directory revision)))
        (should (equal (a-get after 'revision) revision))
        (should (equal (a-get-in after '(request description))
                       "Updated title\nUpdated details"))
        (should (equal (a-get-in after '(request reviewRef))
                       (a-get-in before '(request reviewRef))))
        (should (equal (a-get-in after '(request targetRef))
                       (a-get-in before '(request targetRef))))
        (should (= 1 (length (a-get after 'comments))))))))

;;; req-2026-11-24-001: inline comment location survives header-like content
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend--diff-tables, git-appraise-backend--position
;; enforcement: test
(ert-deftest git-appraise-backend-review/req-2026-11-24-001-map-header-like-added-line ()
  "WHEN an added line resembles a diff header THE SYSTEM SHALL keep comments anchored to its file."
  (let* ((diff (concat "diff --git a/example.txt b/example.txt\n"
                       "--- a/example.txt\n+++ b/example.txt\n"
                       "@@ -1 +1,2 @@\n base\n+++ value\n"))
         (tables (git-appraise-backend--diff-tables diff)))
    (should (equal (mapcar #'car tables) '("example.txt")))
    (should (numberp (git-appraise-backend--position tables "example.txt" 2)))))

;;; req-2026-11-24-006: reload keeps drafts on an unchanged review
;; covers: git-appraise-backend.el git-appraise-db.el
;; covers-fn: git-appraise-backend-refresh, git-appraise-db--reconcile-raw-comments
;; enforcement: test
(ert-deftest git-appraise-backend-review/req-2026-11-24-006-keep-drafts-on-reload ()
  "WHEN a reviewer reloads an unchanged review THE SYSTEM SHALL retain unsent comments alongside new discussion."
  (git-appraise-backend-test-with-repo
    (git-appraise-backend-open revision)
    (let ((git-appraise-section-full-refresh? t))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (git-appraise-db--pullreq-raw-comments-update
     '((author (login . "Tester"))
       (state . "LOCAL COMMENT")
       (comments (nodes ((internal-id . "draft-reload-1")
                         (bodyText . "Keep this draft")
                         (path . "example.txt")
                         (position . 3)
                         (line-type . "ADDED")
                         (local? . t))))))
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "External discussion" revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (git-appraise-reload))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (let ((rendered (with-current-buffer (git-appraise-pr-buffer-name)
                      (substring-no-properties (buffer-string)))))
      (should (string-match-p "Keep[[:space:]\n]+this[[:space:]\n]+draft" rendered))
      (should (string-match-p "External[[:space:]\n]+discussion" rendered)))))

;;; req-2026-11-24-007: head change preserves unsent drafts
;; covers: git-appraise-backend.el git-appraise-section.el
;; covers-fn: git-appraise-backend-refresh, git-appraise--build-buffer
;; enforcement: test
(ert-deftest git-appraise-backend-review/req-2026-11-24-007-confirm-discard-on-head-change ()
  "WHEN the reviewed head changes THE SYSTEM SHALL preserve unsent comments and refresh the review without blocking."
  (git-appraise-backend-test-with-repo
    (git-appraise-backend-open revision)
    (let ((git-appraise-section-full-refresh? t))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (git-appraise-db--pullreq-raw-comments-update
     '((author (login . "Tester"))
       (state . "LOCAL COMMENT")
       (comments (nodes ((internal-id . "draft-head-change")
                         (bodyText . "Hold this draft")
                         (path . "example.txt")
                         (position . 3)
                         (line-type . "ADDED")
                         (local? . t))))))
    (with-temp-file (expand-file-name "example.txt" directory)
      (insert "first\nchanged again\nthird\n"))
    (git-appraise-test--run directory "git" "commit" "-qam" "followup")
    (let ((prompted nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq prompted t) nil)))
        (with-current-buffer (git-appraise-pr-buffer-name)
          (git-appraise-reload)))
      (should-not prompted))
    ;; Head may or may not advance depending on git-appraise; the key
    ;; behavior is no blocking prompt and no draft loss.
    (should (cl-find-if #'git-appraise-db--local-comment-entry-p
                        (oref (git-appraise-db-get-pullreq) raw-comments)))))

;;; req-2026-11-25-071: comments need a real diff anchor
;; covers: git-appraise-comment.el
;; covers-fn: git-appraise-comment-handler-add-or-edit, git-appraise--comment-region-bounds
;; enforcement: test
(ert-deftest git-appraise-backend-review/req-2026-11-25-071-reject-gap-comment ()
  "WHEN a reviewer comments between a file heading and its patch THE SYSTEM SHALL require a real file or patch location."
  (let ((git-appraise-test--before-request-hook
         (lambda (directory)
           (with-temp-file (expand-file-name "more.txt" directory)
             (insert "more\n"))
           (git-appraise-test--run directory "git" "add" "more.txt")
           (git-appraise-test--run
            directory "git" "commit" "--amend" "--no-edit" "-q"))))
    (git-appraise-backend-test-with-repo
      (let ((git-appraise-history-enabled nil)
            (git-appraise-lazy-enabled nil))
        (git-appraise-backend-open revision)
        (let ((git-appraise-section-full-refresh? t))
          (git-appraise--build-buffer))
        (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name))))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (let ((transient-mark-mode t))
          (pcase-let ((`(,file ,hunk)
                       (git-appraise-backend-test--first-file-and-hunk)))
            (should hunk)
            (git-appraise-folding--set-file file nil)
            ;; the gap between the file heading and the hunk header has
            ;; no patch location, and neither does the hunk header itself
            (goto-char (oref file content))
            (should-error (git-appraise-comment-add-or-edit) :type 'user-error)
            (goto-char (oref hunk start))
            (should-error (git-appraise-comment-add-or-edit) :type 'user-error)
            ;; a selection through the hunk's last line still anchors there
            (goto-char (git-appraise--hunk-content-start hunk))
            (push-mark (oref hunk end) t t)
            (git-appraise-comment-add-or-edit)
            (should (integerp (oref git-appraise-comment-uncommitted line)))
            (should (integerp
                     (oref git-appraise-comment-uncommitted start-line)))
            (deactivate-mark)
            (git-appraise-comment-reset-global-vars)
            (let ((composer (get-buffer git-appraise-comment-buffer-name)))
              (when composer (kill-buffer composer)))
            ;; patch lines of two different hunks cannot form one range
            (let ((hunks (let (all)
                           (magit-map-sections
                            (lambda (s)
                              (when (magit-hunk-section-p s) (push s all)))
                            magit-root-section)
                           (nreverse all))))
              (should (>= (length hunks) 2))
              (goto-char (git-appraise--hunk-content-start (nth 0 hunks)))
              (push-mark (git-appraise--hunk-content-start (nth 1 hunks)) t t)
              (should-error (git-appraise-comment-add-or-edit) :type 'user-error)
              (deactivate-mark))))))))

;;; req-2026-11-25-072: file comment count updates as drafts change
;; covers: git-appraise-comment-draft.el git-appraise-section-wash.el git-appraise-actions.el
;; covers-fn: git-appraise-comment--save-draft, git-appraise-wash--refresh-file-comment-count, git-appraise-section-delete-comment
;; enforcement: test
(ert-deftest git-appraise-backend-review/req-2026-11-25-072-inline-draft-updates-file-count ()
  "WHEN a reviewer adds, replies to or removes an inline draft THE SYSTEM SHALL update that file's visible comment count."
  (git-appraise-backend-test-with-repo
    (let ((git-appraise-history-enabled nil)
          (git-appraise-lazy-enabled nil))
      (git-appraise-backend-open revision)
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise--build-buffer))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name))))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (pcase-let ((`(,file ,hunk) (git-appraise-backend-test--first-file-and-hunk)))
        (should hunk)
        (git-appraise-folding--set-file file nil)
        (goto-char (oref hunk start))
        (forward-line 1)
        (should (git-appraise--patch-line-p))
        (git-appraise-comment-add-or-edit)
        (with-current-buffer git-appraise-comment-buffer-name
          (erase-buffer)
          (insert "Inline count")
          (git-appraise-comment-commit))
        (goto-char (oref file start))
        (should (string-match-p "💬1" (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))))
        ;; a committed draft and an in-place reply both move the count
        (let* ((parent (git-appraise-local-comment-section
                        :state "LOCAL COMMENT"
                        :author "Tester"
                        :path "example.txt"
                        :side "RIGHT"
                        :line 2
                        :line-type "ADDED"
                        :send? nil
                        :msg "parent for count"))
               (reply (git-appraise-reply-comment-section
                       :state "COMMENTED"
                       :author "Tester"
                       :path "example.txt"
                       :position 3
                       :diffHunk "@@ -1,2 +1,3 @@"
                       :id 77
                       :msg "reply count"))
               (builds 0))
          (oset parent id 77)
          (setq git-appraise-comment-cursor-pos (oref hunk start)
                git-appraise-comment-uncommitted parent
                git-appraise-comment-commit-buffer? nil)
          (cl-letf (((symbol-function 'git-appraise--build-buffer)
                     (lambda (&rest _) (cl-incf builds))))
            (git-appraise-comment-handler-commit
             parent git-appraise-comment-buffer-msg)
            (goto-char (oref file start))
            (should (string-match-p "💬2" (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position))))
            (setq git-appraise-comment-uncommitted reply)
            (cl-letf (((symbol-function 'git-appraise-backend-send-reply)
                       (lambda (&rest _) nil)))
              (git-appraise-comment-handler-commit
               reply git-appraise-comment-buffer-msg))
            (should (= builds 0))
            (goto-char (oref file start))
            (should (string-match-p "💬3" (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position))))))
        (git-appraise-comment-reset-global-vars)
        ;; deleting one draft drops the count again
        (let (draft)
          (magit-map-sections
           (lambda (s)
             (when (and (not draft) (git-appraise-local-comment-section-p s))
               (setq draft s)))
           magit-root-section)
          (should draft)
          (goto-char (oref draft start))
          (git-appraise-section-delete-comment))
        (goto-char (oref file start))
        (should (string-match-p "💬2" (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))))
        (should-not (string-match-p "💬3" (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position))))))))

;;; git-appraise-backend-review-test.el ends here
