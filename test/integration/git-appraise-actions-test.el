;;; git-appraise-actions-test.el --- ERT tests for main review actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Action-level coverage for review submission, navigation, and buffers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)
(require 'git-appraise-test-actions-fixture)

;;; req-2026-11-23-007: approve action submits review
;; covers: git-appraise-actions.el
;; covers-fn: git-appraise-submit-approve, git-appraise--submit
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-007-approve-action ()
  "WHEN a reviewer approves the review THE SYSTEM SHALL submit an approval decision."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (git-appraise-submit-approve))
    (should (eq t (a-get (git-appraise-actions-test--show directory revision) 'resolved)))))

;;; req-2026-11-23-008: submit-comments action sends staged comments
;; covers: git-appraise-actions.el git-appraise-backend.el
;; covers-fn: git-appraise-submit-comments, git-appraise--submit
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-008-submit-comments-action ()
  "WHEN a reviewer submits review comments THE SYSTEM SHALL publish staged local comments."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (git-appraise-db--pullreq-raw-comments-update
     '((author (login . "Tester"))
       (state . "LOCAL COMMENT")
       (comments (nodes ((internal-id . "draft-action-1")
                         (bodyText . "Drafted from action")
                         (path . "example.txt")
                         (position . 3)
                         (line-type . "ADDED")
                         (local? . t))))))
    (let ((git-appraise-section-full-refresh? nil))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (git-appraise-submit-comments))
    (should (cl-find-if
             (lambda (thread)
               (equal (a-get-in thread '(comment description))
                      "Drafted from action"))
             (a-get (git-appraise-actions-test--show directory revision)
                    'comments)))))

;;; req-2026-11-25-060: mixed findings can be marked FYI
;; covers: git-appraise-actions.el git-appraise-section-comment.el
;; covers-fn: git-appraise-toggle-draft-fyi
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-25-060-toggle-draft-fyi ()
  "WHEN a reviewer marks a draft FYI THE SYSTEM SHALL preserve that choice for a needs-work submission."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (git-appraise-db--pullreq-raw-comments-update
     '((author (login . "Tester"))
       (state . "LOCAL COMMENT")
       (comments (nodes ((internal-id . "fyi-draft")
                         (bodyText . "Context only")
                         (path . "example.txt")
                         (position . 3)
                         (line-type . "ADDED")
                         (local? . t))))))
    (let ((git-appraise-section-full-refresh? nil))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (search-forward "Context only")
      (execute-kbd-macro (kbd "C-c C-f")))
    (should (a-get (car (a-get-in
                         (car (git-appraise-db--pullreq-raw-comments))
                         '(comments nodes))) 'fyi))))

;;; req-2026-11-25-065: reviewer resolves a specific finding
;; covers: git-appraise-actions.el git-appraise-backend.el
;; covers-fn: git-appraise-resolve-comment-at-point
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-25-065-resolve-comment-at-point ()
  "WHEN a reviewer resolves a published finding THE SYSTEM SHALL reply to that finding and mark it addressed."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--run
     directory "git-appraise" "comment" "-nmw" "-m" "Please fix" revision)
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (search-forward "Please")
      (git-appraise-resolve-comment-at-point "Fixed in followup"))
    (let* ((root (car (a-get (git-appraise-actions-test--show
                               directory revision) 'comments)))
           (child (car (a-get root 'children))))
      (should (equal "Fixed in followup"
                     (a-get-in child '(comment description))))
      (should (eq t (a-get-in child '(comment resolved)))))))

;;; req-2026-11-25-066: published comments remain append-only
;; covers: git-appraise-comment.el
;; covers-fn: git-appraise-edit-remote-comment-at-point
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-25-066-published-comment-is-immutable ()
  "WHEN a reviewer tries to edit a published comment THE SYSTEM SHALL direct them to add a reply instead."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Published note" revision)
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (search-forward "Published")
      (should-error (git-appraise-edit-remote-comment-at-point)
                    :type 'user-error))
    (should (= 1 (length (a-get (git-appraise-actions-test--show
                                 directory revision) 'comments))))))

;;; req-2026-11-24-002: remove an unsent review comment
;; covers: git-appraise-actions.el git-appraise-section-comment.el
;; covers-fn: git-appraise-section-delete-comment
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-24-002-delete-draft-comment ()
  "WHEN a reviewer removes an unsent comment THE SYSTEM SHALL discard that draft without publishing it."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (git-appraise-db--pullreq-raw-comments-update
     '((author (login . "Tester"))
       (state . "LOCAL COMMENT")
       (comments (nodes ((internal-id . "draft-delete-1")
                         (bodyText . "Remove this draft")
                         (path . "example.txt")
                         (position . 3)
                         (line-type . "ADDED")
                         (local? . t))))))
    (let ((git-appraise-section-full-refresh? nil))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (search-forward "Remove this draft")
      (execute-kbd-macro (kbd "C-c C-k")))
    (should-not (cl-find-if
                 (lambda (entry)
                   (equal (a-get (car (a-get-in entry '(comments nodes)))
                                 'internal-id)
                          "draft-delete-1"))
                 (oref (git-appraise-db-get-pullreq) raw-comments)))
    (should-not (a-get (git-appraise-actions-test--show directory revision)
                       'comments))))

;;; req-2026-11-24-003: remove unsent review feedback
;; covers: git-appraise-actions.el git-appraise-section-header.el
;; covers-fn: git-appraise-delete-feedback
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-24-003-clear-feedback ()
  "WHEN a reviewer clears unsent feedback THE SYSTEM SHALL leave the review without that message."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (git-appraise-db--pullreq-feedback-update "Needs work")
    (let ((git-appraise-section-full-refresh? nil))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (search-forward "Your Review Feedback")
      (execute-kbd-macro (kbd "C-c C-k")))
    (should-not (oref (git-appraise-db-get-pullreq) feedback))))

;;; req-2026-11-23-009: needs-work action publishes actionable feedback
;; covers: git-appraise-actions.el git-appraise-backend.el
;; covers-fn: git-appraise-submit-request-changes, git-appraise--submit
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-009-request-changes-action ()
  "WHEN a reviewer sends needs-work feedback THE SYSTEM SHALL publish an actionable review comment."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (git-appraise-db--pullreq-feedback-update "Needs work")
    (let ((git-appraise-section-full-refresh? t))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (git-appraise-submit-request-changes))
    (let ((native (git-appraise-actions-test--show directory revision)))
      (should (eq :false (a-get native 'resolved)))
      (should (equal "Needs work"
                     (a-get-in (car (a-get native 'comments))
                               '(comment description)))))))

;;; req-2026-11-23-010: LGTM action approves with message
;; covers: git-appraise-actions.el
;; covers-fn: git-appraise-submit-lgtm
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-010-lgtm-action ()
  "WHEN a reviewer submits LGTM THE SYSTEM SHALL approve the review."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (git-appraise-submit-lgtm))
    (should (eq t (a-get (git-appraise-actions-test--show directory revision) 'resolved)))))

;;; req-2026-11-23-011: submit-only-replies sends no review decision
;; covers: git-appraise-actions.el
;; covers-fn: git-appraise-submit-only-replies, git-appraise--submit
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-011-submit-only-replies ()
  "WHEN a reviewer submits only replies with no staged replies THE SYSTEM SHALL leave the review decision unchanged."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (git-appraise-submit-only-replies))
    (let ((native (git-appraise-actions-test--show directory revision)))
      (should-not (eq t (a-get native 'resolved)))
      (should-not (equal "REJECTED" (git-appraise-backend--state native))))))

;;; req-2026-11-23-012: single top-level comment opens comment buffer
;; covers: git-appraise-actions.el
;; covers-fn: git-appraise-submit-single-top-level-comment
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-012-single-top-level-comment ()
  "WHEN a reviewer starts a single top-level comment THE SYSTEM SHALL open the comment composer."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (unwind-protect
        (with-current-buffer (git-appraise-pr-buffer-name)
          (git-appraise-submit-single-top-level-comment)
          (should (get-buffer git-appraise-comment-buffer-name))
          (with-current-buffer git-appraise-comment-buffer-name
            (should (string-match-p "comment to the PR"
                                    (substring-no-properties (buffer-string))))))
      (when (get-buffer git-appraise-comment-buffer-name)
        (kill-buffer git-appraise-comment-buffer-name))
      (git-appraise-comment-reset-global-vars))))

;;; req-2026-11-23-013: reload rebuilds review buffer
;; covers: git-appraise-actions.el
;; covers-fn: git-appraise-reload
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-013-reload-action ()
  "WHEN a reviewer reloads the review THE SYSTEM SHALL rebuild the review buffer."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "External reload comment" revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (git-appraise-reload))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (should (string-match-p
             "External[ \t\n]+reload[ \t\n]+comment"
             (with-current-buffer (git-appraise-pr-buffer-name)
               (substring-no-properties (buffer-string)))))))

;;; req-2026-11-27-009: Magit commands remain available beside review entry
;; covers: git-appraise.el
;; covers-fn: git-appraise-magit-dispatch
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-27-009-magit-commands ()
  "WHEN a reviewer uses Magit THE SYSTEM SHALL preserve Magit commands beside review actions."
  (require 'magit)
  (should (eq (lookup-key magit-status-mode-map (kbd "R"))
              'magit-file-rename))
  (should (eq (lookup-key magit-revision-mode-map (kbd "R"))
              'magit-file-rename))
  (should (transient-get-suffix 'magit-dispatch "R")))

;;; req-2026-11-27-010: standard refresh rebuilds the review
;; covers: git-appraise.el git-appraise-actions.el
;; covers-fn: git-appraise--revert-buffer, git-appraise-reload
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-27-010-standard-refresh ()
  "WHEN a reviewer refreshes the buffer THE SYSTEM SHALL rebuild the current review."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Standard refresh comment" revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (should (eq (key-binding (kbd "g")) 'revert-buffer))
      (should (eq (key-binding (kbd "G")) 'git-appraise-reload))
      (let ((git-appraise-db--pullreq-id nil))
        (revert-buffer nil t)))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (should (string-match-p "Standard[ \t\n]+refresh[ \t\n]+comment"
                              (substring-no-properties (buffer-string)))))))

;;; req-2026-11-27-011: unrelated Magit folds keep their own visibility
;; covers: git-appraise-folding.el
;; covers-fn: git-appraise-folding--fix-hidden-overlay
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-27-011-magit-fold-isolation ()
  "WHEN a Magit section folds THE SYSTEM SHALL leave its visibility behavior intact."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (root)
        (magit-insert-section (example)
          (magit-insert-heading "Example")
          (insert "Body\n"))))
    (let* ((section (car (oref magit-root-section children)))
           (_ (magit-section-hide section))
           (content (oref section content))
           (overlay (--first (overlay-get it 'invisible)
                             (overlays-at content))))
      (should overlay)
      (should-not (overlay-get overlay 'rear-advance)))))

;;; req-2026-11-23-014: PR description popup shows title and body
;; covers: git-appraise-actions.el
;; covers-fn: git-appraise-popup-pr-description, git-appraise-pr-description-insert
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-014-popup-pr-description ()
  "WHEN a reviewer opens the PR description THE SYSTEM SHALL show the review title and description."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buf &rest _)
                       (let ((b (if (bufferp buf) buf (get-buffer-create buf))))
                         (set-window-buffer (selected-window) b)
                         (selected-window)))))
            (with-current-buffer (git-appraise-pr-buffer-name)
              (git-appraise-popup-pr-description)))
          (should (get-buffer git-appraise-pr-description-buffer-name))
          (with-current-buffer git-appraise-pr-description-buffer-name
            (let ((text (substring-no-properties (buffer-string))))
              (should (string-match-p "Example review" text)))))
      (when (get-buffer git-appraise-pr-description-buffer-name)
        (kill-buffer git-appraise-pr-description-buffer-name)))))

;;; req-2026-11-23-015: hunk navigation moves between hunks
;; covers: git-appraise-actions.el
;; covers-fn: git-appraise-next-hunk, git-appraise-previous-hunk, git-appraise--hunk-sections
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-015-hunk-navigation ()
  "WHEN a reviewer navigates hunks THE SYSTEM SHALL move point to the next or previous hunk."
  (git-appraise-actions-test-with-repo
    (let ((git-appraise-fold-files-by-default nil))
      (git-appraise-test--open-and-render revision))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (magit-map-sections
       (lambda (s)
         (when (eq (oref s type) 'hunk)
           (oset s hidden nil))))
      (let ((hunks (git-appraise--hunk-sections)))
        (should hunks)
        (goto-char (point-min))
        (git-appraise-next-hunk)
        (should (= (point) (oref (car hunks) start)))
        (should-error (git-appraise-previous-hunk) :type 'user-error)
        (goto-char (point-max))
        (should-error (git-appraise-next-hunk) :type 'user-error)
        (when (cdr hunks)
          (goto-char (point-min))
          (git-appraise-next-hunk)
          (git-appraise-next-hunk)
          (should (= (point) (oref (cadr hunks) start)))
          (git-appraise-previous-hunk)
          (should (= (point) (oref (car hunks) start))))))))

;;; req-2026-11-24-012: navigation visits every visible file and hunk
;; covers: git-appraise-actions.el git-appraise.el
;; covers-fn: git-appraise-next-item, git-appraise-previous-item
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-24-012-file-hunk-navigation ()
  "WHEN a reviewer moves through a diff THE SYSTEM SHALL visit each visible file and hunk in order, one per keypress."
  (let ((git-appraise-actions-test-before-request
         (lambda (directory)
           (with-temp-file (expand-file-name "other.txt" directory)
             (insert "another file\n"))
           (git-appraise-test--run directory "git" "add" "other.txt")
           (git-appraise-test--run directory "git" "commit" "--amend" "--no-edit" "-q"))))
    (git-appraise-actions-test-with-repo
      (let ((git-appraise-lazy-enabled nil)
            (git-appraise-fold-hunks-by-default t)
            (git-appraise-fold-files-by-default nil))
        (git-appraise-test--open-and-render revision)
        (with-current-buffer (git-appraise-pr-buffer-name)
          (should (eq (key-binding (kbd "n")) 'git-appraise-next-item))
          (let* ((items (git-appraise--navigation-sections))
                 (types (mapcar (lambda (s) (oref s type)) items)))
            (should (equal types '(file hunk file hunk)))
            (goto-char (point-min))
            (dolist (item items)
              (git-appraise-next-item)
              (should (= (point) (oref item start))))
            (should-error (git-appraise-next-item) :type 'user-error)
            (dolist (item (cdr (reverse items)))
              (git-appraise-previous-item)
              (should (= (point) (oref item start))))
            (magit-section-hide (car items))
            (let ((visible (git-appraise--navigation-sections)))
              (should (equal (mapcar (lambda (s) (oref s type)) visible)
                             '(file file hunk)))
              (goto-char (point-min))
              (dolist (item visible)
                (git-appraise-next-item)
                (should (= (point) (oref item start)))))))))))

;;; req-2026-11-23-016: set-feedback opens feedback composer
;; covers: git-appraise-actions.el
;; covers-fn: git-appraise-set-feedback
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-016-set-feedback-opens-composer ()
  "WHEN a reviewer sets feedback THE SYSTEM SHALL open the feedback composer with review feedback content."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (unwind-protect
        (cl-letf (((symbol-function 'switch-to-buffer-other-window)
                   (lambda (buf &rest _) (set-buffer buf))))
          (with-current-buffer (git-appraise-pr-buffer-name)
            (git-appraise-set-feedback))
          (should (get-buffer git-appraise-comment-buffer-name))
          (should git-appraise-comment-feedback?)
          (with-current-buffer git-appraise-comment-buffer-name
            (should (string-match-p "Leave a comment here"
                                    (substring-no-properties (buffer-string))))))
      (when (get-buffer git-appraise-comment-buffer-name)
        (kill-buffer git-appraise-comment-buffer-name))
      (git-appraise-comment-reset-global-vars))))

;;; req-2026-11-23-017: needs-work without findings prompts for feedback
;; covers: git-appraise-actions.el git-appraise-comment.el
;; covers-fn: git-appraise-submit-request-changes, git-appraise--submit, git-appraise-set-feedback
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-017-request-changes-prompts-for-feedback ()
  "WHEN a reviewer sends needs-work feedback without findings THE SYSTEM SHALL ask for a summary and publish it."
  (git-appraise-actions-test-with-repo
    (git-appraise-test--open-and-render revision)
    (unwind-protect
        (cl-letf (((symbol-function 'switch-to-buffer-other-window)
                   (lambda (buf &rest _) (set-buffer buf))))
          (with-current-buffer (git-appraise-pr-buffer-name)
            (git-appraise-submit-request-changes))
          (should (equal "NEEDS_WORK" git-appraise-comment-pending-submit-event))
          (should (get-buffer git-appraise-comment-buffer-name))
          (with-current-buffer git-appraise-comment-buffer-name
            (erase-buffer)
            (insert "Needs more work")
            (git-appraise-comment-commit))
          (let ((native (git-appraise-actions-test--show directory revision)))
            (should (eq :false (a-get native 'resolved)))
            (should (equal "REJECTED" (git-appraise-backend--state native))))
          (should-not git-appraise-comment-pending-submit-event))
      (when (get-buffer git-appraise-comment-buffer-name)
        (kill-buffer git-appraise-comment-buffer-name))
      (git-appraise-comment-reset-global-vars))))

;;; req-2026-11-23-018: magit start appraise dispatches to appraise start
;; covers: git-appraise.el
;; covers-fn: git-appraise-magit-start-appraise
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-018-magit-start-appraise ()
  "WHEN a reviewer opens a review from Magit THE SYSTEM SHALL start the appraise review flow."
  (let (called backend)
    (cl-letf (((symbol-function 'git-appraise-start)
               (lambda (&rest _)
                 (interactive)
                 (setq called t
                       backend git-appraise-start-backend))))
      (git-appraise-magit-start-appraise)
      (should called)
      (should (eq 'appraise backend)))))

;;; req-2026-11-23-019: transient dispatch opens review transient
;; covers: git-appraise.el
;; covers-fn: git-appraise-transient-dispatch
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-019-transient-dispatch ()
  "WHEN a reviewer opens the submit menu THE SYSTEM SHALL show the review transient."
  (let (called)
    (cl-letf (((symbol-function 'git-appraise-transient-api)
               (lambda (&rest _) (setq called t))))
      (git-appraise-transient-dispatch)
      (should called))))

;;; git-appraise-actions-test.el ends here
