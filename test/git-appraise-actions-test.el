;;; git-appraise-actions-test.el --- ERT tests for main review actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Action-level coverage for review submission, navigation, and buffers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defvar git-appraise-actions-test-before-request nil
  "Optional fixture callback run before creating a review request.")

(defmacro git-appraise-actions-test-with-repo (&rest body)
  "Run BODY in a disposable git-appraise repository with one review."
  (declare (indent 0))
  `(let ((git-appraise-test--before-request-hook
          git-appraise-actions-test-before-request))
     (git-appraise-test--with-review-repo "git-appraise-actions-test-"
       ,@body)))

(defun git-appraise-actions-test--open-and-render (revision)
  "Open REVISION and render it fully."
  (let ((git-appraise-history-enabled nil))
    (git-appraise-backend-open revision)
    (let ((git-appraise-section-full-refresh? t))
      (git-appraise--build-buffer))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))))

(defun git-appraise-actions-test--show (directory revision)
  "Return native git-appraise JSON for REVISION in DIRECTORY."
  (git-appraise-backend--json
   (git-appraise-test--run directory "git-appraise" "show" "-json" revision)
   "test show"))

;;; req-2026-11-23-007: approve action submits review
;; covers: git-appraise-actions.el
;; covers-fn: git-appraise-submit-approve, git-appraise--submit
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-007-approve-action ()
  "WHEN a reviewer approves the review THE SYSTEM SHALL submit an approval decision."
  (git-appraise-actions-test-with-repo
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "External reload comment" revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (git-appraise-reload))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (should (string-match-p
             "External[ \t\n]+reload[ \t\n]+comment"
             (with-current-buffer (git-appraise-pr-buffer-name)
               (substring-no-properties (buffer-string)))))))

;;; req-2026-11-23-014: PR description popup shows title and body
;; covers: git-appraise-actions.el
;; covers-fn: git-appraise-popup-pr-description, git-appraise-pr-description-insert
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-014-popup-pr-description ()
  "WHEN a reviewer opens the PR description THE SYSTEM SHALL show the review title and description."
  (git-appraise-actions-test-with-repo
    (git-appraise-actions-test--open-and-render revision)
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
      (git-appraise-actions-test--open-and-render revision))
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
        (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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

;;; req-2026-11-23-020: fold file at point toggles file section
;; covers: git-appraise-folding.el
;; covers-fn: git-appraise-fold-file-at-point
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-23-020-fold-file-at-point ()
  "WHEN a reviewer folds a file at point THE SYSTEM SHALL toggle the file section visibility."
  (git-appraise-actions-test-with-repo
    (git-appraise-actions-test--open-and-render revision)
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
(ert-deftest git-appraise-actions/req-2026-11-24-111-folded-file-layout ()
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
        (git-appraise-actions-test--open-and-render revision))
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
(ert-deftest git-appraise-actions/req-2026-11-24-112-persistent-folds ()
  "WHEN a reviewer changes a file fold THE SYSTEM SHALL restore that choice on a later review opening."
  (git-appraise-actions-test-with-repo
    (git-appraise-actions-test--open-and-render revision)
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
    (git-appraise-actions-test--open-and-render revision)
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
(ert-deftest git-appraise-actions/req-2026-11-24-113-hunk-fold-actions ()
  "WHEN a reviewer opens hunks from a file THE SYSTEM SHALL retain those choices across a reload."
  (git-appraise-actions-test-with-repo
    (git-appraise-actions-test--open-and-render revision)
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
(ert-deftest git-appraise-actions/req-2026-11-24-127-fold-toggle-repairs-drift ()
  "WHEN a file section is slot-hidden without an invisible overlay THE SYSTEM SHALL fold it on first toggle."
  (git-appraise-actions-test-with-repo
    (git-appraise-actions-test--open-and-render revision)
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

;;; req-2026-11-24-117: special diff headers do not block review rendering
;; covers: git-appraise-fold-state.el
;; covers-fn: git-appraise-fold-state--hunk-identity, git-appraise-fold-state--hunk-folded-p
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-24-117-special-hunk-folds ()
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
(ert-deftest git-appraise-actions/req-2026-11-24-118-plain-author-name ()
  "WHEN a reviewer opens a review THE SYSTEM SHALL show author names without a mention prefix."
  (git-appraise-actions-test-with-repo
    (git-appraise-actions-test--open-and-render revision)
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
(ert-deftest git-appraise-actions/req-2026-11-24-120-file-change-summary ()
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
        (git-appraise-actions-test--open-and-render revision)
        (with-current-buffer (git-appraise-pr-buffer-name)
          (let ((text (buffer-string)))
            (should (string-match-p "^modified[ \t]+\\+2 -1[ \t]+example\\.txt" text))
            (should (string-match-p "^new file[ \t]+\\+1 -0[ \t]+other\\.txt" text))
            (should-not (string-match-p "load with TAB" text))))
        (let ((git-appraise-show-file-change-summary nil))
          (git-appraise-actions-test--open-and-render revision)
          (with-current-buffer (git-appraise-pr-buffer-name)
            (should-not (string-match-p (regexp-quote "+2 -1")
                                        (buffer-string)))))))))

;;; req-2026-11-24-121: classification precedes aligned file names
;; covers: git-appraise-section-wash.el git-appraise-lazy.el
;; covers-fn: git-appraise-wash--insert-file-heading
;; enforcement: test
(ert-deftest git-appraise-actions/req-2026-11-24-121-file-heading-columns ()
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
        (git-appraise-actions-test--open-and-render revision))
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
(ert-deftest git-appraise-actions/req-2026-11-24-124-plain-change-totals ()
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
(ert-deftest git-appraise-actions/req-2026-11-24-126-file-report-lines ()
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
        (git-appraise-actions-test--open-and-render revision))
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
(ert-deftest git-appraise-actions/req-2026-11-24-114-fold-write-failure ()
  "WHEN a fold choice cannot be saved THE SYSTEM SHALL keep its previous visibility."
  (git-appraise-actions-test-with-repo
    (git-appraise-actions-test--open-and-render revision)
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
(ert-deftest git-appraise-actions/req-2026-11-23-021-comment-add-or-edit-on-hunk ()
  "WHEN a reviewer adds a comment at a hunk THE SYSTEM SHALL open the comment composer."
  (git-appraise-actions-test-with-repo
    (git-appraise-actions-test--open-and-render revision)
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

(provide 'git-appraise-actions-test)
;;; git-appraise-actions-test.el ends here
