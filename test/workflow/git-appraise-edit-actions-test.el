;;; git-appraise-edit-actions-test.el --- Request and comment edits -*- lexical-binding: t; -*-

(require 'a)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-comment-events)
(require 'git-appraise-request-edit)
(require 'git-appraise-test-helpers)

(defun git-appraise-edit-actions-test--show (directory revision)
  "Return native review JSON for REVISION in DIRECTORY."
  (git-appraise-backend--read-review directory revision))

(defun git-appraise-edit-actions-test--at-comment (hash)
  "Move to the published comment section identified by HASH."
  (let (target)
    (magit-map-sections
     (lambda (section)
       (when (and (not target)
                  (slot-boundp section 'value)
                  (let ((value (oref section value)))
                    (and (eieio-object-p value)
                         (slot-exists-p value 'id)
                         (slot-boundp value 'id)
                         (equal (oref value id) hash))))
         (setq target section)))
     magit-root-section)
    (unless target (error "Comment section %s not found" hash))
    (goto-char (oref target start))
    (forward-line 1)))

;;; req-2026-11-27-003: request metadata changes keep review identity
;; covers: git-appraise-request-edit.el git-appraise-backend.el
;; covers-fn: git-appraise-edit-reviewers, git-appraise-retarget-review, git-appraise-edit-review-description
;; enforcement: test
(ert-deftest edit-actions/req-2026-11-27-003-request-fields ()
  "WHEN the request author edits description, reviewers, or target THE SYSTEM SHALL update those fields on the same review."
  (git-appraise-test--with-review-repo "git-appraise-edit-request-"
    (git-appraise-test--run directory "git" "branch" "alternate" "master")
    (let ((buffer (git-appraise-test--open-and-render revision)))
      (with-current-buffer buffer
        (should (transient-get-suffix 'git-appraise-edit-request "d"))
        (git-appraise-edit-reviewers "alice@example.com, bob@example.com")
        (git-appraise-retarget-review "alternate")
        (git-appraise-edit-review-description))
      (with-current-buffer git-appraise-comment-buffer-name
        (erase-buffer)
        (insert "New title\nLonger description")
        (execute-kbd-macro (kbd "C-c C-c")))
      (let* ((review (git-appraise-edit-actions-test--show
                      directory revision))
             (request (a-get review 'request)))
        (should (equal (a-get review 'revision) revision))
        (should (equal (a-get request 'reviewRef) "refs/heads/feature"))
        (should (equal (a-get request 'targetRef) "refs/heads/alternate"))
        (should (equal (a-get request 'reviewers)
                       '("alice@example.com" "bob@example.com")))
        (should (equal (a-get request 'description)
                       "New title\nLonger description")))
      (git-appraise-test--run directory "git" "branch" "included" "feature")
      (with-current-buffer buffer
        (should-error (git-appraise-retarget-review "included")
                      :type 'user-error))
      (git-appraise-test--run directory "git" "config"
                              "user.email" "another@example.com")
      (with-current-buffer buffer
        (should-error (git-appraise-edit-review-description)
                      :type 'user-error)))))

;;; req-2026-11-27-004: published edits retain original discussion
;; covers: git-appraise-comment-events.el git-appraise-backend-read.el
;; covers-fn: git-appraise-edit-published-comment, git-appraise-set-published-comment-status, git-appraise-reopen-finding
;; enforcement: test
(ert-deftest edit-actions/req-2026-11-27-004-published-comment-lifecycle ()
  "WHEN an author edits a published finding THE SYSTEM SHALL retain its history and let reviewers change or reopen its status."
  (git-appraise-test--with-review-repo "git-appraise-edit-comment-"
    (git-appraise-test--run directory "git-appraise" "comment"
                            "-m" "Original finding" "-nmw" revision)
    (let* ((original-review (git-appraise-edit-actions-test--show
                             directory revision))
           (hash (a-get (car (a-get original-review 'comments)) 'hash))
           (buffer (git-appraise-test--open-and-render revision)))
      (with-current-buffer buffer
        (git-appraise-edit-actions-test--at-comment hash)
        (should (eq (key-binding (kbd "e"))
                    #'git-appraise-edit-published-comment))
        (call-interactively (key-binding (kbd "e"))))
      (with-current-buffer "*Git Appraise Published Edit*"
        (erase-buffer)
        (insert "Revised finding")
        (execute-kbd-macro (kbd "C-c C-c")))
      (git-appraise-test--drain-lazy buffer)
      (let ((thread (car (a-get (git-appraise-edit-actions-test--show
                                 directory revision) 'comments))))
        (should (equal (a-get thread 'hash) hash))
        (should (eq (a-get thread 'edited) t))
        (should (equal (a-get-in thread '(original description))
                       "Original finding"))
        (should (equal (a-get-in thread '(comment description))
                       "Revised finding"))
        (should (= 1 (length (a-get thread 'edits)))))
      (with-current-buffer buffer
        (should (string-match-p "Edit history (2)" (buffer-string)))
        (let (history)
          (magit-map-sections
           (lambda (section)
             (when (eq (oref section type) 'git-appraise-edit-history)
               (setq history section)))
           magit-root-section)
          (should history)
          (should (cl-some (lambda (overlay)
                             (overlay-get overlay 'invisible))
                           (overlays-at (oref history content)))))
        (git-appraise-edit-actions-test--at-comment hash)
        (git-appraise-set-published-comment-status "FYI"))
      (should-not (a-get-in (car (a-get (git-appraise-edit-actions-test--show
                                       directory revision) 'comments))
                            '(comment resolved)))
      (with-current-buffer buffer
        (git-appraise-edit-actions-test--at-comment hash)
        (git-appraise-set-published-comment-status "Needs work"))
      (should (eq :false
                  (a-get-in (car (a-get (git-appraise-edit-actions-test--show
                                       directory revision) 'comments))
                            '(comment resolved))))
      (with-current-buffer buffer
        (git-appraise-edit-actions-test--at-comment hash)
        (git-appraise-resolve-comment-at-point "Fixed"))
      (with-current-buffer buffer
        (git-appraise-edit-actions-test--at-comment hash)
        (git-appraise-reopen-finding "Still broken"))
      (should (eq :false
                  (a-get (car (a-get (git-appraise-edit-actions-test--show
                                    directory revision) 'comments))
                         'resolved))))))

;;; req-2026-11-27-006: published edits travel through normal notes sync
;; covers: git-appraise-comment-events.el
;; covers-fn: git-appraise-comment-events--append-edit
;; enforcement: test
(ert-deftest edit-actions/req-2026-11-27-006-edit-pushes-without-force ()
  "WHEN an author edits a shared comment THE SYSTEM SHALL publish the new version through a normal notes push."
  (git-appraise-test--with-review-repo "git-appraise-edit-remote-"
    (git-appraise-test--run directory "git-appraise" "comment"
                            "-m" "Before push" revision)
    (let ((remote (make-temp-file "git-appraise-edit-bare-" t))
          (clone (make-temp-file "git-appraise-edit-clone-" t)))
      (unwind-protect
          (progn
            (git-appraise-test--run remote "git" "init" "--bare" "-q")
            (git-appraise-test--run directory "git" "remote" "add" "origin" remote)
            (git-appraise-test--run directory "git" "push" "-q" "origin"
                                    "master" "feature")
            (git-appraise-test--run directory "git-appraise" "push" "origin")
            (let* ((pr (git-appraise-backend-open revision))
                   (thread (car (a-get (git-appraise-edit-actions-test--show
                                         directory revision) 'comments)))
                   (hash (a-get thread 'hash)))
              (git-appraise-comment-events--append-edit
               pr hash (a-get thread 'comment) "After push" nil))
            (git-appraise-test--run directory "git-appraise" "push" "origin")
            (git-appraise-test--run clone "git" "clone" "-q" remote ".")
            (git-appraise-test--run clone "git-appraise" "pull" "origin")
            (let ((thread (car (a-get (git-appraise-edit-actions-test--show
                                       clone revision) 'comments))))
              (should (equal (a-get-in thread '(comment description))
                             "After push"))
              (should (eq (a-get thread 'edited) t))))
        (delete-directory clone t)
        (delete-directory remote t)))))

(ert-deftest edit-actions/published-edit-rechecks-author-and-version ()
  "A published edit keeps its composer on conflict and rejects another author."
  (git-appraise-test--with-review-repo "git-appraise-edit-guard-"
    (git-appraise-test--run directory "git-appraise" "comment"
                            "-m" "First version" revision)
    (let* ((thread (car (a-get (git-appraise-edit-actions-test--show
                               directory revision) 'comments)))
           (hash (a-get thread 'hash))
           (buffer (git-appraise-test--open-and-render revision)))
      (with-current-buffer buffer
        (git-appraise-edit-actions-test--at-comment hash)
        (git-appraise-edit-published-comment))
      (let ((pr (git-appraise-db-get-pullreq)))
        (git-appraise-comment-events--append-edit
         pr hash (a-get thread 'comment) "Concurrent version" nil))
      (with-current-buffer "*Git Appraise Published Edit*"
        (erase-buffer)
        (insert "Stale version")
        (should-error (git-appraise-comment-events-commit-edit)
                      :type 'user-error)
        (should (buffer-live-p (current-buffer)))
        (git-appraise-comment-events-cancel-edit))
      (git-appraise-test--run directory "git" "config"
                              "user.email" "someone-else@example.com")
      (with-current-buffer buffer
        (git-appraise-edit-actions-test--at-comment hash)
        (should-error (git-appraise-edit-published-comment)
                      :type 'user-error)
        (should-error (git-appraise-set-published-comment-status "FYI")
                      :type 'user-error)))))

;;; req-2026-11-27-007: decision and lifecycle remain distinct
;; covers: git-appraise-backend-read.el git-appraise-section-header.el git-appraise.el
;; covers-fn: git-appraise-backend--decision, git-appraise-backend--lifecycle, git-appraise-review-open-p
;; enforcement: test
(ert-deftest edit-actions/req-2026-11-27-007-review-status-axes ()
  "WHEN a review is rejected and then abandoned THE SYSTEM SHALL show its decision and lifecycle separately."
  (git-appraise-test--with-review-repo "git-appraise-status-axes-"
    (let ((buffer (git-appraise-test--open-and-render revision)))
      (with-current-buffer buffer
        (should (git-appraise-review-open-p))
        (should (string-match-p "Decision:[[:space:]]+PENDING"
                                (buffer-string)))
        (should (string-match-p "Lifecycle:[[:space:]]+OPEN"
                                (buffer-string))))
      (git-appraise-test--run directory "git-appraise" "reject"
                              "-m" "Needs more work" revision)
      (git-appraise-test--run directory "git-appraise" "abandon"
                              "-m" "Stopping this review" revision)
      (with-current-buffer buffer
        (git-appraise-reload))
      (git-appraise-test--drain-lazy buffer)
      (with-current-buffer buffer
        (should-not (git-appraise-review-open-p))
        (should (string-match-p "Decision:[[:space:]]+REJECTED"
                                (buffer-string)))
        (should (string-match-p "Lifecycle:[[:space:]]+ABANDONED"
                                (buffer-string)))))))

(ert-deftest edit-actions/landed-review-retains-decision ()
  "A landed review still shows the reviewer's decision separately."
  (git-appraise-test--with-review-repo "git-appraise-status-landed-"
    (git-appraise-test--run directory "git-appraise" "accept"
                            "-m" "Looks good" revision)
    (git-appraise-test--run directory "git-appraise" "submit"
                            "--fast-forward" revision)
    (let ((buffer (git-appraise-test--open-and-render revision)))
      (with-current-buffer buffer
        (should-not (git-appraise-review-open-p))
        (should (string-match-p "Decision:[[:space:]]+APPROVED"
                                (buffer-string)))
        (should (string-match-p "Lifecycle:[[:space:]]+LANDED"
                                (buffer-string)))))))

(ert-deftest edit-actions/commands-use-their-review-buffer ()
  "Editing one of two open reviews keeps its request and comment context."
  (git-appraise-test--with-review-repo "git-appraise-edit-context-"
    (git-appraise-test--run directory "git-appraise" "comment"
                            "-m" "First review note" revision)
    (let* ((first-revision revision)
           (comment-hash (a-get (car (a-get
                                      (git-appraise-edit-actions-test--show
                                       directory first-revision)
                                      'comments)) 'hash))
           (first-buffer (git-appraise-test--open-and-render first-revision))
           second-buffer)
      (unwind-protect
          (progn
            (git-appraise-test--run directory "git" "checkout" "-qb" "other"
                                    "master")
            (with-temp-file (expand-file-name "other.txt" directory)
              (insert "other review\n"))
            (git-appraise-test--run directory "git" "add" "other.txt")
            (git-appraise-test--run directory "git" "commit" "-qm" "other")
            (let ((second-revision
                   (git-appraise-test--run directory "git" "rev-parse" "HEAD")))
              (git-appraise-test--run directory "git-appraise" "request"
                                      "-quiet" "-m" "Other review"
                                      "-target" "refs/heads/master")
              (setq second-buffer
                    (git-appraise-test--open-and-render second-revision))
              (with-current-buffer first-buffer
                (git-appraise-edit-reviewers "first@example.com")
                (git-appraise-edit-actions-test--at-comment comment-hash)
                (should (equal (oref (car (git-appraise-comment-events--at-point))
                                     number)
                               first-revision)))
              (should (equal '("first@example.com")
                             (a-get-in (git-appraise-edit-actions-test--show
                                        directory first-revision)
                                       '(request reviewers))))
              (should-not (a-get-in (git-appraise-edit-actions-test--show
                                     directory second-revision)
                                    '(request reviewers)))))
        (when (buffer-live-p first-buffer)
          (kill-buffer first-buffer))
        (when (buffer-live-p second-buffer)
          (kill-buffer second-buffer))))))

;;; git-appraise-edit-actions-test.el ends here
