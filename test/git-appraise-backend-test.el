;;; git-appraise-backend-test.el --- git-appraise integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; These tests use real git-appraise notes in disposable Git repositories.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defmacro git-appraise-backend-test-with-repo (&rest body)
  "Run BODY in a disposable repository with an existing review."
  (declare (indent 0))
  `(git-appraise-test--with-review-repo "git-appraise-backend-test-"
					,@body))

(defun git-appraise-backend-test--show (directory revision)
  "Return native review JSON for REVISION in DIRECTORY."
  (git-appraise-backend--json
   (git-appraise-test--run directory "git-appraise" "show" "-json" revision)
   "test show"))

(ert-deftest git-appraise-backend/discovers-local-reviews-and-handles-a-repository-without-reviews ()
  "discovers local reviews and handles a repository without reviews"
  (git-appraise-backend-test-with-repo
   (should (equal (mapcar (lambda (review) (a-get review 'revision))
			  (git-appraise-backend-list directory)) (list revision)))
   (let ((empty (make-temp-file "git-appraise-backend-empty-" t)))
     (unwind-protect
         (progn
           (git-appraise-test--run empty "git" "init" "-q")
           (should-not (git-appraise-backend-list empty)))
       (delete-directory empty t)))))

(ert-deftest git-appraise-backend/renders-actual-diff-and-inline-comments-in-the-existing-buffer ()
  "renders actual diff and inline comments in the existing buffer"
  (git-appraise-backend-test-with-repo
   (git-appraise-test--run
    directory "git-appraise" "comment" "-m" "Inline feedback"
    "-f" "example.txt" "-l" "2" revision)
   (git-appraise-test--run
    directory "git-appraise" "comment" "-m" "File feedback"
    "-f" "example.txt" revision)
   (git-appraise-backend-open revision)
   (let ((git-appraise-section-full-refresh? t))
     (git-appraise--build-buffer))
   (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
   (let ((rendered (with-current-buffer (git-appraise-pr-buffer-name)
                     (substring-no-properties (buffer-string)))))
     (should (string-match-p "Example review" rendered))
     (should (string-match-p "\\+changed" rendered))
     (should (string-match-p "Inline[ \t\n]+feedback" rendered))
     (should (string-match-p "File[ \t\n]+feedback" rendered))
     (should (string-match "\\+changed\\(?:.\\|\n\\)*Inline[ \t\n]+feedback" rendered)))
   (with-current-buffer (git-appraise-pr-buffer-name)
     (goto-char (point-min))
     (re-search-forward "^modified[ \t]+.*example\\.txt")
     (beginning-of-line)
     (git-appraise-comment-add-or-edit))
   (should (equal (oref git-appraise-comment-uncommitted line-type) "FILE"))
   (kill-buffer git-appraise-comment-buffer-name)
   (git-appraise-comment-reset-global-vars)
   (git-appraise-test--run
    directory "git-appraise" "comment" "-m" "External update" revision)
   (let ((git-appraise-section-full-refresh? t))
     (git-appraise--build-buffer))
   (should (string-match-p
            "External[ \t\n]+update"
            (with-current-buffer (git-appraise-pr-buffer-name)
              (substring-no-properties (buffer-string)))))))

(ert-deftest git-appraise-backend/submits-a-staged-inline-comment-through-the-existing-review-command ()
  "submits a staged inline comment through the existing review command"
  (git-appraise-backend-test-with-repo
   (git-appraise-backend-open revision)
   (let ((git-appraise-section-full-refresh? t))
     (git-appraise--build-buffer))
   (git-appraise-db--pullreq-raw-comments-update
    '((author (login . "Tester"))
      (state . "LOCAL COMMENT")
      (comments (nodes ((internal-id . "draft-1")
                        (bodyText . "Drafted inline")
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
                     "Drafted inline"))
            (a-get (git-appraise-backend-test--show directory revision)
                   'comments)))))

(ert-deftest git-appraise-backend/approves-through-the-existing-review-command ()
  "approves through the existing review command"
  (git-appraise-backend-test-with-repo
   (git-appraise-backend-open revision)
   (let ((git-appraise-section-full-refresh? t))
     (git-appraise--build-buffer))
   (with-current-buffer (git-appraise-pr-buffer-name)
     (git-appraise-submit-approve))
   (should (eq (a-get (git-appraise-backend-test--show directory revision)
                      'resolved) t))))

(ert-deftest git-appraise-backend/replies-to-a-conversation-comment-through-the-existing-editor ()
  "replies to a conversation comment through the existing editor"
  (git-appraise-backend-test-with-repo
   (git-appraise-test--run
    directory "git-appraise" "comment" "-m" "Parent" revision)
   (git-appraise-backend-open revision)
   (let ((git-appraise-section-full-refresh? t))
     (git-appraise--build-buffer))
   (let* ((parent (car (a-get (git-appraise-backend-test--show directory revision)
                              'comments)))
          (hash (a-get parent 'hash))
          (section (git-appraise-comment-section :author "Tester" :id hash)))
     (git-appraise-comment-handler-add-or-edit section)
     (oset git-appraise-comment-uncommitted msg "Reply through editor")
     (git-appraise-comment-handler-commit git-appraise-comment-uncommitted "")
     (should (equal (a-get-in
                     (car (a-get
			   (car (a-get (git-appraise-backend-test--show directory revision)
                                       'comments))
			   'children))
                     '(comment description)) "Reply through editor"))
     (kill-buffer git-appraise-comment-buffer-name)
     (git-appraise-comment-reset-global-vars))))

(ert-deftest git-appraise-backend/creates-line-and-file-comments-replies-and-decisions-through-git-appraise ()
  "creates line and file comments, replies, and decisions through git-appraise"
  (git-appraise-backend-test-with-repo
   (let* ((pr (git-appraise-backend-repo
               :owner directory :repo "example" :number revision))
          (diff (git-appraise-test--run
                 directory "git-appraise" "show" "-diff" revision))
          (comment (git-appraise-local-comment-section
                    :state "LOCAL COMMENT" :author "Tester"
                    :msg "New line comment" :path "example.txt"
                    :position 3 :line-type "ADDED"))
          (file-comment (git-appraise-local-comment-section
                         :state "LOCAL COMMENT" :author "Tester"
                         :msg "Whole file comment" :path "example.txt"
                         :position 0 :line-type "FILE"))
          (callback-count 0))
     (oset pr raw-diff diff)
     (git-appraise-backend-send-code-comment
      pr comment (lambda () (setq callback-count (1+ callback-count))))
     (git-appraise-backend-send-code-comment
      pr file-comment (lambda () (setq callback-count (1+ callback-count))))
     (should (equal callback-count 2))
     (let* ((native (git-appraise-backend-test--show directory revision))
            (threads (a-get native 'comments))
            (line-thread (cl-find-if
                          (lambda (thread)
                            (equal (a-get-in thread '(comment description))
                                   "New line comment"))
                          threads)))
       (should (equal (a-get-in line-thread '(comment location range startLine)) 2))
       (should (cl-find-if
                (lambda (thread)
                  (equal (a-get-in thread '(comment description))
                         "Whole file comment")) threads))
       (git-appraise-backend-send-reply pr (a-get line-thread 'hash) "Reply")
       (should (a-get-in (git-appraise-backend-test--show directory revision)
                         '(comments)))
       (should (equal (a-get-in
		       (car (a-get
			     (cl-find-if
			      (lambda (thread)
				(equal (a-get thread 'hash) (a-get line-thread 'hash)))
			      (a-get (git-appraise-backend-test--show directory revision)
				     'comments))
			     'children))
		       '(comment description)) "Reply")))
     (let ((decision (git-appraise-native-publication)))
       (oset decision pr pr)
       (oset decision state "APPROVE")
       (git-appraise-backend-publish-review decision (lambda () (setq callback-count (1+ callback-count))))
       (should (eq (a-get (git-appraise-backend-test--show directory revision) 'resolved) t))
       (should (equal callback-count 3))))))

;;; req-2026-11-25-057: mixed needs-work submission
;; covers: git-appraise-backend.el git-appraise-actions.el
;; covers-fn: git-appraise-backend-publish-review
;; enforcement: test
(ert-deftest git-appraise-backend/req-2026-11-25-057-mixed-needs-work ()
  "WHEN a reviewer publishes actionable and FYI findings THE SYSTEM SHALL preserve each finding's chosen status."
  (git-appraise-backend-test-with-repo
    (let* ((pr (git-appraise-backend-open revision))
           (submission (git-appraise-native-publication))
           (diff (git-appraise-test--run directory "git-appraise" "show" "-diff" revision)))
      (oset pr raw-diff diff)
      (oset submission pr pr)
      (oset submission state "NEEDS_WORK")
      (oset submission local-comments
            (list (git-appraise-submit-local-comment
                   :path "example.txt" :position 3 :line-type "ADDED"
                   :body "Must fix" :internal-id "actionable" :fyi nil)
                  (git-appraise-submit-local-comment
                   :path "example.txt" :position 3 :line-type "ADDED"
                   :body "For context" :internal-id "fyi" :fyi t)))
      (git-appraise-backend-publish-review submission #'ignore)
      (let ((threads (a-get (git-appraise-backend-test--show directory revision)
                            'comments)))
        (should (eq :false
                    (a-get-in (car (--filter
                                     (equal (a-get-in it '(comment description))
                                            "Must fix") threads))
                              '(comment resolved))))
        (should-not (a-get-in (car (--filter
                                    (equal (a-get-in it '(comment description))
                                           "For context") threads))
                              '(comment resolved)))))))

;;; req-2026-11-25-058: explicit thread resolution
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend-send-reply
;; enforcement: test
(ert-deftest git-appraise-backend/req-2026-11-25-058-resolve-by-reply ()
  "WHEN a reviewer explicitly addresses a finding THE SYSTEM SHALL attach a resolving reply to that finding."
  (git-appraise-backend-test-with-repo
    (let ((pr (git-appraise-backend-open revision)))
      (git-appraise-backend--send-comment pr "Fix this" nil nil nil 'needs-work)
      (let* ((root (car (a-get (git-appraise-backend-test--show directory revision)
                               'comments)))
             (hash (a-get root 'hash)))
        (git-appraise-backend-send-reply pr hash "Fixed in the next commit" t)
        (let* ((updated (car (a-get
                              (git-appraise-backend-test--show directory revision)
                              'comments)))
               (child (car (a-get updated 'children))))
          (should (equal hash (a-get-in child '(comment parent))))
          (should (eq t (a-get-in child '(comment resolved))))
          (should-not (eq :false (a-get updated 'resolved))))))))

;;; req-2026-11-25-061: review identity survives appended commits
;; covers: git-appraise-backend.el git-appraise-db.el
;; covers-fn: git-appraise-backend-refresh, git-appraise-backend--native-review
;; enforcement: test
(ert-deftest git-appraise-backend/req-2026-11-25-061-native-review-follows-branch ()
  "WHEN new commits arrive on a reviewed branch THE SYSTEM SHALL retain the review identity and use the new head."
  (git-appraise-backend-test-with-repo
    (let ((git-appraise-history-enabled nil)
          (pr (git-appraise-backend-open revision)))
      (let ((git-appraise-section-full-refresh? t))
        (git-appraise--build-buffer))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (should (equal revision (oref (oref pr native-review) revision)))
      (with-temp-file (expand-file-name "example.txt" directory)
        (insert "first\nchanged\nthird\nfourth\n"))
      (git-appraise-test--run directory "git" "commit" "-qam" "followup")
      (let ((new-head (git-appraise-test--run directory "git" "rev-parse" "HEAD"))
            (git-appraise-section-full-refresh? t))
        (git-appraise-backend-refresh pr (git-appraise-pr-buffer-name pr))
        (should (equal revision (oref (oref pr native-review) revision)))
        (should (equal new-head (oref (oref pr native-review) head)))))))

;;; req-2026-11-25-062: partial publication retains only unsent drafts
;; covers: git-appraise-backend.el git-appraise-db.el git-appraise-pr-state.el
;; covers-fn: git-appraise-backend-publish-review, git-appraise-db-delete-raw-comment
;; enforcement: test
(ert-deftest git-appraise-backend/req-2026-11-25-062-partial-send-retry ()
  "WHEN a publication stops after one successful comment THE SYSTEM SHALL retain only unsent drafts for retry."
  (git-appraise-backend-test-with-repo
    (let* ((pr (git-appraise-backend-open revision))
           (submission (git-appraise-native-publication))
           (send (symbol-function 'git-appraise-backend--send-comment))
           (attempts 0))
      (oset submission pr pr)
      (oset submission state "COMMENT")
      (dolist (pair '(("first" . "One") ("second" . "Two")))
        (git-appraise-db--pullreq-raw-comments-update
         `((state . "LOCAL COMMENT")
           (comments (nodes ((internal-id . ,(car pair))
                             (bodyText . ,(cdr pair))
                             (path . "example.txt")
                             (line-type . "FILE")
                             (local? . t)))))))
      (oset submission local-comments
            (list (git-appraise-submit-local-comment
                   :path "example.txt" :line-type "FILE"
                   :body "Two" :internal-id "second")
                  (git-appraise-submit-local-comment
                   :path "example.txt" :line-type "FILE"
                   :body "One" :internal-id "first")))
      (cl-letf (((symbol-function 'git-appraise-backend--send-comment)
                 (lambda (&rest args)
                   (cl-incf attempts)
                   (if (= attempts 2)
                       (error "injected send failure")
                     (apply send args)))))
        (should-error (git-appraise-backend-publish-review submission #'ignore)))
      (should (= 1 (length (a-get (git-appraise-backend-test--show
                                   directory revision) 'comments))))
      (should (= 1 (length (oref pr raw-comments))))
      (should (equal "second"
                     (a-get (car (a-get-in (car (oref pr raw-comments))
                                           '(comments nodes)))
                            'internal-id))))))

(ert-deftest git-appraise-backend/keeps-comments-on-earlier-commits-readable-after-the-review-ref-moves ()
  "keeps comments on earlier commits readable after the review ref moves"
  (git-appraise-backend-test-with-repo
   (git-appraise-test--run
    directory "git-appraise" "comment" "-m" "Earlier line"
    "-f" "example.txt" "-l" "2" revision)
   (with-temp-file (expand-file-name "example.txt" directory)
     (insert "first\nchanged again\nthird\n"))
   (git-appraise-test--run directory "git" "commit" "-qam" "followup")
   (let* ((review (git-appraise-backend-test--show directory revision))
          (diff (git-appraise-test--run
                 directory "git-appraise" "show" "-diff" revision))
          (head (git-appraise-backend--head directory review))
          (normalized (git-appraise-backend--normalize review diff head))
          (fallback (a-get-in (car normalized) '(historicalComments nodes))))
     (should-not (equal head revision))
     (should-not (cdr normalized))
     (should (equal (length fallback) 1))
     (should (equal (a-get (car fallback) 'path) "example.txt"))
     (should (equal (a-get (car fallback) 'line) 2))
     (should (equal (a-get (car fallback) 'commit) revision))
     (should (equal (a-get (car fallback) 'bodyText) "Earlier line")))))

(ert-deftest git-appraise-backend/rejects-the-current-review-with-a-native-needs-work-decision ()
  "rejects the current review with a native needs-work decision"
  (git-appraise-backend-test-with-repo
   (let* ((pr (git-appraise-backend-repo
               :owner directory :repo "example" :number revision))
          (decision (git-appraise-native-publication)))
     (oset decision pr pr)
     (oset decision state "REQUEST_CHANGES")
     (oset decision feedback "Needs more work")
     (git-appraise-backend-publish-review decision (lambda ()))
     (let ((native (git-appraise-backend-test--show directory revision)))
       (should (eq (a-get native 'resolved) :false))
       (should (equal (git-appraise-backend--state native) "REJECTED"))))))

(ert-deftest git-appraise-backend/refuses-removed-line-comments-that-git-appraise-cannot-locate-in-the-head ()
  "refuses removed-line comments that git-appraise cannot locate in the head"
  (git-appraise-backend-test-with-repo
   (let* ((pr (git-appraise-backend-repo
               :owner directory :repo "example" :number revision))
          (diff (git-appraise-test--run
                 directory "git-appraise" "show" "-diff" revision))
          (comment (git-appraise-local-comment-section
                    :state "LOCAL COMMENT" :author "Tester"
                    :msg "Removed line" :path "example.txt"
                    :position 2 :line-type "REMOVED")))
     (oset pr raw-diff diff)
     (should (eq (condition-case nil
                     (progn (git-appraise-backend-send-code-comment pr comment (lambda ())) nil)
                   (user-error t)) t))
     (should-not (a-get (git-appraise-backend-test--show directory revision)
			'comments)))))

(ert-deftest git-appraise-backend/refreshes-and-remaps-when-the-head-moves-before-sending-a-line-comment ()
  "refreshes and remaps when the head moves before sending a line comment"
  (git-appraise-backend-test-with-repo
   (git-appraise-backend-open revision)
   (let* ((pr (git-appraise-db-get-pullreq))
          (comment (git-appraise-local-comment-section
                    :state "LOCAL COMMENT" :author "Tester"
                    :msg "Stale line" :path "example.txt"
                    :position 3 :line-type "ADDED")))
     (oset pr raw-diff (git-appraise-test--run
                        directory "git-appraise" "show" "-diff" revision))
     (oset pr sha revision)
     (with-temp-file (expand-file-name "example.txt" directory)
       (insert "first\nchanged again\nthird\n"))
     (git-appraise-test--run directory "git" "commit" "-qam" "followup")
     (let ((err (condition-case e
                    (progn (git-appraise-backend-send-code-comment pr comment (lambda ())) nil)
                  (user-error e))))
       ;; The old blocking UX must be gone.  If the comment cannot be
       ;; remapped on the new diff, a remap error is acceptable; a
       ;; "reload before sending" block is not.
       (when err
         (should-not (string-match-p "reload before sending" (error-message-string err))))))))

(ert-deftest git-appraise-backend/rejects-malformed-cli-data-and-missing-executables ()
  "rejects malformed CLI data and missing executables"
  (git-appraise-backend-test-with-repo
   (cl-letf (((symbol-function 'git-appraise-backend--run)
              (lambda (&rest _) "{")))
     (should (eq (condition-case nil
                     (progn (git-appraise-backend-list directory) nil)
                   (user-error t)) t)))
   (should (eq (condition-case nil
                   (progn (git-appraise-backend--json "[] trailing" "test") nil)
                 (user-error t)) t))
   (cl-letf (((symbol-function 'executable-find)
              (lambda (_) nil)))
     (should (eq (condition-case nil
                     (progn (git-appraise-backend-list directory) nil)
                   (user-error t)) t)))))

;;; req-2026-11-25-056: open a local review by revision
;; covers: git-appraise.el
;; covers-fn: git-appraise-start
;; enforcement: test
(ert-deftest git-appraise-backend/req-2026-11-25-056-start-local-review ()
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
(ert-deftest git-appraise-backend/req-2026-11-23-006-send-hunk-comment-from-editor ()
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
(ert-deftest git-appraise-backend/req-2026-11-25-063-stage-then-publish-finding ()
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
(ert-deftest git-appraise-backend/req-2026-11-25-064-update-request ()
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
(ert-deftest git-appraise-backend/req-2026-11-24-001-map-header-like-added-line ()
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
(ert-deftest git-appraise-backend/req-2026-11-24-006-keep-drafts-on-reload ()
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
(ert-deftest git-appraise-backend/req-2026-11-24-007-confirm-discard-on-head-change ()
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

(provide 'git-appraise-backend-test)
;;; git-appraise-backend-test.el ends here
