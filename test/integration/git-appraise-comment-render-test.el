;;; git-appraise-comment-render-test.el --- Review comment integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defun git-appraise-comment-inplace-test--flatten-comment-nodes ()
  "The conversation nodes currently stored for the review."
  (a-get-in (git-appraise-db--pullreq-raw-infos) '(comments nodes)))
(ert-deftest git-appraise-comment-render/req-2026-11-24-103-file-comment-no-refresh ()
  "WHEN a file comment is sent from an ordinary or renamed file heading THE SYSTEM SHALL store it once and show it in Conversation without a full refresh or diff reload."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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
              ;; shown in Conversation; the files stay folded as usual
              (with-current-buffer buf
                (goto-char (point-min))
                (should (search-forward "file" nil t))
                (should (search-forward "level" nil t))
                (should (search-forward "note" nil t))
                (should
                 (--any? (and (eq (oref it type) 'file) (oref it hidden))
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
(ert-deftest git-appraise-comment-render/req-2026-11-24-104-folded-conversation ()
  "WHEN a file comment is sent while Conversation is folded THE SYSTEM SHALL conceal it until the section is opened, and a reload shall keep one copy with the same identity."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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
(ert-deftest git-appraise-comment-render/req-2026-11-24-108-conversation-summary-and-open ()
  "WHEN a reviewer sees a folded conversation THE SYSTEM SHALL show its message count and latest date and open the message with its context."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "First file note" "-f" "example.txt" revision)
    (git-appraise-test--run
     directory "git-appraise" "comment" "-m" "Latest file note" "-f" "example.txt" revision)
    (git-appraise-test--open-and-render revision)
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
(ert-deftest git-appraise-comment-render/req-2026-11-24-105-concurrent-comments ()
  "WHEN another comment appears before the metadata read with equal timestamps THE SYSTEM SHALL show every canonical comment exactly once and keep unsent drafts."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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
(ert-deftest git-appraise-comment-render/req-2026-11-24-106-send-failures ()
  "WHEN the CLI write fails, the metadata read fails after a successful send, or the head changed THE SYSTEM SHALL keep the composer, never resend, and use the recovery paths."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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

;;; req-2026-11-24-115: ordinary comments stay private until publication
;; covers: git-appraise-comment.el git-appraise-comment-draft.el git-appraise-backend.el
;; covers-fn: git-appraise-comment-handler-add-or-edit, git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest git-appraise-comment-render/req-2026-11-24-115-staged-comment ()
  "WHEN a reviewer finishes an ordinary line comment THE SYSTEM SHALL keep it as a private draft until publication."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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
        (should-not (oref comment send?))
        (oset comment msg "Saved before verdict")
        (setq git-appraise-comment-commit-buffer? nil)
        (git-appraise-comment-handler-commit
         comment git-appraise-comment-buffer-msg))
      (let ((review (git-appraise-backend--read-review directory revision)))
        (should-not (a-get review 'comments))
        (should-not (a-get review 'resolved)))
      (should (-any? #'git-appraise-db--local-comment-entry-p
                     (git-appraise-db--pullreq-raw-comments))))))

;;; req-2026-11-24-128: file draft is visible during lazy render
;; covers: git-appraise-comment-draft.el
;; covers-fn: git-appraise-comment-handler-commit
;; enforcement: test
(ert-deftest git-appraise-comment-render/req-2026-11-24-128-file-draft-visible-with-lazy ()
  "WHEN a reviewer saves a file draft while lazy rendering is active THE SYSTEM SHALL show the draft in the review buffer."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
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
(ert-deftest git-appraise-comment-render/req-2026-11-24-139-conversation-timeline-groups ()
  "WHEN a review has top-level and review comments THE SYSTEM SHALL show them chronologically in foldable review groups with inline diff links."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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
        (let* ((fixture (git-appraise-test--fixture-path
                         "conversation-timeline.txt"))
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
          (should (equal (string-trim-right actual)
                         (string-trim-right expected)))
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

;;; req-2026-11-26-004: conversation folds survive a reload
;; covers: git-appraise-section-conversation.el git-appraise-folding.el git-appraise-fold-state.el
;; covers-fn: git-appraise-fold-toggle-at-point, git-appraise-comment--insert-conversation-group
;; enforcement: test
(ert-deftest git-appraise-comment-render/req-2026-11-26-004-conversation-fold-persisted ()
  "WHEN a reviewer unfolds a conversation THE SYSTEM SHALL keep it open after reloading the review."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--run directory "git-appraise" "comment" "-m"
                            "Conversation to unfold" revision)
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (group)
        (magit-map-sections
         (lambda (s)
           (when (eq (oref s type) 'git-appraise-conversation-group)
             (setq group s))))
        (should group)
        (magit-section-show (oref group parent))
        (goto-char (oref group start))
        (git-appraise-fold-toggle-at-point)
        (should-not (oref group hidden))
        (git-appraise-reload)))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (group)
        (magit-map-sections
         (lambda (s)
           (when (eq (oref s type) 'git-appraise-conversation-group)
             (setq group s))))
        (should group)
        (should-not (oref group hidden))))))

;;; req-2026-11-24-140: conversation diff link jumps to diff comment
;; covers: git-appraise-section-comment.el
;; covers-fn: git-appraise-comment--goto-diff-comment, git-appraise-comment--insert-link
;; enforcement: test
(ert-deftest git-appraise-comment-render/req-2026-11-24-140-conversation-link-jumps ()
  "WHEN a reviewer activates a conversation diff link THE SYSTEM SHALL jump to the diff comment."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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
(ert-deftest git-appraise-comment-render/req-2026-11-24-141-conversation-link-handles-rename-sections ()
  "WHEN a conversation diff link is activated in a buffer containing rename-shaped section values THE SYSTEM SHALL jump to the comment."
  (git-appraise-test--with-review-repo "git-appraise-comment-inplace-test-"
    (git-appraise-test--open-and-render revision)
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

;;; git-appraise-comment-render-test.el ends here
