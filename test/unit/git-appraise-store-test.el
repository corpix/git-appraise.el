;;; git-appraise-store-test.el --- EmacSQL review store tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'a)
(require 'emacsql-sqlite)
(require 'git-appraise-store)

(defvar git-appraise-store-test--repo)

(defmacro git-appraise-store-test--with-db (&rest body)
  "Run BODY with an isolated in-memory review database."
  (declare (indent 0))
  `(let ((git-appraise-store-file nil))
     (unwind-protect
         (progn (git-appraise-store-close) ,@body)
       (git-appraise-store-close))))

(defmacro git-appraise-store-test--with-file-db (&rest body)
  "Run BODY with an isolated temporary file database for reopen tests."
  (declare (indent 0))
  `(let ((git-appraise-store-file
          (make-temp-file "git-appraise-store-reopen-" nil ".sqlite")))
     (unwind-protect
         (progn (git-appraise-store-close) ,@body)
       (git-appraise-store-close)
       (ignore-errors (delete-file git-appraise-store-file))
       (ignore-errors (delete-file (concat git-appraise-store-file "-wal")))
       (ignore-errors (delete-file (concat git-appraise-store-file "-shm"))))))

(defmacro git-appraise-store-test--with-repo (&rest body)
  "Run BODY with a disposable git repository."
  (declare (indent 0))
  `(let ((git-appraise-store-test--repo
          (make-temp-file "git-appraise-store-test-repo" t)))
     (unwind-protect
         (progn
           (call-process "git" nil nil nil "-C" git-appraise-store-test--repo
                         "init" "-q" "-b" "master")
           ,@body)
       (delete-directory git-appraise-store-test--repo 'recursive))))

(defun git-appraise-store-test--event (id &optional viewed at actor)
  "A valid viewed event with ID, VIEWED flag, AT and ACTOR."
  `((id . ,id) (actor . ,(or actor "tester@example.com"))
    (item . "hunk") (path . "src/a.el")
    (fingerprint . ,(concat "sha256:" (make-string 64 ?a)))
    (range . "-1,1 +1,1") (viewed . ,viewed)
    (at . ,(or at "2026-01-01T00:00:00.000000Z"))))

;;; req-2026-11-27-001: fresh schema
;; covers: git-appraise-store-schema.el git-appraise-store.el
;; covers-fn: git-appraise-store-schema-create, git-appraise-store-db
;; enforcement: test
(ert-deftest store/req-2026-11-27-001-fresh-schema ()
  "WHEN a fresh review store opens THE SYSTEM SHALL create its current tables."
  (git-appraise-store-test--with-db
    (let ((db (git-appraise-store-db)))
      (should (= git-appraise-store-schema-version
                 (git-appraise-store--user-version db)))
      (dolist (table git-appraise-store--tables)
        (should (zerop (caar (emacsql db
                                     [:select [(funcall count *)]
                                      :from $i1]
                                     table))))))))

;;; req-2026-11-27-002: incompatible store guard
;; covers: git-appraise-store-schema.el
;; covers-fn: git-appraise-store-schema-create
;; enforcement: test
(ert-deftest store/req-2026-11-27-002-reject-old-store ()
  "WHEN an incompatible store is opened THE SYSTEM SHALL require an explicit reset without changing it."
  (emacsql-with-connection (db (emacsql-sqlite-open nil))
    (emacsql db [:create-table legacy [value]])
    (should-error (git-appraise-store-schema-create db) :type 'error)
    (should (= 0 (git-appraise-store--user-version db))))
  (emacsql-with-connection (db (emacsql-sqlite-open nil))
    (emacsql db [:pragma (= user-version 3)])
    (should-error (git-appraise-store-schema-create db) :type 'error)
    (should (= 3 (git-appraise-store--user-version db)))))

;;; req-2026-11-26-005: viewed marks and baselines commit together
;; covers: git-appraise-store.el
;; covers-fn: git-appraise-store-append-events, git-appraise-store-viewed-snapshot
;; enforcement: test
(ert-deftest store/req-2026-11-26-005-viewed-snapshot-atomic ()
  "WHEN saving a viewed baseline fails THE SYSTEM SHALL leave both the baseline and its mark unchanged."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        (should-error
         (git-appraise-store-append-events
          repo "review" 'viewed
          (list (git-appraise-store-test--event "atomic" t))
          (list (list "tester@example.com" "src/a.el" nil))))
        (should-not (git-appraise-store-events repo "review" 'viewed))
        (should-not (git-appraise-store-viewed-snapshot
                     repo "review" "tester@example.com" "src/a.el"))))))

;;; req-2026-11-26-007: viewed baselines stay personal and review scoped
;; covers: git-appraise-store.el
;; covers-fn: git-appraise-store-viewed-snapshots
;; enforcement: test
(ert-deftest store/req-2026-11-26-007-viewed-snapshot-isolation ()
  "WHEN a reviewer resumes a review THE SYSTEM SHALL load only that reviewer's saved file baselines."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        (git-appraise-store-save-viewed-snapshot
         repo "review-a" "alice" "a.el" "old diff\n")
        (git-appraise-store-save-viewed-snapshot
         repo "review-a" "bob" "b.el" "bob diff\n")
        (git-appraise-store-save-viewed-snapshot
         repo "review-b" "alice" "c.el" "other diff\n")
        (let ((saved (git-appraise-store-viewed-snapshots
                      repo "review-a" "alice")))
          (should (equal "old diff\n" (gethash "a.el" saved)))
          (should (= 1 (hash-table-count saved))))))))

;;; req-2026-11-27-003: reviewed history and reviewer isolation
;; covers: git-appraise-store.el
;; covers-fn: git-appraise-store-events, git-appraise-store-append-events
;; enforcement: test
(ert-deftest store/req-2026-11-27-003-viewed-history-and-isolation ()
  "WHEN review marks are loaded THE SYSTEM SHALL retain prior marks and return only the current reviewer's history."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let* ((repo git-appraise-store-test--repo)
             (early (git-appraise-store-test--event "early" t))
             (late (git-appraise-store-test--event
                    "late" nil "2026-01-02T00:00:00.000000Z"))
             (other (git-appraise-store-test--event "other" t nil "other")))
        (git-appraise-store-append-events repo "review" 'viewed
                                          (list late early other early))
        (should (equal (mapcar (lambda (e) (a-get e 'id))
                               (git-appraise-store-events
                                repo "review" 'viewed "tester@example.com"))
                       '("early" "late")))
        (should (= 3 (length (git-appraise-store-events repo "review" 'viewed))))
        (should-not (git-appraise-store-events repo "different" 'viewed))))))

;;; req-2026-11-27-004: current fold choice
;; covers: git-appraise-store.el
;; covers-fn: git-appraise-store-append-events, git-appraise-store-events
;; enforcement: test
(ert-deftest store/req-2026-11-27-004-fold-choice-replaces-older-choice ()
  "WHEN a reviewer changes a fold choice THE SYSTEM SHALL retain only the current choice for that item."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let* ((repo git-appraise-store-test--repo)
             (first (git-appraise-store-test--event "one" t))
             (second (git-appraise-store-test--event "two" nil)))
        (setf (alist-get 'folded first) t
              (alist-get 'folded second) nil)
        (git-appraise-store-append-events repo "review" 'fold (list first))
        (git-appraise-store-append-events repo "review" 'fold (list second))
        (let ((rows (git-appraise-store-events
                     repo "review" 'fold "tester@example.com")))
          (should (= 1 (length rows)))
          (should (equal "two" (a-get (car rows) 'id)))
          (should-not (a-get (car rows) 'folded)))))))

;;; req-2026-11-27-005: current draft
;; covers: git-appraise-store.el
;; covers-fn: git-appraise-store-save-pr-state, git-appraise-store-pr-state
;; enforcement: test
(ert-deftest store/req-2026-11-27-005-draft-replaces-older-draft ()
  "WHEN a reviewer edits unsent feedback THE SYSTEM SHALL recover the latest complete draft."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        (git-appraise-store-save-pr-state
         repo "review" "alice"
         '((v . 2) (id . "old") (feedback . "first")
           (local_comments . nil) (at . "2026-01-01T00:00:00Z")))
        (git-appraise-store-save-pr-state
         repo "review" "alice"
         '((v . 2) (id . "new") (feedback . "second")
           (local_comments . nil) (at . "2026-01-02T00:00:00Z")))
        (should (equal "second" (a-get (git-appraise-store-pr-state
                                        repo "review" "alice") 'feedback)))
        (should (= 1 (caar (emacsql (git-appraise-store-db)
                                    [:select [(funcall count *)]
                                     :from pr-state]))))
        (should-not (git-appraise-store-pr-state repo "review" "bob"))))))

;;; req-2026-11-26-017: drafts roundtrip through JSON
;; covers: git-appraise-store.el
;; covers-fn: git-appraise-store--encode-comments, git-appraise-store--decode-comments
;; enforcement: test
(ert-deftest store/req-2026-11-26-017-draft-comments-roundtrip ()
  "WHEN draft comments are saved THE SYSTEM SHALL read back the same entries, including JSON false flags."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let* ((repo git-appraise-store-test--repo)
             (draft '(((state . "LOCAL COMMENT")
                       (comments (nodes ((internal-id . "d1")
                                         (outdated . :json-false))))))))
        (git-appraise-store-save-pr-state
         repo "review" "alice"
         `((v . 2) (id . "s1") (feedback . nil)
           (local_comments . ,draft) (at . "2026-01-01T00:00:00Z")))
        (should (equal draft (a-get (git-appraise-store-pr-state
                                     repo "review" "alice") 'local_comments)))))))

;;; req-2026-11-27-006: state survives reopen
;; covers: git-appraise-store.el
;; covers-fn: git-appraise-store-close, git-appraise-store-db
;; enforcement: test
(ert-deftest store/req-2026-11-27-006-viewed-mark-survives-reopen ()
  "WHEN the review store is closed and reopened THE SYSTEM SHALL recover saved review marks."
  (git-appraise-store-test--with-file-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo)
            (event (git-appraise-store-test--event "reopen" t)))
        (git-appraise-store-append-events repo "review" 'viewed (list event))
        (git-appraise-store-close)
        (should (equal (list event)
                       (git-appraise-store-events repo "review" 'viewed)))))))

;;; req-2026-11-27-007: repository existence
;; covers: git-appraise-store.el
;; covers-fn: git-appraise-store-contains-repo-p
;; enforcement: test
(ert-deftest store/req-2026-11-27-007-repository-existence ()
  "WHEN personal review state is saved THE SYSTEM SHALL recognize the repository as having stored state."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        (should-not (git-appraise-store-contains-repo-p repo))
        (git-appraise-store-append-events repo "review" 'viewed
                                          (list (git-appraise-store-test--event
                                                 "one" t)))
        (should (git-appraise-store-contains-repo-p repo))))))

;;; git-appraise-store-test.el ends here
