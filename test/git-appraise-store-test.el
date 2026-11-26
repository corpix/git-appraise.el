;;; git-appraise-store-test.el --- review store tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'a)
(require 'sqlite)
(require 'git-appraise-store)
(require 'git-appraise-store-migrations)

(defvar git-appraise-store-test--repo)

(defmacro git-appraise-store-test--with-db (&rest body)
  "Run BODY against a fresh temporary store database."
  (declare (indent 0))
  `(let ((git-appraise-store-file
          (make-temp-file "git-appraise-store-test-" nil ".sqlite")))
     (unwind-protect
         (progn
           (git-appraise-store-close)
           ,@body)
       (git-appraise-store-close)
       (ignore-errors (delete-file git-appraise-store-file)))))

(defmacro git-appraise-store-test--with-repo (&rest body)
  "Run BODY with a disposable git repository.
Binds `git-appraise-store-test--repo'."
  (declare (indent 0))
  `(let ((git-appraise-store-test--repo
          (make-temp-file "git-appraise-store-test-repo" t)))
     (unwind-protect
         (progn
           (call-process "git" nil nil nil "-C" git-appraise-store-test--repo
                         "init" "-q" "-b" "master")
           ,@body)
       (delete-directory git-appraise-store-test--repo 'recursive))))

(defun git-appraise-store-test--event (id &optional viewed range path at)
  "A valid viewed event alist with ID, VIEWED flag and RANGE.
AT defaults to a canonical stamp; the store returns timestamps
normalized to six fractional digits."
  `((id . ,id)
    (actor . "tester@example.com")
    (item . ,(if range "hunk" "file"))
    (path . ,(or path "src/a.el"))
    (fingerprint . ,(concat "sha256:" (make-string 64 ?a)))
    ,@(when range `((range . ,range)))
    (viewed . ,viewed)
    (at . ,(or at "2026-01-01T00:00:00.000000Z"))))

;;; req-2026-11-26-010: fresh initialization
;; covers: git-appraise-store-migrate git-appraise-store-migrations
;; enforcement: test
(ert-deftest store/req-2026-11-26-010-fresh-db-initializes-latest-schema ()
  "WHEN the store opens an empty database THE SYSTEM SHALL bring it to the newest schema version."
  (git-appraise-store-test--with-db
    (let ((db (git-appraise-store-db)))
      (should (= (git-appraise-store--user-version db)
                 (git-appraise-store-migrations-latest)))
      (dolist (table git-appraise-store--tables)
        (should (> (length (sqlite-select
                            db "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
                            (list (symbol-name table))))
                   0))))))

;;; req-2026-11-26-011: sequential upgrade
;; covers: git-appraise-store-migrate
;; enforcement: test
(ert-deftest store/req-2026-11-26-011-upgrade-applies-only-pending-migrations ()
  "WHEN the schema gains a migration THE SYSTEM SHALL apply only the pending entries in order."
  (git-appraise-store-test--with-db
    (let ((first (aref git-appraise-store-migrations 0))
          (calls 0)
          db)
      (let ((git-appraise-store-migrations (vector first)))
        (setq db (git-appraise-store-db))
        (should (= (git-appraise-store--user-version db) 1)))
      (let ((git-appraise-store-migrations
             (vector first
                     (lambda (d)
                       (cl-incf calls)
                       (sqlite-execute d "CREATE TABLE added (x)")))))
        (should (= (git-appraise-store-migrate db) 2))
        (should (= calls 1))
        (should (> (length (sqlite-select
                            db "SELECT name FROM sqlite_master WHERE name='added'"))
                   0))))))

;;; req-2026-11-26-012: downgrade refusal
;; covers: git-appraise-store-migrate
;; enforcement: test
(ert-deftest store/req-2026-11-26-012-newer-schema-refused ()
  "WHEN the database was written by a newer version THE SYSTEM SHALL refuse to open it for migration."
  (git-appraise-store-test--with-db
    (let ((db (git-appraise-store-db)))
      (sqlite-execute db (format "PRAGMA user_version = %d"
                                 (1+ (git-appraise-store-migrations-latest))))
      (should-error (git-appraise-store-migrate db) :type 'error))))

;;; req-2026-11-26-013: failed migration rolls back
;; covers: git-appraise-store-migrate git-appraise-store--call-in-write
;; enforcement: test
(ert-deftest store/req-2026-11-26-013-failed-migration-leaves-schema-untouched ()
  "WHEN a migration fails midway THE SYSTEM SHALL roll the schema and version back and keep the connection usable."
  (git-appraise-store-test--with-db
    (let ((db (sqlite-open git-appraise-store-file))
          (broken-registry
           (vector (aref git-appraise-store-migrations 0)
                   (lambda (d)
                     (sqlite-execute d "CREATE TABLE extra (x)")
                     (error "boom")))))
      (unwind-protect
          (progn
            (let ((git-appraise-store-migrations broken-registry))
              (should-error (git-appraise-store-migrate db) :type 'error))
            ;; the whole run rolled back: migration 1's schema and
            ;; version bump are gone along with the broken entry
            (should (= (git-appraise-store--user-version db) 0))
            (should (= (length (sqlite-select
                                db "SELECT name FROM sqlite_master WHERE name='extra'"))
                       0))
            (should (= (length (sqlite-select
                                db "SELECT name FROM sqlite_master WHERE name='viewed_events'"))
                       0))
            ;; the same connection still migrates cleanly afterwards
            (should (= (git-appraise-store-migrate db)
                       (git-appraise-store-migrations-latest))))
        (ignore-errors (sqlite-close db))))))

;;; req-2026-11-24-001: events roundtrip
;; covers: git-appraise-store-append-events git-appraise-store-events
;; covers-fn: git-appraise-store--event-row git-appraise-store--row->event
;; enforcement: test
(ert-deftest store/req-2026-11-24-001-events-roundtrip ()
  "WHEN viewed and fold events are stored THE SYSTEM SHALL return exactly those events on a later read."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let* ((repo git-appraise-store-test--repo)
             (hunk (git-appraise-store-test--event "h1" t "-7,6 +8,7"
                                                   nil "2026-01-01T00:00:00.000000Z"))
             (file (git-appraise-store-test--event "f1" nil nil
                                                   "src/b.el" "2026-01-01T00:00:01.000000Z")))
        (git-appraise-store-append-events repo "rev1" 'viewed (list hunk file))
        (git-appraise-store-append-events repo "rev1" 'fold
                                          (list `((id . "fd1")
                                                  (actor . "tester@example.com")
                                                  (item . "file")
                                                  (path . "src/a.el")
                                                  (fingerprint . ,(concat "sha256:" (make-string 64 ?a)))
                                                  (folded . t)
                                                  (at . "2026-01-01T00:00:00Z"))))
        (should (equal (git-appraise-store-events repo "rev1" 'viewed)
                       (list hunk file)))
        (should (= (length (git-appraise-store-events repo "rev1" 'fold)) 1))
        (should (eq (a-get (car (git-appraise-store-events repo "rev1" 'fold))
                            'folded)
                    t))))))

;;; req-2026-11-24-002: absent state is empty
;; covers: git-appraise-store-events
;; enforcement: test
(ert-deftest store/req-2026-11-24-002-absent-state-is-empty ()
  "WHEN no state exists for a repository or revision THE SYSTEM SHALL return no events without signalling."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        (should (null (git-appraise-store-events repo "missing" 'viewed)))
        (should (null (git-appraise-store-events nil "missing" 'viewed)))
        (should (null (git-appraise-store-events "/nonexistent-dir-xyz" "m" 'fold)))))))

;;; req-2026-11-24-003: append preserves prior
;; covers: git-appraise-store-append-events
;; enforcement: test
(ert-deftest store/req-2026-11-24-003-append-preserves-prior ()
  "WHEN a second batch is stored THE SYSTEM SHALL keep every event of the first batch."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        (git-appraise-store-append-events
         repo "rev1" 'viewed (list (git-appraise-store-test--event "h1" t)))
        (git-appraise-store-append-events
         repo "rev1" 'viewed (list (git-appraise-store-test--event "h2" nil)))
        (should (equal (mapcar #'car (--map (list (a-get it 'id))
                                            (git-appraise-store-events repo "rev1" 'viewed)))
                       '("h1" "h2")))))))

;;; req-2026-11-24-005: write failure reported
;; covers: git-appraise-store-append-events git-appraise-store-save-pr-state
;; enforcement: test
(ert-deftest store/req-2026-11-24-005-write-failure-reported ()
  "WHEN a store write fails THE SYSTEM SHALL signal the failure and leave the store unchanged."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        (should-error
         (git-appraise-store-append-events
          repo "rev1" 'viewed
          (list `((id . "bad") (actor . "tester@example.com")
                  (item . "hunk") (path . "p") (fingerprint . "fp")
                  (range . nil) (viewed . maybe) (at . "2026-01-01T00:00:00Z"))))
         :type 'error)
        (should (null (git-appraise-store-events repo "rev1" 'viewed)))
        (should-error (git-appraise-store-save-pr-state repo nil "a" nil)
                      :type 'error)))))

;;; req-2026-11-26-014: state survives reopening
;; covers: git-appraise-store-db git-appraise-store-close
;; enforcement: test
(ert-deftest store/req-2026-11-26-014-state-survives-reopening ()
  "WHEN the store connection closes and reopens THE SYSTEM SHALL read back everything committed before."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        (git-appraise-store-append-events
         repo "rev1" 'viewed (list (git-appraise-store-test--event "h1" t)))
        (git-appraise-store-close)
        (should (equal (git-appraise-store-events repo "rev1" 'viewed)
                       (list (git-appraise-store-test--event "h1" t))))))))

;;; req-2026-11-26-015: duplicate ids stored once
;; covers: git-appraise-store-append-events
;; enforcement: test
(ert-deftest store/req-2026-11-26-015-duplicate-ids-stored-once ()
  "WHEN the same event id is stored twice THE SYSTEM SHALL keep one row."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo)
            (event (git-appraise-store-test--event "h1" t)))
        (git-appraise-store-append-events repo "rev1" 'viewed (list event))
        (git-appraise-store-append-events repo "rev1" 'viewed (list event))
        (should (= (length (git-appraise-store-events repo "rev1" 'viewed)) 1))))))

;;; req-2026-11-26-016: pr-state precedence
;; covers: git-appraise-store-save-pr-state git-appraise-store-pr-state
;; enforcement: test
(ert-deftest store/req-2026-11-26-016-pr-state-newest-snapshot-wins ()
  "WHEN snapshots accumulate THE SYSTEM SHALL return the highest version, then the newest timestamp, then the largest id."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        (git-appraise-store-save-pr-state
         repo "rev1" "a@x"
         `((v . 1) (id . "old") (feedback . "legacy") (local_comments . nil)
           (at . "2026-01-01T00:00:00Z")))
        (should (equal (a-get (git-appraise-store-pr-state repo "rev1" "a@x")
                              'feedback)
                       "legacy"))
        (git-appraise-store-save-pr-state
         repo "rev1" "a@x"
         `((v . 2) (id . "new") (feedback . "current") (local_comments . nil)
           (at . "2026-01-02T00:00:00Z")))
        (should (equal (a-get (git-appraise-store-pr-state repo "rev1" "a@x")
                              'feedback)
                       "current"))
        ;; same version: newest at wins, then the largest id
        (git-appraise-store-save-pr-state
         repo "rev1" "a@x"
         `((v . 2) (id . "aaa") (feedback . "tie-old") (local_comments . nil)
           (at . "2026-01-02T00:00:00Z")))
        (should (equal (a-get (git-appraise-store-pr-state repo "rev1" "a@x")
                              'feedback)
                       "current"))
        (git-appraise-store-save-pr-state
         repo "rev1" "a@x"
         `((v . 2) (id . "zzz") (feedback . "tie-new") (local_comments . nil)
           (at . "2026-01-02T00:00:00Z")))
        (should (equal (a-get (git-appraise-store-pr-state repo "rev1" "a@x")
                              'feedback)
                       "tie-new"))
        ;; other actors and revisions do not interfere
        (should (null (git-appraise-store-pr-state repo "rev1" "b@x")))
        (should (null (git-appraise-store-pr-state repo "rev2" "a@x")))))))

;;; req-2026-11-26-017: drafts roundtrip through JSON
;; covers: git-appraise-store-save-pr-state git-appraise-store-pr-state
;; covers-fn: git-appraise-store--encode-comments git-appraise-store--decode-comments
;; enforcement: test
(ert-deftest store/req-2026-11-26-017-draft-comments-roundtrip ()
  "WHEN draft comments are saved THE SYSTEM SHALL read back the same entries, including JSON false flags."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo)
            (draft `(((state . "LOCAL COMMENT")
                      (comments (nodes ((internal-id . "d1")
                                        (bodyText . "note")
                                        (local? . t)
                                        (outdated . :json-false)
                                        (position . 3))))))))
        (git-appraise-store-save-pr-state
         repo "rev1" "a@x"
         `((v . 2) (id . "s1") (feedback . nil)
           (local_comments . ,draft) (at . "2026-01-01T00:00:00Z")))
        (let ((snap (git-appraise-store-pr-state repo "rev1" "a@x")))
          (should (null (a-get snap 'feedback)))
          (should (equal (a-get snap 'local_comments) draft)))))))

;;; req-2026-11-26-018: repository identity
;; covers: git-appraise-store-repo-key
;; enforcement: test
(ert-deftest store/req-2026-11-26-018-linked-worktrees-share-identity ()
  "WHEN a review runs in a linked worktree THE SYSTEM SHALL store state under the main repository's identity, and non-repositories have none."
  (git-appraise-store-test--with-repo
    (let* ((repo git-appraise-store-test--repo)
           (wt (expand-file-name "wt" repo))
           (key (git-appraise-store-repo-key repo)))
      (should (stringp key))
      (should (= (call-process "git" nil nil nil "-C" repo
                               "worktree" "add" "-q" wt "-b" "store-test-wt")
                 0))
      (should (equal (git-appraise-store-repo-key wt) key))
      (should (null (git-appraise-store-repo-key
                     (make-temp-file "git-appraise-store-test-norepo" t)))))))

;;; req-2026-11-26-019: existing-state probe
;; covers: git-appraise-store-contains-repo-p
;; enforcement: test
(ert-deftest store/req-2026-11-26-019-contains-repo-tracks-any-state ()
  "WHEN any state is stored for a repository THE SYSTEM SHALL report the repository as present."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        (should-not (git-appraise-store-contains-repo-p repo))
        (git-appraise-store-append-events
         repo "rev1" 'viewed (list (git-appraise-store-test--event "h1" t)))
        (should (git-appraise-store-contains-repo-p repo))
        (should-not (git-appraise-store-contains-repo-p nil))))))

;;; req-2026-11-26-030: typed record time
;; covers: git-appraise-time-parse-utc git-appraise-time-from-epoch-us
;; covers-fn: git-appraise-store--event-row
;; enforcement: test
(ert-deftest store/req-2026-11-26-030-timestamps-order-numerically ()
  "WHEN stamps of different precision order THE SYSTEM SHALL order them by actual time, not lexically."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let ((repo git-appraise-store-test--repo))
        ;; second-precision stamp stored FIRST sorts before the
        ;; half-second one; lexical text order would get this wrong
        (git-appraise-store-append-events
         repo "rev1" 'viewed
         (list (git-appraise-store-test--event "h1" t nil nil
                                               "2026-01-01T00:00:00Z")))
        (git-appraise-store-append-events
         repo "rev1" 'viewed
         (list (git-appraise-store-test--event "h2" t nil nil
                                               "2026-01-01T00:00:00.500000Z")))
        (should (equal (--map (a-get it 'id)
                              (git-appraise-store-events repo "rev1" 'viewed))
                       '("h1" "h2")))
        ;; both stamps read back in the canonical six-digit form
        (should (equal (a-get (car (git-appraise-store-events
                                    repo "rev1" 'viewed))
                              'at)
                       "2026-01-01T00:00:00.000000Z"))))))

;;; req-2026-11-26-031: v1 to v2 upgrade converts timestamps
;; covers: git-appraise-store-migrate git-appraise-store-migrations--timestamp-rebuild
;; enforcement: test
(ert-deftest store/req-2026-11-26-031-upgrade-converts-legacy-timestamps ()
  "WHEN a v1 database upgrades THE SYSTEM SHALL convert every stored stamp to integer microseconds and keep it readable."
  (git-appraise-store-test--with-db
    (git-appraise-store-test--with-repo
      (let* ((first (aref git-appraise-store-migrations 0))
             (repo git-appraise-store-test--repo)
             (repo-key (git-appraise-store-repo-key repo))
             (db (sqlite-open git-appraise-store-file)))
        (unwind-protect
            (progn
              ;; a v1 database with one ISO-stamped row
              (let ((git-appraise-store-migrations (vector first)))
                (git-appraise-store-migrate db)
                (sqlite-execute
                 db (concat "INSERT INTO viewed_events VALUES"
                            " (?, 'rev1', 'h1', 'a@x', 'file', 'p', 'fp',"
                            " NULL, 1, '2026-01-01T00:00:00Z')")
                 (list repo-key)))
              (sqlite-close db)
              (setq git-appraise-store--db nil)
              ;; an ordinary open upgrades through the real registry
              (let ((events (git-appraise-store-events repo "rev1" 'viewed)))
                (should (= (git-appraise-store--user-version
                            (git-appraise-store-db))
                           (git-appraise-store-migrations-latest)))
                (should (= (length events) 1))
                (should (equal (a-get (car events) 'at)
                               "2026-01-01T00:00:00.000000Z"))
                (let ((stamp (caar (sqlite-select
                                    (git-appraise-store-db)
                                    "SELECT at_us FROM viewed_events"))))
                  (should (integerp stamp))
                  (should (= stamp (git-appraise-time-parse-utc
                                    "2026-01-01T00:00:00Z"))))))
          (ignore-errors (sqlite-close db)))))))

(provide 'git-appraise-store-test)
;;; git-appraise-store-test.el ends here
