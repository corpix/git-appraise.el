;;; git-appraise-sql-test.el --- query builder tests -*- lexical-binding: t; -*-

;;; req-2026-11-26-001:
;; covers: git-appraise-sql-select git-appraise-sql-insert
;; covers-fn: git-appraise-sql--where git-appraise-sql--id
;; enforcement: test

(require 'ert)
(require 'dash)
(require 'sqlite)
(require 'git-appraise-sql)

;;; req-2026-11-26-001: placeholder binding
(ert-deftest sql/req-2026-11-26-001-select-binds-values-as-placeholders ()
  "WHEN a select is compiled THE SYSTEM SHALL bind every value through placeholders in order."
  (pcase-let* ((`(,sql . ,params)
                (git-appraise-sql-select
                 :from 'viewed_events
                 :columns '(id actor at)
                 :where `(:and (= repo ,"/r/proj/.git") (= revision ,"abc"))
                 :order-by '((at asc) (id desc)))))
    (should (equal sql
                   (concat "SELECT \"id\", \"actor\", \"at\" FROM \"viewed_events\""
                           " WHERE (\"repo\" = ? AND \"revision\" = ?)"
                           " ORDER BY \"at\" ASC, \"id\" DESC")))
    (should (equal params '("/r/proj/.git" "abc")))))

;;; req-2026-11-26-002: identifier safety
;; covers: git-appraise-sql--id
;; enforcement: test
(ert-deftest sql/req-2026-11-26-002-unsafe-identifiers-refused ()
  "WHEN an identifier carries SQL syntax THE SYSTEM SHALL refuse to compile it."
  (dolist (bad '("repo; DROP TABLE x" "repo--x" "Repo" "repo x" "1x" ""))
    (should-error (git-appraise-sql--id bad) :type 'error))
  (should (equal (git-appraise-sql--id :repo) "\"repo\""))
  (should (equal (git-appraise-sql--id 'repo) "\"repo\"")))

;;; req-2026-11-26-003: where operators
;; covers: git-appraise-sql--where
;; enforcement: test
(ert-deftest sql/req-2026-11-26-003-where-forms-compile ()
  "WHEN where clauses combine THE SYSTEM SHALL compile and, or, not, in and null tests with ordered parameters."
  (pcase-let* ((`(,sql . ,params)
                (git-appraise-sql--where
                 `(:and (:or (= a ,1) (:= a nil))
                        (:in b (,2 ,3))
                        (:not (:is-null c)))
                 nil)))
    (should (equal sql
                   (concat "((\"a\" = ? OR \"a\" IS NULL)"
                           " AND \"b\" IN (?, ?)"
                           " AND NOT \"c\" IS NULL)")))
    (should (equal params '(1 2 3)))))

;;; req-2026-11-26-004: insert compiles rows
;; covers: git-appraise-sql-insert
;; enforcement: test
(ert-deftest sql/req-2026-11-26-004-insert-binds-every-row ()
  "WHEN rows are inserted THE SYSTEM SHALL compile one values list per row with all values bound in order."
  (pcase-let* ((`(,sql . ,params)
                (git-appraise-sql-insert
                 :into 'fold_events
                 :columns '(repo revision id)
                 :rows '(("/r" "rev" "id1") ("/r" "rev" "id2"))
                 :on-conflict :ignore)))
    (should (equal sql
                   (concat "INSERT OR IGNORE INTO \"fold_events\""
                           " (\"repo\", \"revision\", \"id\")"
                           " VALUES (?, ?, ?), (?, ?, ?)")))
    (should (equal params '("/r" "rev" "id1" "/r" "rev" "id2")))
    (should-error (git-appraise-sql-insert
                   :into 'fold_events :columns '(a b) :rows '((1)))
                  :type 'error)))

;;; req-2026-11-26-005: unconditional delete refused
;; covers: git-appraise-sql-delete
;; enforcement: test
(ert-deftest sql/req-2026-11-26-005-unconditional-delete-refused ()
  "WHEN a delete carries no where clause THE SYSTEM SHALL refuse to compile it."
  (should-error (git-appraise-sql-delete :from 'viewed_events) :type 'error)
  (pcase-let ((`(,sql . ,params)
               (git-appraise-sql-delete :from 'viewed_events
                                        :where '(:= repo "/r"))))
    (should (equal sql "DELETE FROM \"viewed_events\" WHERE \"repo\" = ?"))
    (should (equal params '("/r")))))

;;; req-2026-11-26-007: DDL compiles
;; covers: git-appraise-sql-create-table git-appraise-sql-rename-table git-appraise-sql-drop-table git-appraise-sql-pragma
;; enforcement: test
(ert-deftest sql/req-2026-11-26-007-ddl-compiles-from-specs ()
  "WHEN a table schema is declared THE SYSTEM SHALL compile it with quoted identifiers, constraints and no bound values."
  (pcase-let ((`(,sql . ,params)
               (git-appraise-sql-create-table
                :name 'viewed_events
                :columns '((repo :text :not-null)
                           (revision :text :not-null)
                           (id :text :not-null)
                           (range :text)
                           (viewed :integer :not-null)
                           (at_us :integer :not-null))
                :primary-key '(repo revision id)
                :without-rowid t)))
    (should (equal sql
                   (concat "CREATE TABLE \"viewed_events\" ("
                           "\"repo\" TEXT NOT NULL, \"revision\" TEXT NOT NULL,"
                           " \"id\" TEXT NOT NULL, \"range\" TEXT,"
                           " \"viewed\" INTEGER NOT NULL,"
                           " \"at_us\" INTEGER NOT NULL,"
                           " PRIMARY KEY (\"repo\", \"revision\", \"id\")"
                           ") WITHOUT ROWID")))
    (should (null params))
    (should-error (git-appraise-sql-create-table
                   :name 't :columns '((a :blob))) :type 'error))
  (should (equal (car (git-appraise-sql-rename-table 'a 'b))
                 "ALTER TABLE \"a\" RENAME TO \"b\""))
  (should (equal (car (git-appraise-sql-drop-table 'a)) "DROP TABLE \"a\""))
  (should (equal (car (git-appraise-sql-pragma 'user_version))
                 "PRAGMA \"user_version\""))
  (should (equal (car (git-appraise-sql-pragma 'user_version 2))
                 "PRAGMA \"user_version\" = 2")))

;;; req-2026-11-26-008: count select
;; covers: git-appraise-sql-select
;; enforcement: test
(ert-deftest sql/req-2026-11-26-008-count-select-compiles ()
  "WHEN a select asks for a count THE SYSTEM SHALL compile count(*) with the where clause bound."
  (pcase-let ((`(,sql . ,params)
               (git-appraise-sql-select :from 'viewed_events :count t
                                        :where '(:= repo "/r"))))
    (should (equal sql
                   "SELECT count(*) FROM \"viewed_events\" WHERE \"repo\" = ?"))
    (should (equal params '("/r")))))

;;; req-2026-11-26-006: compiled statements execute
;; covers: git-appraise-sql-select git-appraise-sql-insert
;; covers-fn: git-appraise-sql-create-table
;; enforcement: test
(ert-deftest sql/req-2026-11-26-006-compiled-statements-execute ()
  "WHEN compiled statements run against SQLite THE SYSTEM SHALL round-trip the bound values."
  (let ((db (sqlite-open)))
    (unwind-protect
        (progn
          (sqlite-execute
           db (car (git-appraise-sql-create-table
                    :name 't
                    :columns '((a :integer :not-null) (b :text))
                    :primary-key '(a))))
          (pcase-let ((`(,insert . ,params)
                       (git-appraise-sql-insert
                        :into 't :columns '(a b)
                        :rows '((1 "x") (2 nil)))))
            (sqlite-execute db insert params))
          (pcase-let ((`(,select . ,params)
                       (git-appraise-sql-select
                        :from 't :columns '(a b)
                        :where '(:is-null b))))
            (should (equal (sqlite-select db select params) '((2 nil)))))
          (pcase-let ((`(,count-sql . ,count-params)
                       (git-appraise-sql-select :from 't :count t)))
            (should (= (caar (sqlite-select db count-sql count-params)) 2)))
          (pcase-let ((`(,delete . ,params)
                       (git-appraise-sql-delete :from 't :where '(:= a 1))))
            (sqlite-execute db delete params))
          (should (equal (caar (sqlite-select db "SELECT count(*) FROM t")) 1)))
      (sqlite-close db))))

(provide 'git-appraise-sql-test)
;;; git-appraise-sql-test.el ends here
