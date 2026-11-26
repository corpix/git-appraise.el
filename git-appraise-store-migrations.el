;;; git-appraise-store-migrations.el --- Versioned store schema -*- lexical-binding: t; -*-

;;; Commentary:
;; Ordered, immutable schema migrations for the personal review
;; store.  Entry N brings the database to `PRAGMA user_version' N+1.
;; `git-appraise-store-migrate' applies pending entries inside one
;; IMMEDIATE transaction, so the DDL and the version bump roll back
;; together, and two Emacs processes can never double-apply.
;;
;; The DDL is compiled by `git-appraise-sql' from the shared column
;; specs below; migrations only declare what changed.

;;; Code:

(require 'dash)
(require 'sqlite)

(require 'git-appraise-sql)
(require 'git-appraise-time)

(defun git-appraise-store-migrations--mark-columns (flag time type)
  "Column specs for a marks table.
FLAG is the boolean column (`viewed' or `folded'); TIME and TYPE
are the record-time column -- (at :text) in v1, (at_us :integer)
from v2 on."
  `((repo :text :not-null)
    (revision :text :not-null)
    (id :text :not-null)
    (actor :text :not-null)
    (item :text :not-null)
    (path :text :not-null)
    (fingerprint :text :not-null)
    (range :text)
    (,flag :integer :not-null)
    (,time ,type :not-null)))

(defun git-appraise-store-migrations--pr-columns (time type)
  "Column specs for pr_state; TIME and TYPE as in
`git-appraise-store-migrations--mark-columns'."
  `((repo :text :not-null)
    (revision :text :not-null)
    (actor :text :not-null)
    (v :integer :not-null)
    (id :text :not-null)
    (feedback :text)
    (local_comments :text :not-null)
    (,time ,type :not-null)))

(defun git-appraise-store-migrations--import-columns (time type)
  "Column specs for legacy_imports; TIME and TYPE as in
`git-appraise-store-migrations--mark-columns'."
  `((repo :text :not-null)
    (ref :text :not-null)
    (tip :text :not-null)
    (records :integer :not-null)
    (skipped :integer :not-null)
    (,time ,type :not-null)))

(defun git-appraise-store-migrations--table (name columns primary-key)
  "The CREATE TABLE statement for NAME with COLUMNS and PRIMARY-KEY."
  (car (git-appraise-sql-create-table :name name :columns columns
                                      :primary-key primary-key
                                      :without-rowid t)))

(defun git-appraise-store-migrations--v2-specs ()
  "The v1 to v2 rebuild specs, one (TABLE NEW-COLUMNS PRIMARY-KEY
OLD-TABLE OLD-COLUMNS) per table; NEW-COLUMNS are typed column
specs, OLD-COLUMNS the v1 column names whose last entry holds the
ISO stamp being converted to integer microseconds."
  (list
   (list 'viewed_events
         (git-appraise-store-migrations--mark-columns 'viewed 'at_us :integer)
         '(repo revision id)
         'viewed_events_v1
         '(repo revision id actor item path fingerprint range viewed at))
   (list 'fold_events
         (git-appraise-store-migrations--mark-columns 'folded 'at_us :integer)
         '(repo revision id)
         'fold_events_v1
         '(repo revision id actor item path fingerprint range folded at))
   (list 'pr_state
         (git-appraise-store-migrations--pr-columns 'at_us :integer)
         '(repo revision actor id)
         'pr_state_v1
         '(repo revision actor v id feedback local_comments at))
   (list 'legacy_imports
         (git-appraise-store-migrations--import-columns
          'imported_at_us :integer)
         '(repo ref)
         'legacy_imports_v1
         '(repo ref tip records skipped imported_at))))

(defconst git-appraise-store-migrations
  (vector
   ;; 1: initial schema.  Boolean marks keep their full event
   ;; history -- the viewed projection reads superseded events for
   ;; the [CHANGED] signal -- and pr_state keeps every saved
   ;; snapshot, latest wins by (v, at, id).  WITHOUT ROWID clusters
   ;; rows by the primary key, so the per-review reads are plain
   ;; prefix scans.
   (lambda (db)
     (dolist (ddl (list
                   (git-appraise-store-migrations--table
                    'viewed_events
                    (git-appraise-store-migrations--mark-columns
                     'viewed 'at :text)
                    '(repo revision id))
                   (git-appraise-store-migrations--table
                    'fold_events
                    (git-appraise-store-migrations--mark-columns
                     'folded 'at :text)
                    '(repo revision id))
                   (git-appraise-store-migrations--table
                    'pr_state
                    (git-appraise-store-migrations--pr-columns 'at :text)
                    '(repo revision actor id))
                   (git-appraise-store-migrations--table
                    'legacy_imports
                    (git-appraise-store-migrations--import-columns
                     'imported_at :text)
                    '(repo ref))))
       (sqlite-execute db ddl)))
   ;; 2: record time becomes typed data -- integer microseconds
   ;; since the epoch instead of an ISO string.  Numeric ordering
   ;; no longer depends on a fixed-width text format (second- and
   ;; microsecond-precision stamps used to misorder lexically).
   ;; The store converts at the boundary, so callers keep passing
   ;; and receiving ISO strings.
   (lambda (db)
     (git-appraise-store-migrations--timestamp-rebuild
      db (git-appraise-store-migrations--v2-specs)))
   ;; 3: one local diff snapshot per fully viewed file.  The snapshot
   ;; is review progress, not published git-appraise metadata.
   (lambda (db)
     (sqlite-execute
      db (git-appraise-store-migrations--table
          'viewed_snapshots
          '((repo :text :not-null) (revision :text :not-null)
            (actor :text :not-null) (path :text :not-null)
            (diff :text :not-null) (at_us :integer :not-null))
          '(repo revision actor path)))))
  "Ordered schema migrations; entry N brings the database to
version N+1.  Never edit an entry that shipped: add a new one.")

(defun git-appraise-store-migrations--copy-row (db table new-columns row)
  "Copy one OLD-TABLE ROW into TABLE, converting its trailing
stamp with `git-appraise-time-parse-utc' (unparseable stamps keep
0, the epoch)."
  (let ((values (append (butlast row)
                        (list (or (git-appraise-time-parse-utc
                                   (-last-item row))
                                  0)))))
    (let ((ins (git-appraise-sql-insert
                :into table
                :columns (-map #'car new-columns)
                :rows (list values))))
      (sqlite-execute db (car ins) (cdr ins)))))

(defun git-appraise-store-migrations--timestamp-rebuild (db specs)
  "Rebuild each table in SPECS with a typed record-time column.
Each spec is (TABLE NEW-COLUMNS PRIMARY-KEY OLD-TABLE OLD-COLUMNS):
the v1 table is renamed out of the way, the replacement is created
from NEW-COLUMNS, every copied row has its trailing stamp parsed
with `git-appraise-time-parse-utc' (unparseable stamps keep 0, the
epoch, rather than aborting an upgrade), and the old table is
dropped."
  (pcase-dolist (`(,table ,new-columns ,primary-key ,old-table ,old-columns)
                 specs)
    (let ((rows (sqlite-select
                 db (car (git-appraise-sql-select
                          :from table :columns old-columns)))))
      (sqlite-execute db (car (git-appraise-sql-rename-table table old-table)))
      (sqlite-execute db (car (git-appraise-sql-create-table
                               :name table :columns new-columns
                               :primary-key primary-key
                               :without-rowid t)))
      (dolist (row rows)
        (git-appraise-store-migrations--copy-row db table new-columns row))
      (sqlite-execute db (car (git-appraise-sql-drop-table old-table))))))


(defun git-appraise-store-migrations-latest ()
  "The newest schema version this package understands."
  (length git-appraise-store-migrations))

(defun git-appraise-store--user-version (db)
  "The schema version of the open database DB."
  (caar (sqlite-select db (car (git-appraise-sql-pragma 'user_version)))))

(defun git-appraise-store-migrate (db)
  "Bring the open database DB to the newest schema version.
Refuses a database written by a newer version of this package.
Pending migrations and the version bump run inside one IMMEDIATE
transaction, so a concurrent Emacs either waits for the lock and
sees the applied schema, or finds everything rolled back.  Return
the resulting version."
  (let ((latest (git-appraise-store-migrations-latest))
        committed)
    (unwind-protect
        (progn
          (sqlite-execute db "BEGIN IMMEDIATE")
          (prog1
              (let ((version (git-appraise-store--user-version db)))
                (when (> version latest)
                  (error
                   "Store schema v%d is newer than this version understands (v%d)"
                   version latest))
                (dotimes (i latest)
                  (when (>= i version)
                    (funcall (aref git-appraise-store-migrations i) db)
                    (sqlite-execute
                     db (car (git-appraise-sql-pragma
                              'user_version (1+ i))))))
                (sqlite-execute db "COMMIT")
                (git-appraise-store--user-version db))
            (setq committed t)))
      (unless committed
        (ignore-errors (sqlite-execute db "ROLLBACK"))))))

(provide 'git-appraise-store-migrations)
;;; git-appraise-store-migrations.el ends here
