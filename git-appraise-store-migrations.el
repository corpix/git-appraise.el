;;; git-appraise-store-migrations.el --- Versioned store schema -*- lexical-binding: t; -*-

;;; Commentary:
;; Ordered, immutable schema migrations for the personal review
;; store.  Entry N brings the database to `PRAGMA user_version' N+1.
;; `git-appraise-store-migrate' applies pending entries inside one
;; IMMEDIATE transaction, so the DDL and the version bump roll back
;; together, and two Emacs processes can never double-apply.

;;; Code:

(require 'sqlite)

(defconst git-appraise-store-migrations
  (vector
   ;; 1: initial schema.  Boolean marks keep their full event
   ;; history -- the viewed projection reads superseded events for
   ;; the [CHANGED] signal -- and pr_state keeps every saved
   ;; snapshot, latest wins by (v, at, id).  WITHOUT ROWID clusters
   ;; rows by the primary key, so the per-review reads are plain
   ;; prefix scans.
   (lambda (db)
     (--each
         (list
          "CREATE TABLE viewed_events (
  repo TEXT NOT NULL,
  revision TEXT NOT NULL,
  id TEXT NOT NULL,
  actor TEXT NOT NULL,
  item TEXT NOT NULL,
  path TEXT NOT NULL,
  fingerprint TEXT NOT NULL,
  \"range\" TEXT,
  viewed INTEGER NOT NULL,
  at TEXT NOT NULL,
  PRIMARY KEY (repo, revision, id)) WITHOUT ROWID"
          "CREATE TABLE fold_events (
  repo TEXT NOT NULL,
  revision TEXT NOT NULL,
  id TEXT NOT NULL,
  actor TEXT NOT NULL,
  item TEXT NOT NULL,
  path TEXT NOT NULL,
  fingerprint TEXT NOT NULL,
  \"range\" TEXT,
  folded INTEGER NOT NULL,
  at TEXT NOT NULL,
  PRIMARY KEY (repo, revision, id)) WITHOUT ROWID"
          "CREATE TABLE pr_state (
  repo TEXT NOT NULL,
  revision TEXT NOT NULL,
  actor TEXT NOT NULL,
  v INTEGER NOT NULL,
  id TEXT NOT NULL,
  feedback TEXT,
  local_comments TEXT NOT NULL,
  at TEXT NOT NULL,
  PRIMARY KEY (repo, revision, actor, id)) WITHOUT ROWID"
          "CREATE TABLE legacy_imports (
  repo TEXT NOT NULL,
  ref TEXT NOT NULL,
  tip TEXT NOT NULL,
  records INTEGER NOT NULL,
  skipped INTEGER NOT NULL,
  imported_at TEXT NOT NULL,
  PRIMARY KEY (repo, ref)) WITHOUT ROWID")
       (sqlite-execute db it))))
  "Ordered schema migrations; entry N brings the database to
version N+1.  Never edit an entry that shipped: add a new one.")

(defun git-appraise-store-migrations-latest ()
  "The newest schema version this package understands."
  (length git-appraise-store-migrations))

(defun git-appraise-store--user-version (db)
  "The schema version of the open database DB."
  (caar (sqlite-select db "PRAGMA user_version")))

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
                     db (format "PRAGMA user_version = %d" (1+ i)))))
                (sqlite-execute db "COMMIT")
                (git-appraise-store--user-version db))
            (setq committed t)))
      (unless committed
        (ignore-errors (sqlite-execute db "ROLLBACK"))))))

(provide 'git-appraise-store-migrations)
;;; git-appraise-store-migrations.el ends here
