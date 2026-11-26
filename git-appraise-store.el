;;; git-appraise-store.el --- Durable local review state (SQLite) -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal, durable review state -- viewed marks, fold choices and
;; unsent feedback/drafts -- in one SQLite database under the user's
;; Emacs directory (2026-11-26 store plan; replaces the git notes
;; transport).
;;
;; This file owns the connection and the row mapping and nothing
;; else: callers pass repo/revision and receive the same event
;; alists the projection reducers have always consumed.  SQL is
;; built by `git-appraise-sql', the schema lives in
;; `git-appraise-store-migrations', and the one-shot git-notes
;; import is `git-appraise-store-import'.  A repository's identity
;; is its canonical common git dir, so linked worktrees share state
;; the way git notes did.

;;; Code:

(require 'a)
(require 'dash)
(require 'json)
(require 'subr-x)
(require 'sqlite)

(require 'git-appraise-sql)
(require 'git-appraise-store-migrations)
(require 'git-appraise-utils)

(defcustom git-appraise-store-file
  (expand-file-name "git-appraise/state.sqlite" user-emacs-directory)
  "SQLite database file holding personal review state.
Created on first use; the parent directory is created too."
  :group 'git-appraise
  :type 'file)

(defconst git-appraise-store--max-params 900
  "Bound values per statement.
Well below SQLite's historical 999-variable limit, so one insert
statement never depends on the build's SQLITE_MAX_VARIABLE_NUMBER.")

(defconst git-appraise-store--tables
  '(viewed_events fold_events pr_state legacy_imports)
  "Every table the schema defines.")

(defvar git-appraise-store--db nil
  "The open store connection, or nil.")

(defvar git-appraise-store--repo-keys (make-hash-table :test 'equal)
  "Cache of repository directory to store identity.")

;;; Lifecycle

(defun git-appraise-store-available-p ()
  "Non-nil when this Emacs can open the store."
  (and (fboundp 'sqlite-available-p)
       (sqlite-available-p)))

(defun git-appraise-store-db ()
  "The open store connection; opened and migrated on first use."
  (or git-appraise-store--db
      (setq git-appraise-store--db (git-appraise-store--open))))

(defun git-appraise-store--open ()
  (unless (git-appraise-store-available-p)
    (error "git-appraise needs an Emacs built with SQLite support"))
  (let* ((file (expand-file-name git-appraise-store-file))
         ;; `sqlite-open' returns nil instead of signalling when the
         ;; directory cannot be created or the file is not a database.
         (db (progn
               (make-directory (or (file-name-directory file) ".") t)
               (sqlite-open file))))
    (unless db
      (error "Could not open the review store: %s" file))
    (sqlite-execute db "PRAGMA busy_timeout = 5000")
    (sqlite-execute db "PRAGMA journal_mode = WAL")
    (sqlite-execute db "PRAGMA foreign_keys = ON")
    (git-appraise-store-migrate db)
    db))

(defun git-appraise-store-close ()
  "Close the store connection, if one is open."
  (when git-appraise-store--db
    (ignore-errors (sqlite-close git-appraise-store--db))
    (setq git-appraise-store--db nil)))

(add-hook 'kill-emacs-hook #'git-appraise-store-close)

;;; Transactions and helpers

(defun git-appraise-store--call-in-write (thunk)
  "Run THUNK inside one IMMEDIATE transaction on the store.
Roll back on any non-local exit; return THUNK's result."
  (let ((db (git-appraise-store-db))
        committed)
    (unwind-protect
        (progn
          (sqlite-execute db "BEGIN IMMEDIATE")
          (let ((result (funcall thunk)))
            (sqlite-execute db "COMMIT")
            (setq committed t)
            result))
      (unless committed
        (ignore-errors (sqlite-execute db "ROLLBACK"))))))

(defun git-appraise-store--count (db table repo-key)
  "The rows of TABLE belonging to REPO-KEY."
  (unless (memq table git-appraise-store--tables)
    (error "Unknown table: %S" table))
  (caar (sqlite-select db
                       (format "SELECT count(*) FROM \"%s\" WHERE repo = ?" table)
                       (list repo-key))))

(defun git-appraise-store-contains-repo-p (repo)
  "Non-nil when the store already holds any state for REPO."
  (if-let* ((repo-key (git-appraise-store-repo-key repo)))
      (let ((db (git-appraise-store-db)))
        (--any? (> (git-appraise-store--count db it repo-key) 0)
                git-appraise-store--tables))
    nil))

;;; Repository identity

(defun git-appraise-store-repo-key (repo)
  "The store identity of the repository at directory REPO.
Its canonical common git dir: linked worktrees share one
identity, like git notes did.  Nil when REPO is not a git
repository.  Memoized per session."
  (let* ((dir (and repo (expand-file-name repo)))
         (hit (and dir (gethash dir git-appraise-store--repo-keys 'miss))))
    (if (not (eq hit 'miss))
        hit
      (let ((key (git-appraise-store--repo-key dir)))
        (puthash dir key git-appraise-store--repo-keys)
        key))))

(defun git-appraise-store--repo-key (dir)
  (let ((common (and (file-directory-p dir)
                     (git-appraise-utils--git dir "rev-parse" "--git-common-dir"))))
    (when common
      (file-truename
       (directory-file-name (expand-file-name common dir))))))

;;; Boolean marks: viewed and fold events

(defun git-appraise-store--table (kind)
  "The table holding KIND's events."
  (pcase kind
    (`viewed 'viewed_events)
    (`fold 'fold_events)
    (_ (error "Unknown mark kind: %S" kind))))

(defun git-appraise-store--flag (kind)
  "The boolean flag column (and alist key) of KIND."
  (if (eq kind 'viewed) 'viewed 'folded))

(defun git-appraise-store--event-columns (kind)
  "The event columns an insert names, in row order."
  `(repo revision id actor item path fingerprint range
         ,(git-appraise-store--flag kind) at))

(defun git-appraise-store--event-select-columns (kind)
  "The event columns a read returns, in row order."
  `(id actor item path fingerprint range
       ,(git-appraise-store--flag kind) at))

(defun git-appraise-store--event-row (kind event repo-key revision)
  "The full insert row for event alist EVENT of KIND."
  (list repo-key
        revision
        (or (a-get event 'id) (error "Event without id"))
        (or (a-get event 'actor) (error "Event without actor"))
        (or (a-get event 'item) (error "Event without item"))
        (or (a-get event 'path) (error "Event without path"))
        (or (a-get event 'fingerprint) (error "Event without fingerprint"))
        (a-get event 'range)
        (let ((flag (a-get event (git-appraise-store--flag kind))))
          (unless (memq flag '(t nil))
            (error "Bad %s flag in event %s"
                   (git-appraise-store--flag kind) (a-get event 'id)))
          (if (eq flag t) 1 0))
        (or (a-get event 'at) (error "Event without at"))))

(defun git-appraise-store--row->event (kind row)
  "The event alist for a stored KIND row ROW."
  (pcase-let ((`(,id ,actor ,item ,path ,fingerprint ,range ,flag ,at) row))
    `((id . ,id)
      (actor . ,actor)
      (item . ,item)
      (path . ,path)
      (fingerprint . ,fingerprint)
      ,@(when range `((range . ,range)))
      (,(git-appraise-store--flag kind) . ,(if (and flag (= flag 1)) t nil))
      (at . ,at))))

(defun git-appraise-store-events (repo revision kind)
  "All stored events of KIND for REPOSITORY REVISION, oldest first.
KIND is `viewed' or `fold'.  The event alists are exactly what the
projection reducers consume; a repository without state, or a nil
REPO, has no events."
  (if-let* ((repo-key (git-appraise-store-repo-key repo)))
      (pcase-let* ((`(,sql . ,params)
                    (git-appraise-sql-select
                     :from (git-appraise-store--table kind)
                     :columns (git-appraise-store--event-select-columns kind)
                     :where `(:and (= repo ,repo-key) (= revision ,revision))
                     :order-by '((at asc) (id asc))))
                   (rows (sqlite-select (git-appraise-store-db) sql params)))
        (--map (git-appraise-store--row->event kind it) rows))
    nil))

(defun git-appraise-store--insert-chunked (db table columns rows)
  "Insert ROWS into TABLE, several statements' worth if needed.
Rows are split so no statement binds more than
`git-appraise-store--max-params' values; the caller owns the
transaction, so a split batch still commits whole or not at all."
  (let ((per (max 1 (/ git-appraise-store--max-params (length columns)))))
    (--each (-partition-all per rows)
      (pcase-let ((`(,sql . ,params)
                   (git-appraise-sql-insert :into table :columns columns
                                            :rows it :on-conflict :ignore)))
        (sqlite-execute db sql params)))))

(defun git-appraise-store-append-events (repo revision kind events)
  "Store EVENTS of KIND for REPOSITORY REVISION in one transaction.
EVENTS are event alists as produced by the mark commands.  Any
failure signals, leaving the store unchanged; a batch with a
duplicate id is stored once.  Return EVENTS."
  (let* ((repo-key (or (git-appraise-store-repo-key repo)
                       (error "No review repository to store %s marks in" kind)))
         (rows (--map (git-appraise-store--event-row kind it repo-key revision)
                      events)))
    (when rows
      (git-appraise-store--call-in-write
       (lambda ()
         (git-appraise-store--insert-chunked
          git-appraise-store--db (git-appraise-store--table kind)
          (git-appraise-store--event-columns kind) rows))))
    events))

;;; Unsent review state (feedback and draft comments)

(defun git-appraise-store--encode-comments (comments)
  "Encode the LOCAL-COMMENTS entries as JSON text; nil is an empty array."
  (if comments (json-encode comments) "[]"))

(defun git-appraise-store--decode-comments (text)
  "Decode stored LOCAL-COMMENTS JSON text into a list of entries."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-false :json-false))
    (json-read-from-string text)))

(defun git-appraise-store-pr-state (repo revision actor)
  "The latest snapshot for ACTOR at REPOSITORY REVISION, or nil.
Latest is the highest record version, then the newest `at', then
the largest id: the same precedence the notes reader applied to
review-revision records over legacy ones."
  (if-let* ((repo-key (git-appraise-store-repo-key repo)))
      (pcase-let* ((`(,sql . ,params)
                    (git-appraise-sql-select
                     :from 'pr_state
                     :columns '(v id feedback local_comments at)
                     :where `(:and (= repo ,repo-key)
                                   (= revision ,revision)
                                   (= actor ,actor))
                     :order-by '((v desc) (at desc) (id desc))
                     :limit 1))
                   (row (car (sqlite-select (git-appraise-store-db)
                                            sql params))))
        (when row
          (pcase-let ((`(,v ,id ,feedback ,comments ,at) row))
            `((v . ,v)
              (id . ,id)
              (feedback . ,feedback)
              (local_comments . ,(git-appraise-store--decode-comments comments))
              (at . ,at)))))
    nil))

(defun git-appraise-store-save-pr-state (repo revision actor snapshot)
  "Append SNAPSHOT for ACTOR at REPOSITORY REVISION, atomically.
The snapshot carries (v id feedback local_comments at): `feedback'
a string or nil, `local_comments' a list of raw comment entries.
Any failure signals, leaving the store unchanged.  Return
SNAPSHOT."
  (let* ((repo-key (or (git-appraise-store-repo-key repo)
                       (error "No review repository to save the draft in")))
         (db (git-appraise-store-db))
         (feedback (a-get snapshot 'feedback))
         (row (progn
                (unless (stringp revision)
                  (error "Snapshot needs a review revision"))
                (list repo-key
                      revision
                      (or actor (error "Snapshot without actor"))
                      (or (a-get snapshot 'v) (error "Snapshot without version"))
                      (or (a-get snapshot 'id) (error "Snapshot without id"))
                      (and (stringp feedback) (not (string-empty-p feedback))
                           feedback)
                      (git-appraise-store--encode-comments
                       (a-get snapshot 'local_comments))
                      (or (a-get snapshot 'at) (error "Snapshot without at"))))))
    (git-appraise-store--call-in-write
     (lambda ()
       (pcase-let ((`(,sql . ,params)
                    (git-appraise-sql-insert
                     :into 'pr_state
                     :columns '(repo revision actor v id feedback local_comments at)
                     :rows (list row)
                     :on-conflict :ignore)))
         (sqlite-execute db sql params))))
    snapshot))

(provide 'git-appraise-store)
;;; git-appraise-store.el ends here
