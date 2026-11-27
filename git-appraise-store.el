;;; git-appraise-store.el --- Durable local review state (SQLite) -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal, durable review state -- viewed marks, fold choices and
;; unsent feedback/drafts -- in one EmacSQL SQLite database.
;;
;; This file owns the connection and the row mapping and nothing
;; else: callers pass repo/revision and receive the same event
;; alists the projection reducers have always consumed.  SQL is
;; built with EmacSQL forms.  A repository's identity is its
;; canonical common git dir, so linked worktrees share state.

;;; Code:

(require 'a)
(require 'dash)
(require 'json)
(require 'subr-x)
(require 'emacsql)
(require 'emacsql-sqlite)

(require 'git-appraise-store-schema)
(require 'git-appraise-time)
(require 'git-appraise-utils)

(defcustom git-appraise-store-file
  (expand-file-name "git-appraise/state.sqlite" user-emacs-directory)
  "SQLite database file holding personal review state.
Created on first use; the parent directory is created too."
  :group 'git-appraise
  :type 'file)

(defconst git-appraise-store--max-batch-values 900
  "Maximum values in a batched EmacSQL insert statement.")

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
  "The open store connection; opened and checked on first use."
  (or git-appraise-store--db
      (setq git-appraise-store--db (git-appraise-store--open))))

(defun git-appraise-store--open ()
  (unless (git-appraise-store-available-p)
    (error "git-appraise needs an Emacs built with SQLite support"))
  (let* ((file (and git-appraise-store-file
                    (expand-file-name git-appraise-store-file)))
         (db nil)
         (ready nil))
    (when file
      (make-directory (or (file-name-directory file) ".") t))
    (unwind-protect
        (progn
          (setq db (emacsql-sqlite-open file))
          (git-appraise-store-schema-create db)
          (when file
            (emacsql db [:pragma (= journal-mode wal)]))
          (setq ready t)
          db)
      (when (and db (not ready))
        (emacsql-close db)))))

(defun git-appraise-store-close ()
  "Close the store connection, if one is open."
  (when git-appraise-store--db
    (ignore-errors (emacsql-close git-appraise-store--db))
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
          (emacsql db [:begin-immediate])
          (let ((result (funcall thunk)))
            (emacsql db [:commit])
            (setq committed t)
            result))
      (unless committed
        (ignore-errors (emacsql db [:rollback]))))))

(defun git-appraise-store--contains-repo-p (db table repo-key)
  "Non-nil when TABLE contains a row for REPO-KEY."
  (unless (memq table git-appraise-store--tables)
    (error "Unknown table: %S" table))
  (car (emacsql db [:select [1] :from $i1
                    :where (= repo $s2) :limit 1]
                table repo-key)))

(defun git-appraise-store-contains-repo-p (repo)
  "Non-nil when the store already holds any state for REPO."
  (if-let* ((repo-key (git-appraise-store-repo-key repo)))
      (let ((db (git-appraise-store-db)))
        (--any? (git-appraise-store--contains-repo-p db it repo-key)
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
    (`viewed 'viewed-events)
    (`fold 'fold-state)
    (_ (error "Unknown mark kind: %S" kind))))

(defun git-appraise-store--flag (kind)
  "The boolean flag column (and alist key) of KIND."
  (if (eq kind 'viewed) 'viewed 'folded))

(defun git-appraise-store--event-columns (kind)
  "The event columns an insert names, in row order."
  (append '(repo revision actor)
          (when (eq kind 'fold) '(identity))
          `(id item path fingerprint range
               ,(git-appraise-store--flag kind) at-us)))

(defun git-appraise-store--event-select-columns (kind)
  "The event columns a read returns, in row order."
  `(id actor item path fingerprint range
       ,(git-appraise-store--flag kind) at-us))

(defun git-appraise-store--fold-identity (event)
  "Return the stable row identity for fold EVENT."
  (secure-hash 'sha256
               (prin1-to-string
                (list (a-get event 'item) (a-get event 'path)
                      (a-get event 'fingerprint) (a-get event 'range)))))

(defun git-appraise-store--event-row (kind event repo-key revision)
  "The full insert row for event alist EVENT of KIND."
  (vconcat
   (--map (if (stringp it) (substring-no-properties it) it)
          (append
           (list repo-key revision
                 (or (a-get event 'actor) (error "Event without actor")))
           (when (eq kind 'fold)
             (list (git-appraise-store--fold-identity event)))
           (list (or (a-get event 'id) (error "Event without id"))
                 (or (a-get event 'item) (error "Event without item"))
                 (or (a-get event 'path) (error "Event without path"))
                 (or (a-get event 'fingerprint)
                     (error "Event without fingerprint"))
                 (a-get event 'range)
                 (let ((flag (a-get event (git-appraise-store--flag kind))))
                   (unless (memq flag '(t nil))
                     (error "Bad %s flag in event %s"
                            (git-appraise-store--flag kind)
                            (a-get event 'id)))
                   (if flag 1 0))
                 (or (git-appraise-time-parse-utc (a-get event 'at))
                     (error "Bad timestamp in event %s"
                            (a-get event 'id))))))))

(defun git-appraise-store--row->event (kind row)
  "The event alist for a stored KIND row ROW."
  (pcase-let ((`(,id ,actor ,item ,path ,fingerprint ,range ,flag ,at-us) row))
    `((id . ,id)
      (actor . ,actor)
      (item . ,item)
      (path . ,path)
      (fingerprint . ,fingerprint)
      ,@(when range `((range . ,range)))
      (,(git-appraise-store--flag kind) . ,(if (and flag (= flag 1)) t nil))
      (at . ,(git-appraise-time-from-epoch-us at-us)))))

(defun git-appraise-store-events (repo revision kind &optional actor)
  "Stored events of KIND for REPOSITORY REVISION, oldest first.
When ACTOR is non-nil, return only that reviewer's events.
KIND is `viewed' or `fold'.  The event alists are exactly what the
projection reducers consume; a repository without state, or a nil
REPO, has no events."
  (if-let* ((repo-key (git-appraise-store-repo-key repo)))
      (let* ((db (git-appraise-store-db))
             (cols (vconcat (git-appraise-store--event-select-columns kind)))
             (table (git-appraise-store--table kind))
             (rows (if actor
                       (emacsql db
                                [:select $i1 :from $i2
                                 :where (and (= repo $s3)
                                             (= revision $s4)
                                             (= actor $s5))
                                 :order-by [(asc at-us) (asc id)]]
                                cols table repo-key revision actor)
                     (emacsql db
                              [:select $i1 :from $i2
                               :where (and (= repo $s3) (= revision $s4))
                               :order-by [(asc at-us) (asc id)]]
                              cols table repo-key revision))))
        (--map (git-appraise-store--row->event kind it) rows))
    nil))

(defun git-appraise-store--insert-chunked (db table columns rows)
  "Insert ROWS into TABLE, several statements' worth if needed.
Rows are split to cap compiled SQL statement size at roughly
`git-appraise-store--max-batch-values' values; the caller owns the
transaction, so a split batch still commits whole or not at all."
  (let ((per (max 1 (/ git-appraise-store--max-batch-values
                       (length columns)))))
    (--each (-partition-all per rows)
      (if (eq table 'viewed-events)
          (emacsql db
                   [:insert-or-ignore-into $i1 [$i2] :values $v3]
                   table (vconcat columns) it)
        (emacsql db
                 [:insert-or-replace-into $i1 [$i2] :values $v3]
                 table (vconcat columns) it)))))

(defun git-appraise-store--insert-viewed-snapshot (db key revision actor path diff)
  "Insert ACTOR's viewed DIFF for PATH using DB and repository KEY."
  (unless (stringp diff)
    (error "Viewed snapshot needs diff text"))
  (emacsql db
           [:insert-or-replace-into viewed-snapshots
            [repo revision actor path diff at-us] :values $v1]
           (vector key
                   (substring-no-properties revision)
                   (substring-no-properties actor)
                   (substring-no-properties path)
                   (substring-no-properties diff)
                   (git-appraise-time-parse-utc
                    (git-appraise-time-now-utc t)))))

(defun git-appraise-store-append-events (repo revision kind events &optional snapshots)
  "Store EVENTS of KIND for REPOSITORY REVISION in one transaction.
EVENTS are event alists as produced by the mark commands.  Any
failure signals, leaving the store unchanged; a batch with a
duplicate id is stored once.  SNAPSHOTS, when KIND is viewed, is a
list of (ACTOR PATH DIFF) committed atomically with the events.
Return EVENTS."
  (when (and snapshots (not (eq kind 'viewed)))
    (error "Only viewed events can carry diff snapshots"))
  (let* ((repo-key (or (git-appraise-store-repo-key repo)
                       (error "No review repository to store %s marks in" kind)))
         (rows (--map (git-appraise-store--event-row kind it repo-key revision)
                      events)))
    (when rows
      (git-appraise-store--call-in-write
       (lambda ()
         (git-appraise-store--insert-chunked
          git-appraise-store--db (git-appraise-store--table kind)
          (git-appraise-store--event-columns kind) rows)
         (--each snapshots
           (pcase-let ((`(,actor ,path ,diff) it))
             (git-appraise-store--insert-viewed-snapshot
              git-appraise-store--db repo-key revision actor path diff))))))
    events))

;;; Unsent review state (feedback and draft comments)

(defun git-appraise-store-viewed-snapshot (repo revision actor path)
  "Return the saved diff snapshot for ACTOR's PATH in REPO REVISION."
  (when-let* ((key (git-appraise-store-repo-key repo)))
    (caar (emacsql (git-appraise-store-db)
                   [:select [diff] :from viewed-snapshots
                    :where (and (= repo $s1) (= revision $s2)
                                (= actor $s3) (= path $s4))]
                   key revision actor path))))

(defun git-appraise-store-viewed-snapshots (repo revision actor)
  "Return ACTOR's viewed diff snapshots for REPO REVISION by path."
  (let ((table (make-hash-table :test #'equal)))
    (when-let* ((key (git-appraise-store-repo-key repo)))
      (--each (emacsql (git-appraise-store-db)
                       [:select [path diff] :from viewed-snapshots
                        :where (and (= repo $s1) (= revision $s2)
                                    (= actor $s3))]
                       key revision actor)
        (puthash (car it) (cadr it) table)))
    table))

(defun git-appraise-store-save-viewed-snapshot (repo revision actor path diff)
  "Save DIFF as ACTOR's latest fully viewed PATH in REPO REVISION."
  (let ((key (or (git-appraise-store-repo-key repo)
                 (error "No repository for viewed snapshot"))))
    (git-appraise-store--call-in-write
     (lambda ()
       (git-appraise-store--insert-viewed-snapshot
        git-appraise-store--db key revision actor path diff)))))

(defun git-appraise-store--encode-comments (comments)
  "Encode the LOCAL-COMMENTS entries as JSON text; nil is an empty array."
  (if comments (substring-no-properties (json-encode comments)) "[]"))

(defun git-appraise-store--decode-comments (text)
  "Decode stored LOCAL-COMMENTS JSON text into a list of entries."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-false :json-false))
    (json-read-from-string text)))

(defun git-appraise-store-pr-state (repo revision actor)
  "The current draft for ACTOR at REPOSITORY REVISION, or nil."
  (if-let* ((repo-key (git-appraise-store-repo-key repo)))
      (let ((row (car (emacsql (git-appraise-store-db)
                               [:select [v id feedback local-comments at-us]
                                :from pr-state
                                :where (and (= repo $s1) (= revision $s2)
                                            (= actor $s3))]
                               repo-key revision actor))))
        (when row
          (pcase-let ((`(,v ,id ,feedback ,comments ,at-us) row))
            `((v . ,v)
              (id . ,id)
              (feedback . ,feedback)
              (local_comments . ,(git-appraise-store--decode-comments comments))
              (at . ,(git-appraise-time-from-epoch-us at-us))))))
    nil))

(defun git-appraise-store-save-pr-state (repo revision actor snapshot)
  "Replace ACTOR's draft at REPOSITORY REVISION with SNAPSHOT atomically.
The snapshot carries (v id feedback local_comments at): `feedback'
a string or nil, `local_comments' a list of raw comment entries.
Any failure signals, leaving the store unchanged.  Return
SNAPSHOT."
  (let* ((repo-key (or (git-appraise-store-repo-key repo)
                       (error "No review repository to save the draft in")))
         (feedback (a-get snapshot 'feedback))
         (row (progn
                (unless (stringp revision)
                  (error "Snapshot needs a review revision"))
                (vector repo-key
                        (substring-no-properties revision)
                        (substring-no-properties
                         (or actor (error "Snapshot without actor")))
                        (or (a-get snapshot 'v) (error "Snapshot without version"))
                        (substring-no-properties
                         (or (a-get snapshot 'id)
                             (error "Snapshot without id")))
                        (and (stringp feedback) (not (string-empty-p feedback))
                             (substring-no-properties feedback))
                        (git-appraise-store--encode-comments
                         (a-get snapshot 'local_comments))
                        (or (git-appraise-time-parse-utc (a-get snapshot 'at))
                            (error "Bad timestamp in snapshot"))))))
    (git-appraise-store--call-in-write
     (lambda ()
       (emacsql (git-appraise-store-db)
                [:insert-or-replace-into pr-state
                 [repo revision actor v id feedback local-comments at-us]
                 :values $v1]
                row)))
    snapshot))

(provide 'git-appraise-store)
;;; git-appraise-store.el ends here
