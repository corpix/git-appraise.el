;;; git-appraise-store-schema.el --- Review store schema -*- lexical-binding: t; -*-

;; Version 4 starts the EmacSQL store.  Older native-sqlite databases
;; have a different value representation and must be reset by the user.

;;; Code:

(require 'emacsql)

(defconst git-appraise-store-schema-version 4)

(defconst git-appraise-store--tables
  '(viewed-events fold-state pr-state viewed-snapshots)
  "Tables containing personal review state.")

(defun git-appraise-store--user-version (db)
  "Return DB's SQLite schema version."
  (caar (emacsql db [:pragma user-version])))

(defun git-appraise-store-schema-create (db)
  "Create the current schema in an empty DB.
Reject older or unrecognized databases without modifying them."
  (let ((version (git-appraise-store--user-version db)))
    (cond
     ((= version git-appraise-store-schema-version) nil)
     ((or (/= version 0)
          (not (zerop (caar (emacsql db
                                     [:select [(funcall count *)]
                                      :from sqlite-master])))))
      (error "Incompatible review store; reset git-appraise-store-file before use"))
     (t
      (let (committed)
        (unwind-protect
            (progn
              (emacsql db [:begin-immediate])
              (emacsql db
                       [:create-table viewed-events
                        ([(repo object :not-null)
                          (revision object :not-null)
                          (id object :not-null)
                          (actor object :not-null)
                          (item object :not-null)
                          (path object :not-null)
                          (fingerprint object :not-null)
                          (range object)
                          (viewed integer :not-null)
                          (at-us integer :not-null)]
                         (:primary-key [repo revision id]))])
              (emacsql db
                       [:create-index viewed-events-by-actor
                        :on viewed-events [repo revision actor at-us id]])
              (emacsql db
                       [:create-table fold-state
                        ([(repo object :not-null)
                          (revision object :not-null)
                          (actor object :not-null)
                          (identity object :not-null)
                          (id object :not-null)
                          (item object :not-null)
                          (path object :not-null)
                          (fingerprint object :not-null)
                          (range object)
                          (folded integer :not-null)
                          (at-us integer :not-null)]
                         (:primary-key [repo revision actor identity]))])
              (emacsql db
                       [:create-table pr-state
                        ([(repo object :not-null)
                          (revision object :not-null)
                          (actor object :not-null)
                          (v integer :not-null)
                          (id object :not-null)
                          (feedback object)
                          (local-comments object :not-null)
                          (at-us integer :not-null)]
                         (:primary-key [repo revision actor]))])
              (emacsql db
                       [:create-table viewed-snapshots
                        ([(repo object :not-null)
                          (revision object :not-null)
                          (actor object :not-null)
                          (path object :not-null)
                          (diff object :not-null)
                          (at-us integer :not-null)]
                         (:primary-key [repo revision actor path]))])
              (emacsql db [:pragma (= user-version $s1)]
                       git-appraise-store-schema-version)
              (emacsql db [:commit])
              (setq committed t))
          (unless committed
            (ignore-errors (emacsql db [:rollback])))))))))

(provide 'git-appraise-store-schema)
;;; git-appraise-store-schema.el ends here
