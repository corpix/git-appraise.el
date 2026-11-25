;;; git-appraise-db.el --- In-memory review state -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski

;;; Commentary:
;;  In-memory review model.  No SQL database.

;;; Code:

(require 'a)
(require 'eieio)
(require 'uuidgen)
(require 'cl-lib)
(require 'dash)

(require 'git-appraise-pr-state)

(defgroup git-appraise nil
  "Git Appraise."
  :group 'tools)

(defvar git-appraise-db--pullreq-id nil
  "Id of the current pull request object.")

(defvar git-appraise-db--objects (make-hash-table :test 'equal)
  "In-memory store mapping object id to pullreq objects.")

(defclass git-appraise-db-review ()
  ((id            :initarg :id)
   (base-ref-name :initform nil)
   (head-ref-name :initform nil)
   (raw-infos     :initform nil)
   (raw-diff      :initform nil)
   (diff-tables   :initform nil)
   (raw-comments  :initform nil)
   (owner         :initarg :owner)
   (repo          :initarg :repo)
   (number        :initarg :number)
   (description   :initform nil)
   (title         :initform nil)
   (host          :initform nil)
   (sha           :initform nil)
   (feedback      :initform nil)
   (state         :initform nil)
   (url           :initarg :url)
   ;; lightweight in-memory path bookkeeping for comment positioning
   (paths         :initform nil))
  :abstract t)

(defclass git-appraise-db-path ()
  ((id       :initarg :id)
   (name     :initarg :name)
   (head-pos :initform nil)
   (at-pos-p :initarg :at-pos-p)))

(defun git-appraise-db-update (obj)
  "Store OBJ in the in-memory store."
  (puthash (oref obj id) obj git-appraise-db--objects)
  obj)

(defun git-appraise-db-get-pullreq ()
  "Get the current pullreq object."
  (gethash git-appraise-db--pullreq-id git-appraise-db--objects))

(defun git-appraise-db-local-pr-p ()
  "Return non-nil when the current review is a local diff."
  nil)

(defun git-appraise-db--pullreq-create (obj)
  "Register OBJ as the current pullreq and return it."
  (let ((pr-id (or (and (slot-boundp obj 'id)
                        (oref obj id))
                   (uuidgen-4))))
    (oset obj id pr-id)
    (git-appraise-db-update obj)
    (setq git-appraise-db--pullreq-id pr-id)
    (when (fboundp 'git-appraise-pr-state-hydrate)
      (git-appraise-pr-state-hydrate obj)
      (git-appraise-db-update obj))
    obj))

(defun git-appraise-db--pullreq-sha-update (sha-value)
  "Update current pullreq SHA to SHA-VALUE."
  (let ((pr (git-appraise-db-get-pullreq)))
    (oset pr sha sha-value)
    (git-appraise-db-update pr)))

(defun git-appraise-db--local-comment-entry-p (entry)
  "Return non-nil when raw-comments ENTRY holds unsubmitted local work."
  (or (equal (a-get entry 'state) "LOCAL COMMENT")
      (--any? (and (a-get it 'internal-id) (a-get it 'reply?))
              (a-get-in entry '(comments nodes)))))

(defun git-appraise-db--reconcile-raw-comments (new-reviews old-reviews)
  "Merge OLD-REVIEWS' local-comment entries into NEW-REVIEWS."
  (append new-reviews
          (-filter #'git-appraise-db--local-comment-entry-p old-reviews)))

(defun git-appraise-db--pullreq-raw-infos-update (infos)
  "Save INFOS to the current pullreq."
  (let ((pullreq (git-appraise-db-get-pullreq)))
    (let-alist infos
      (oset pullreq raw-infos infos)
      (oset pullreq title .title)
      (oset pullreq state .state)
      (oset pullreq base-ref-name .baseRefName)
      (oset pullreq head-ref-name .headRefName)
      (oset pullreq description .bodyHTML)
      (oset pullreq sha .headRefOid)
      (git-appraise-db-update pullreq))))

(defun git-appraise-db--pullreq-raw-diff-update (raw-diff)
  "Save RAW-DIFF to the current pullreq."
  (let ((pullreq (git-appraise-db-get-pullreq)))
    (oset pullreq raw-diff raw-diff)
    (oset pullreq diff-tables nil)
    (git-appraise-db-update pullreq)))

(defun git-appraise-db--upsert-local-comment (entry previous-id)
  "Store draft ENTRY, replacing PREVIOUS-ID with one persistent update.
Restore the in-memory comment list if the durable write fails."
  (let* ((pr (git-appraise-db-get-pullreq))
         (old (oref pr raw-comments))
         (new (if previous-id
                  (--remove
                   (--any? (equal (a-get it 'internal-id) previous-id)
                           (a-get-in it '(comments nodes)))
                   old)
                old)))
    (oset pr raw-comments (append new (list entry)))
    (condition-case err
        (when (fboundp 'git-appraise-pr-state-persist)
          (git-appraise-pr-state-persist pr t))
      (error
       (oset pr raw-comments old)
       (signal (car err) (cdr err))))
    (git-appraise-db-update pr)))

(defun git-appraise-db--pullreq-raw-infos ()
  "Get raw infos from current pullreq."
  (oref (git-appraise-db-get-pullreq) raw-infos))

(defun git-appraise-db--pullreq-raw-comments ()
  "Get raw comments from current pullreq."
  (oref (git-appraise-db-get-pullreq) raw-comments))

(defun git-appraise-db--pullreq-raw-diff ()
  "Get raw diff from current pullreq."
  (oref (git-appraise-db-get-pullreq) raw-diff))

(defun git-appraise-db--pullreq-title ()
  "Get title of current pullreq."
  (oref (git-appraise-db-get-pullreq) title))

(defun git-appraise-db--pullreq-state ()
  "Get state of current pullreq."
  (oref (git-appraise-db-get-pullreq) state))

(defun git-appraise-db--pullreq-feedback ()
  "Get feedback for current pullreq."
  (oref (git-appraise-db-get-pullreq) feedback))

(defun git-appraise-db--pullreq-feedback-update (feedback)
  "Save FEEDBACK for current pullreq."
  (let* ((pr (git-appraise-db-get-pullreq))
         (old (oref pr feedback)))
    (oset pr feedback feedback)
    (condition-case err
        (when (fboundp 'git-appraise-pr-state-persist)
          (git-appraise-pr-state-persist pr t))
      (error
       (oset pr feedback old)
       (signal (car err) (cdr err))))
    (git-appraise-db-update pr)))

(defun git-appraise-db--pullreq-raw-comments-update (comment)
  "Add COMMENT to the current pullreq raw comments."
  (let* ((pr (git-appraise-db-get-pullreq))
         (raw-comments (oref pr raw-comments)))
    (oset pr raw-comments (append raw-comments (list comment)))
    (condition-case err
        (when (fboundp 'git-appraise-pr-state-persist)
          (git-appraise-pr-state-persist pr t))
      (error
       (oset pr raw-comments raw-comments)
       (signal (car err) (cdr err))))
    (git-appraise-db-update pr)))

(defun git-appraise-db-delete-raw-comment (identifier)
  "Remove comment identified by IDENTIFIER from raw comments list.
IDENTIFIER can be our local `internal-id' (string UUID) or the provider
`databaseId' (number or string)."
  (let* ((pr (git-appraise-db-get-pullreq))
         (old (oref pr raw-comments))
         (id-str (cond ((numberp identifier) (number-to-string identifier))
                       ((stringp identifier) identifier)
                       (t (format "%s" identifier))))
         (new-comments
          (--keep
           (let ((nodes
                  (-remove
                   (lambda (node)
                     (--any? (and it (equal (format "%s" it) id-str))
                             (list (a-get node 'internal-id)
                                   (a-get node 'databaseId)
                                   (a-get node 'fullDatabaseId))))
                   (a-get-in it '(comments nodes)))))
             (when nodes (a-assoc-in it '(comments nodes) nodes)))
           old)))
    (oset pr raw-comments new-comments)
    (condition-case err
        (when (fboundp 'git-appraise-pr-state-persist)
          (git-appraise-pr-state-persist pr t))
      (error
       (oset pr raw-comments old)
       (signal (car err) (cdr err))))
    (git-appraise-db-update pr)))

;;; Path bookkeeping (in-memory, for diff wash comment positioning)

(defun git-appraise-db--curr-path-update (curr-path)
  "Set CURR-PATH as the active path in the current pullreq."
  (let ((pr (git-appraise-db-get-pullreq)))
    (dolist (p (oref pr paths))
      (oset p at-pos-p (string-equal (oref p name) curr-path)))
    (unless (--first (string-equal (oref it name) curr-path) (oref pr paths))
      (oset pr paths
            (append (oref pr paths)
                    (list (git-appraise-db-path :id (uuidgen-4)
                                               :name curr-path
                                               :at-pos-p t)))))))

(defun git-appraise-db--curr-path-head-pos-update (curr-path hunk-head-pos)
  "Set HUNK-HEAD-POS for CURR-PATH in the current pullreq."
  (let ((pr (git-appraise-db-get-pullreq)))
    (dolist (p (oref pr paths))
      (when (string-equal (oref p name) curr-path)
        (oset p head-pos hunk-head-pos)))))

(defun git-appraise-db--curr-path ()
  "Get the currently active path object."
  (let ((pr (git-appraise-db-get-pullreq)))
    (--first (oref it at-pos-p) (oref pr paths))))

(defun git-appraise-db-get-buffer ()
  "Compatibility stub; in-memory model has no buffer objects."
  nil)

(defun git-appraise-db-get-comment (_id)
  "Compatibility stub; in-memory model has no comment objects."
  nil)

(defun git-appraise-db--curr-comment ()
  "Compatibility stub."
  nil)

(defun git-appraise-db--comment-already-written? (_identifier)
  "Compatibility stub."
  nil)

(defun git-appraise-db-get-comment-written-pos ()
  "Compatibility stub."
  0)

(provide 'git-appraise-db)
;;; git-appraise-db.el ends here
