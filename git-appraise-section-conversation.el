;;; git-appraise-section-conversation.el --- Review conversation sections -*- lexical-binding: t; -*-

;;; Commentary:
;; Conversation headings, timeline projection, and native thread labels.

;;; Code:

(require 'git-appraise-section-shared)
(require 'magit-section)
(require 'cl-lib)
(require 'dash)
(require 'a)
(require 'git-appraise-faces)
(require 'git-appraise-comment-format)
(require 'git-appraise-badges)
(require 'git-appraise-db)
(require 'git-appraise-utils)
(require 'git-appraise-fold-state)
(declare-function git-appraise-comment-add-or-edit "git-appraise-comment")
(declare-function git-appraise-section--insert-edit-history
                  "git-appraise-section-comment" (obj))

;;; general comments - top level comments

(defclass git-appraise-comment-header-section (magit-section)
  ((keymap :initform 'git-appraise-comment-header-section-map)))
(defvar git-appraise-comment-header-section-map (make-sparse-keymap)
  "Keymap for the Discussion heading.")
(defclass git-appraise-comment-section (magit-section)
  ((keymap :initform 'git-appraise-comment-section-map)
   (author :initarg :author
           :type string)
   (msg    :initarg :msg
           :type string)
   (body   :initarg :body
           :initform nil
           :type (or null string)
           :documentation "Raw (markdown) body as written by the author.
Used when editing a submitted comment.")
   (id     :initarg :id)
   (edited? :initarg :edited? :initform nil)
   (edit-history :initarg :edit-history :initform nil)
   (reactions :initarg :reactions)
   (typename :initarg :typename)
   (face   :initform 'magit-log-author)))
(defvar git-appraise-comment-section-map
  (make-sparse-keymap)
  "Keymaps for comment section.")
(defun git-appraise-comment-open-at-point ()
  "Reveal the conversation at point and move into its content."
  (interactive)
  (let ((section (magit-current-section)))
    (unless (or (cl-typep section 'git-appraise-comment-header-section)
                (cl-typep section 'git-appraise-comment-section))
      (user-error "No conversation at point"))
    (magit-section-show section)
    (goto-char (oref section content))
    (when (cl-typep section 'git-appraise-comment-header-section)
      (when-let ((child (car (oref section children))))
        (magit-section-show child)
        (goto-char (oref child content))))))
(define-key git-appraise-comment-header-section-map (kbd "RET")
  #'git-appraise-comment-open-at-point)
(define-key git-appraise-comment-section-map (kbd "RET")
  #'git-appraise-comment-open-at-point)
(define-key git-appraise-comment-section-map (kbd "C-c C-a")
  #'git-appraise-comment-add-or-edit)
(defun git-appraise-comment--created-at-seconds (c)
  "Return C's createdAt as seconds, or 0."
  (let ((ts (or (a-get c 'createdAt)
                (a-get c 'submittedAt)
                (a-get c 'updatedAt))))
    (if (and ts (stringp ts))
        (condition-case nil (time-to-seconds (date-to-time ts)) (error 0))
      0)))

(defun git-appraise-comment--latest-time (times)
  "Return the latest valid ISO timestamp in TIMES, or nil."
  (--reduce-from
   (if (or (null acc)
           (> (git-appraise-comment--created-at-seconds
               `((createdAt . ,it)))
              (git-appraise-comment--created-at-seconds
               `((createdAt . ,acc)))))
       it acc)
   nil (--filter it times)))

(defun git-appraise-comment--goto-diff-comment (id internal-id)
  "Jump to diff comment identified by ID/INTERNAL-ID."
  (let (target)
    (magit-map-sections
     (lambda (s)
       (when (and (not target)
                  (slot-boundp s 'value)
                  (let ((v (oref s value)))
                    (cond
                     ((eieio-object-p v)
                      (or (and id (slot-exists-p v 'id) (slot-boundp v 'id)
                               (equal (oref v id) id))
                          (and internal-id (slot-exists-p v 'internalId)
                               (slot-boundp v 'internalId)
                               (equal (oref v internalId) internal-id))))
                     ((listp v)
                      (ignore-errors
                        (or (and id (equal (a-get v 'databaseId) id))
                            (and internal-id
                                 (equal (a-get v 'internal-id) internal-id))))))))
         (setq target s)))
     magit-root-section)
    (unless target (user-error "Comment not found in diff"))
    (magit-section-show target)
    (goto-char (oref target start))))

(defun git-appraise-comment--insert-link (comment &optional group-author)
  "Insert COMMENT's body and a link back to its diff location.
Omit author when it matches GROUP-AUTHOR."
  (let* ((author (or (a-get-in comment (list 'author 'login))
                     (a-get comment 'author) "unknown"))
         (path (a-get comment 'path))
         (line (or (a-get comment 'line) (a-get comment 'originalLine)
                   (a-get comment 'position)))
         (id (a-get comment 'databaseId))
         (internal-id (a-get comment 'internal-id))
         (who (unless (and group-author (string= author group-author))
                author))
         (label (format "↳ %s%s%s"
                        (if who (concat who " · ") "")
                        (or path "comment")
                        (if line (format ":%s" line) "")))
         (status (a-get comment 'state))
         (badge (if (and (a-get comment 'parentHash)
                         (equal status "APPROVED"))
                    "RESOLVED"
                  (if (equal status "COMMENTED") "COMMENT" status))))
    (magit-insert-section (git-appraise-conversation-link id)
      (insert "  " (git-appraise-badge badge) " ")
      (insert-text-button
       label
       'action (lambda (_) (git-appraise-comment--goto-diff-comment id internal-id))
       'help-echo "Jump to comment in diff"
       'face 'git-appraise-dimmed-face
       'follow-link t)
      (magit-insert-heading)
      (insert (git-appraise-comment-format-time-details
               (a-get comment 'createdAt) (a-get comment 'updatedAt)))
      (insert ?\n)
      (let ((obj (git-appraise-comment-section
                  :author author :msg (git-appraise-comment--node-body comment)
                  :id id :edited? (a-get comment 'edited)
                  :edit-history (a-get comment 'editHistory))))
        (git-appraise-insert-comment-lines obj)
        (git-appraise-section--insert-edit-history obj))
      (insert ?\n))))

(defun git-appraise-comment--inline-diff-comment-p (comment)
  "Non-nil when COMMENT is an inline diff comment rendered in files."
  (and (a-get comment 'path)
       (or (a-get comment 'position)
           (a-get comment 'originalPosition)
           (a-get comment 'line)
           (a-get comment 'startLine))))

(defun git-appraise-comment--node-body (node)
  "Return display body HTML/text for NODE."
  (or (a-get node 'bodyHTML)
      (a-get node 'bodyText)
      (a-get node 'body)
      ""))

(defun git-appraise-comment--review-group-label (review)
  "Return a label for REVIEW group."
  (or (a-get-in review (list 'author 'login)) "unknown"))

(defun git-appraise-comment--review-group-time (review)
  "Return representative time for REVIEW group."
  (git-appraise-comment--latest-time
   (append (list (a-get review 'submittedAt)
                 (a-get review 'createdAt)
                 (a-get review 'updatedAt))
           (--map (or (a-get it 'updatedAt)
                      (a-get it 'createdAt))
                  (a-get-in review '(comments nodes))))))

(defun git-appraise-comment--insert-conversation-group (key label time comments
                                                         &optional group-author status)
  "Insert a foldable conversation group LABEL/TIME with COMMENTS.
GROUP-AUTHOR suppresses redundant author in inline diff links."
  (let ((folded (git-appraise-fold-state--choice "conversation" key key)))
    (git-appraise-section--hide-if-hidden
     (magit-insert-section (git-appraise-conversation-group key
                           (if (eq folded 'unset) t folded))
    (insert (propertize label 'font-lock-face 'magit-section-secondary-heading))
    (when status
      (insert " " (git-appraise-badge status)))
    (insert (format " (%d)" (length comments)))
    (magit-insert-heading)
    (insert (or (git-appraise-comment-format-time-detail
                 "Latest activity" time) ""))
    (dolist (c comments)
      (cond
       ((plist-get c :link)
        (git-appraise-comment--insert-link (plist-get c :node) group-author))
       ((plist-get c :summary)
        (let* ((node (plist-get c :node))
               (obj (git-appraise-comment-section
                     :author (or (a-get-in node (list 'author 'login))
                                 group-author "unknown")
                     :msg (git-appraise-comment--node-body node)
                     :id (a-get node 'databaseId)
                     :edited? (a-get node 'edited)
                     :edit-history (a-get node 'editHistory)))
               (time (a-get node 'createdAt)))
          (magit-insert-section (git-appraise-comment-section obj)
            (insert (propertize "Note" 'face 'magit-section-heading))
            (magit-insert-heading)
            (insert (git-appraise-comment-format-time-details
                     time (a-get node 'updatedAt)))
            (insert ?\n)
            (git-appraise-insert-comment-lines obj)
            (git-appraise-section--insert-edit-history obj)
            (insert ?\n))))
       (t
        (let* ((node (plist-get c :node))
               (obj (git-appraise-comment-section
                     :author (or (a-get-in node (list 'author 'login)) "unknown")
                     :msg (git-appraise-comment--node-body node)
                     :id (a-get node 'databaseId)
                     :edited? (a-get node 'edited)
                     :edit-history (a-get node 'editHistory))))
          (magit-insert-section (git-appraise-comment-section obj)
            (insert (git-appraise-comment-format-heading
                     (if (a-get node 'parentHash) "Reply" "Comment")
                     (oref obj author) (a-get node 'createdAt)
                     (append (unless (equal (a-get node 'state) "COMMENTED")
                               (list (a-get node 'state)))
                             (when (a-get node 'edited) '("EDITED")))))
            (magit-insert-heading)
            (insert (git-appraise-comment-format-time-details
                     (a-get node 'createdAt) (a-get node 'updatedAt)))
            (insert ?\n)
            (git-appraise-insert-comment-lines obj)
            (git-appraise-section--insert-edit-history obj)
            (insert ?\n))))))
       (insert ?\n)))))

(defun git-appraise-comment--collect-conversation-groups (pr)
  "Collect foldable conversation groups from PR raw infos/comments."
  (let* ((raw-infos (oref pr raw-infos))
         (top-level (or (a-get-in raw-infos '(comments nodes)) '()))
         (reviews (or (git-appraise-db--pullreq-raw-comments) '()))
         (seen (make-hash-table :test #'equal))
         groups)
    (cl-labels ((seen? (n)
                  (let ((k (or (a-get n 'databaseId) (a-get n 'internal-id))))
                    (and k (gethash k seen))))
                (mark-seen (n)
                  (let ((k (or (a-get n 'databaseId) (a-get n 'internal-id))))
                    (when k (puthash k t seen)))))
      ;; Top-level/stray and earlier historical comments, folded into timeline by date.
      (let* ((historical (or (a-get-in raw-infos '(historicalComments nodes)) '()))
             (items (--keep (unless (seen? it)
                              (mark-seen it)
                              (list :node it :link nil
                                    :time (git-appraise-comment--created-at-seconds it)))
                            top-level))
             (hist-items (--keep (unless (seen? it)
                                   (mark-seen it)
                                   (list :node it :link t
                                         :author (a-get-in it (list 'author 'login))
                                         :time (git-appraise-comment--created-at-seconds it)))
                                 historical))
             (all-items (append items hist-items)))
        (when all-items
          (let ((sorted-items (--sort (< (plist-get it :time) (plist-get other :time))
                                      all-items)))
            (push (list :key "general" :label "General"
                        :time (git-appraise-comment--latest-time
                               (--map (or (a-get (plist-get it :node)
                                                 'updatedAt)
                                          (a-get (plist-get it :node)
                                                 'createdAt))
                                      sorted-items))
                        :items sorted-items)
                  groups))))
      ;; Review groups
      (dolist (review reviews)
        (let* ((nodes (or (a-get-in review '(comments nodes)) '()))
               (review-body (git-appraise-comment--node-body review))
               (author (or (a-get-in review (list 'author 'login)) "unknown"))
               (items '()))
          (when (and review-body (not (string-empty-p (string-trim review-body)))
                     (not (seen? review)))
            (mark-seen review)
            (push (list :node review :link nil :summary t
                        :author author
                        :time (git-appraise-comment--created-at-seconds review))
                  items))
          (dolist (n nodes)
            (unless (seen? n)
              (mark-seen n)
              (push (list :node n
                          :link (git-appraise-comment--inline-diff-comment-p n)
                          :author (or (a-get-in n (list 'author 'login)) author)
                          :time (git-appraise-comment--created-at-seconds n))
                    items)))
          (when items
            (push (list :key (format "review:%s:%s"
                                      author (or (a-get review 'submittedAt)
                                                 (a-get review 'createdAt)
                                                 (a-get review 'databaseId)
                                                 (a-get (car nodes) 'databaseId)
                                                 (a-get (car nodes) 'internal-id)
                                                 "unknown"))
                        :label (git-appraise-comment--review-group-label review)
                        :status (a-get review 'state)
                        :author author
                        :time (git-appraise-comment--review-group-time review)
                        :items (nreverse items))
                  groups)))))
    (git-appraise-comment--sort-groups groups)))

(defun git-appraise-comment--sort-groups (groups)
  "Sort GROUPS and their items by activity time."
  (let ((sorted (--sort (< (git-appraise-comment--created-at-seconds
                            `((createdAt . ,(or (plist-get it :time) ""))))
                           (git-appraise-comment--created-at-seconds
                            `((createdAt . ,(or (plist-get other :time) "")))))
                        groups)))
    (--map (plist-put (copy-sequence it) :items
                      (--sort (< (plist-get it :time) (plist-get other :time))
                              (plist-get it :items)))
           sorted)))

(defun git-appraise-comment--insert-groups (groups)
  "Insert already collected discussion GROUPS."
  (if (not groups)
      (progn
        (git-appraise-section--insert-dimmed "No comments yet.")
        (insert ?\n))
    (dolist (g groups)
      (git-appraise-comment--insert-conversation-group
       (plist-get g :key)
       (plist-get g :label)
       (plist-get g :time)
       (plist-get g :items)
       (plist-get g :author)
       (plist-get g :status)))))

(defun git-appraise-section-insert-top-level-comments ()
  "Insert general comments for the PULL-REQUEST in the buffer."
  (when-let (pr (and (not (git-appraise-db-local-pr-p))
                     (git-appraise-db-get-pullreq)))
    (let* ((groups (git-appraise-comment--collect-conversation-groups pr))
           (total (--reduce-from (+ acc (length (plist-get it :items))) 0 groups))
           (last-time (git-appraise-comment--latest-time
                       (--map (plist-get it :time) groups))))
      (git-appraise-section--hide-if-hidden
       (magit-insert-section (git-appraise-comment-header-section
                             nil git-appraise-fold-header-sections)
         (insert (propertize
                  (format "Discussion (%d)" total)
                  'font-lock-face 'magit-section-heading))
         (magit-insert-heading)
         (when git-appraise-section--display-top-level-comments
           (when last-time
             (insert (or (git-appraise-comment-format-time-detail
                          "Latest activity" last-time) "")))
           (git-appraise-comment--insert-groups groups)))))))

;; -


(provide 'git-appraise-section-conversation)
;;; git-appraise-section-conversation.el ends here
