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
(require 'git-appraise-db)
(require 'git-appraise-utils)

;;; general comments - top level comments

(defclass git-appraise-comment-header-section (magit-section)
  ((keymap :initform 'git-appraise-comment-header-section-map)))
(defvar git-appraise-comment-header-section-map (make-sparse-keymap)
  "Keymap for the Conversation heading.")
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
(defun git-appraise-comment--created-at-seconds (c)
  "Return C's createdAt as seconds, or 0."
  (let ((ts (or (a-get c 'createdAt)
                (a-get c 'submittedAt)
                (a-get c 'updatedAt))))
    (if (and ts (stringp ts))
        (condition-case nil (time-to-seconds (date-to-time ts)) (error 0))
      0)))

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
  "Insert a compact diff link for COMMENT.
Omit author when it matches GROUP-AUTHOR."
  (let* ((author (or (a-get-in comment (list 'author 'login))
                     (a-get comment 'author) "unknown"))
         (path (a-get comment 'path))
         (line (or (a-get comment 'line) (a-get comment 'originalLine)
                   (a-get comment 'position)))
         (created (a-get comment 'createdAt))
         (id (a-get comment 'databaseId))
         (internal-id (a-get comment 'internal-id))
         (who (unless (and group-author (string= author group-author))
                author))
         (label (format "↳ diff%s%s%s · %s%s"
                        (if who (format " %s on" who) "")
                        (if path (format " %s" path) " comment")
                        (if line (format ":%s" line) "")
                        (or (git-appraise-comment-format-time created) "")
                        (if-let* ((state (a-get comment 'state)))
                            (if (equal state "COMMENTED") ""
                              (format " · %s" state))
                          ""))))
    (magit-insert-section (git-appraise-conversation-link id)
      (insert "  ")
      (insert-text-button
       label
       'action (lambda (_) (git-appraise-comment--goto-diff-comment id internal-id))
       'help-echo "Jump to comment in diff"
       'face 'git-appraise-dimmed-face
       'follow-link t)
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
  (let ((author (or (a-get-in review (list 'author 'login)) "unknown"))
        (state (or (a-get review 'state) "REVIEW")))
    (format "Review: %s · %s" author (git-appraise--propertize-keyword state))))

(defun git-appraise-comment--review-group-time (review)
  "Return representative time for REVIEW group."
  (or (a-get review 'submittedAt)
      (a-get review 'createdAt)
      (let ((nodes (a-get-in review '(comments nodes))))
        (when nodes
          (a-get (car (last (--sort (< (git-appraise-comment--created-at-seconds it)
                                       (git-appraise-comment--created-at-seconds other))
                                    nodes)))
                 'createdAt)))))

(defun git-appraise-comment--insert-conversation-group (label time-str comments &optional group-author)
  "Insert a foldable conversation group LABEL/TIME-STR with COMMENTS.
GROUP-AUTHOR suppresses redundant author in inline diff links."
  (magit-insert-section (git-appraise-conversation-group label t)
    (insert (propertize
             (format "%s (%d)%s"
                     label
                     (length comments)
                     (if time-str (concat " · " time-str) ""))
             'font-lock-face 'magit-section-secondary-heading))
    (magit-insert-heading)
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
                     :id (a-get node 'databaseId)))
               (time (git-appraise-comment-format-time (a-get node 'createdAt))))
          (magit-insert-section (git-appraise-comment-section obj)
            (insert (propertize "Summary" 'face 'magit-section-heading))
            (when time
              (insert " · " (propertize time 'face 'git-appraise-timestamp-face)))
            (magit-insert-heading)
            (insert ?\n)
            (git-appraise-insert-comment-lines obj)
            (insert ?\n))))
       (t
        (let* ((node (plist-get c :node))
               (obj (git-appraise-comment-section
                     :author (or (a-get-in node (list 'author 'login)) "unknown")
                     :msg (git-appraise-comment--node-body node)
                     :id (a-get node 'databaseId))))
          (magit-insert-section (git-appraise-comment-section obj)
            (insert (git-appraise-comment-format-heading
                     (if (a-get node 'parentHash) "Reply" "Comment")
                     (oref obj author) (a-get node 'createdAt)
                     (unless (equal (a-get node 'state) "COMMENTED")
                       (list (a-get node 'state)))))
            (magit-insert-heading)
            (insert ?\n)
            (git-appraise-insert-comment-lines obj)
            (insert ?\n))))))
    (insert ?\n)))

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
            (push (list :label "General comments"
                        :time (a-get (plist-get (car (last sorted-items)) :node)
                                     'createdAt)
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
            (push (list :label (git-appraise-comment--review-group-label review)
                        :author author
                        :time (git-appraise-comment--review-group-time review)
                        :items (nreverse items))
                  groups)))))
    ;; Sort groups and items by date
    (let ((sorted (--sort (< (git-appraise-comment--created-at-seconds
                              `((createdAt . ,(or (plist-get it :time) ""))))
                             (git-appraise-comment--created-at-seconds
                              `((createdAt . ,(or (plist-get other :time) "")))))
                          groups)))
      (--map (plist-put (copy-sequence it) :items
                        (--sort (< (plist-get it :time) (plist-get other :time))
                                (plist-get it :items)))
             sorted))))

(cl-defmethod git-appraise--insert-conversation-section ((appraise git-appraise-backend-repo))
  "Insert conversation section for APPRAISE reviews."
  (let ((groups (git-appraise-comment--collect-conversation-groups appraise)))
    (if (not groups)
        (progn
          (git-appraise-section--insert-dimmed "No conversation found.")
          (insert ?\n))
      (dolist (g groups)
        (git-appraise-comment--insert-conversation-group
         (plist-get g :label)
         (git-appraise-comment-format-time (plist-get g :time))
         (plist-get g :items)
         (plist-get g :author))))))

(defun git-appraise-section-insert-top-level-comments ()
  "Insert general comments for the PULL-REQUEST in the buffer."
  (when-let (pr (and (not (git-appraise-db-local-pr-p))
                     (git-appraise-db-get-pullreq)))
    (let* ((groups (git-appraise-comment--collect-conversation-groups pr))
           (total (--reduce-from (+ acc (length (plist-get it :items))) 0 groups))
           (last-time (--reduce-from
                       (let ((tstr (plist-get it :time)))
                         (if (and tstr
                                  (or (null acc)
                                      (> (git-appraise-comment--created-at-seconds
                                          `((createdAt . ,tstr)))
                                         (git-appraise-comment--created-at-seconds
                                          `((createdAt . ,acc))))))
                             tstr acc))
                       nil groups))
           (last-time-str (and last-time (git-appraise-comment-format-time last-time))))
      (git-appraise-section--hide-if-hidden
       (magit-insert-section (git-appraise-comment-header-section
                             nil git-appraise-fold-header-sections)
         (insert (propertize
                  (format "Conversation (%d)%s" total
                          (if last-time-str (concat " · last " last-time-str) ""))
                  'font-lock-face 'magit-section-heading))
         (magit-insert-heading)
         (when git-appraise-section--display-top-level-comments
           (git-appraise--insert-conversation-section pr)))))))

;; -


(provide 'git-appraise-section-conversation)
;;; git-appraise-section-conversation.el ends here
