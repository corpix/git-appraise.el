;;; git-appraise-comment-draft.el --- Local review draft handling -*- lexical-binding: t; -*-

;;; Commentary:
;; Save draft comments and update their review sections in place.

;;; Code:

(require 'a)
(require 'cl-lib)
(require 'xml)
(require 'uuidgen)
(require 'git-appraise-section)
(require 'git-appraise-db)
(require 'git-appraise-time)
(require 'git-appraise-utils)

(declare-function git-appraise-comment-strip-known-helpers "git-appraise-comment")
(declare-function git-appraise-comment-add "git-appraise-comment")
(declare-function git-appraise-folding--mark-commented-hunk "git-appraise-folding"
                  (section))
(declare-function git-appraise-backend-refresh-comments "git-appraise-backend"
                  (obj))
(declare-function git-appraise-lazy--expand-at "git-appraise-lazy"
                  (section))
(declare-function git-appraise-lazy-placeholder-section-p "git-appraise-lazy"
                  (section))

(defun git-appraise-comment--hunk-ancestor (section)
  "The nearest magit hunk section at or above SECTION, or nil."
  (let ((s section))
    (while (and s (not (magit-hunk-section-p s)))
      (setq s (and (slot-boundp s 'parent) (oref s parent))))
    (and (magit-hunk-section-p s) s)))

(defun git-appraise-comment--absorb-in-hunk (hunk insert-beg insert-end)
  "Extend HUNK's body range over a comment inserted at its boundary.
`magit-insert-section--finish' fixes the hunk's `end' marker when
the wash finishes the section, so content appended at that
boundary lands outside [content, end): a folded hunk's visibility
overlay spans exactly that range and the comment would leak out of
the fold.  Move the section boundary instead of hiding the comment
text with a second ad hoc overlay; when the hunk is currently
folded, move the overlay with it and refresh the collapsed-comment
indicator.  Insertions strictly inside the hunk need nothing."
  (when (and hunk (slot-boundp hunk 'end) (oref hunk end)
             (>= insert-beg (oref hunk end))
             (> insert-end insert-beg))
    (set-marker (oref hunk end) insert-end)
    (when (oref hunk hidden)
      (let ((content (oref hunk content)))
        (dolist (ov (overlays-in content insert-end))
          (when (overlay-get ov 'invisible)
            (cond
             ;; Hunk's own invisibility overlay.
             ((= (overlay-start ov) content)
              (move-overlay ov content insert-end))
             ;; A parent file fold overlay: extend it so the appended
             ;; comment stays concealed.
             ((and (<= (overlay-start ov) content)
                   (= (overlay-end ov) insert-beg))
              (move-overlay ov (overlay-start ov) insert-end))))))
      (git-appraise-folding--mark-commented-hunk hunk))))

(defun git-appraise-comment--insert-obj-in-hunk (hunk obj insert-beg)
  "Insert comment OBJ into HUNK at INSERT-BEG within the hunk section.
Sets up the magit section insertion context and absorbs the boundary."
  (let ((magit-insert-section--parent hunk)
        (magit-insert-section--current hunk)
        (magit-insert-section--oldroot magit-root-section))
    (git-appraise-comment-insert-lines obj)
    (git-appraise-comment--absorb-in-hunk hunk insert-beg (point))))

(defun git-appraise-comment--save-draft (obj entry)
  "Persist OBJ's draft ENTRY, replacing an earlier save atomically.
Keep its identity on OBJ so a render failure followed by retry cannot
duplicate the comment."
  (let* ((previous (and (slot-boundp obj 'internalId) (oref obj internalId)))
         (node (car (a-get-in entry '(comments nodes))))
         (id (or previous (a-get node 'internal-id)))
         (entry (a-assoc-in entry '(comments nodes)
                            (list (a-assoc node 'internal-id id)))))
    (when previous
      (git-appraise-db-delete-raw-comment previous))
    (git-appraise-db--pullreq-raw-comments-update entry)
    (oset obj internalId id)))

(defun git-appraise-comment--goto-saved-comment (obj &optional buff-name)
  "Move point to OBJ's section in BUFF-NAME after a draft rebuild.
Match by internalId because rebuild recreates comment objects."
  (let* ((id (and (slot-boundp obj 'internalId) (oref obj internalId)))
         (buff-name (or buff-name (git-appraise-pr-buffer-name)))
         (buf (get-buffer buff-name))
         (section (and id buf (git-appraise-comment--find-section-by-internal-id buff-name id))))
    (when section
      (with-current-buffer buf
        (goto-char (oref section start))))))

(defun git-appraise-comment--section-has-internal-id-p (section id)
  "Return non-nil when SECTION's value has INTERNAL-ID ID."
  (let ((v (and (slot-boundp section 'value) (oref section value))))
    (and (eieio-object-p v)
         (slot-exists-p v 'internalId)
         (slot-boundp v 'internalId)
         (equal (oref v internalId) id))))

(defun git-appraise-comment--find-section-by-internal-id (buff-name id)
  "Find the outermost magit section in BUFF-NAME whose value has INTERNAL-ID ID."
  (when (and id (get-buffer buff-name))
    (git-appraise-section--find-section
     buff-name
     (lambda (s)
       (and (git-appraise-comment--section-has-internal-id-p s id)
            (let ((parent (and (slot-boundp s 'parent) (oref s parent))))
              (not (and parent
                        (git-appraise-comment--section-has-internal-id-p parent id)))))))))

(defun git-appraise-comment--replace-local-comment-in-place (obj buff-name)
  "Replace existing local comment section for OBJ in BUFF-NAME.
Return non-nil on success."
  (let* ((id (and (slot-boundp obj 'internalId) (oref obj internalId)))
         (buf (get-buffer buff-name))
         (section (and buf (git-appraise-comment--find-section-by-internal-id buff-name id))))
    (when (and section (buffer-live-p buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (start (copy-marker (oref section start)))
              (end (copy-marker (oref section end) t))
              (parent (oref section parent))
              (hunk (git-appraise-comment--hunk-ancestor section)))
          (save-excursion
            (goto-char start)
            (set-marker-insertion-type start nil)
            (set-marker-insertion-type end t)
            (when parent
              (oset parent children (delq section (oref parent children))))
            (delete-region start end)
            (goto-char start)
            (git-appraise-comment--insert-obj-in-hunk hunk obj start)
            (set-marker start nil)
            (set-marker end nil)
            t))))))

(defun git-appraise-comment--find-section-by-id (buff-name id)
  "Find a comment section in BUFF-NAME whose value has backend ID."
  (when (and id (get-buffer buff-name))
    (git-appraise-section--find-section
     buff-name
     (lambda (s)
       (let ((v (and (slot-boundp s 'value) (oref s value))))
         (and (eieio-object-p v)
              (slot-exists-p v 'id)
              (slot-boundp v 'id)
              (equal (oref v id) id)))))))

(defun git-appraise-comment--insert-reply-in-place (obj buff-name)
  "Insert reply OBJ into BUFF-NAME after its parent comment section.
Return non-nil on success."
  (let* ((buf (get-buffer buff-name))
         (parent-id (and (slot-boundp obj 'id) (oref obj id)))
         (parent (and buf (git-appraise-comment--find-section-by-id buff-name parent-id))))
    (when (and parent (buffer-live-p buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (hunk (git-appraise-comment--hunk-ancestor parent)))
          (when hunk
            (save-excursion
              (goto-char (oref parent end))
              (git-appraise-comment--insert-obj-in-hunk hunk obj (point))
              t)))))))

(defun git-appraise-comment--insert-local-comment-in-place (obj buff-name cursor-pos)
  "Insert local draft comment OBJ into BUFF-NAME at CURSOR-POS.
Return non-nil on success.  Falls back to nil when the anchor cannot
be found, so callers can rebuild."
  (let ((buf (get-buffer buff-name)))
    (when (and buf cursor-pos)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (pos (min cursor-pos (point-max))))
          (save-excursion
            (goto-char pos)
            (let* ((section (magit-current-section))
                   (hunk (git-appraise-comment--hunk-ancestor section))
                   (expanded nil))
              ;; If cursor is on a lazy placeholder, expand it first.
              (when (and (not hunk)
                         (git-appraise-lazy-placeholder-section-p section))
                (setq section (git-appraise-lazy--expand-at section))
                (setq expanded t)
                (setq hunk (and section
                                (git-appraise-comment--hunk-ancestor section))))
              (cond
               (hunk
                (goto-char pos)
                (forward-line 1)
                ;; Skip over existing comment sections at this anchor.
                (while (let ((s (magit-current-section)))
                         (and s
                              (not (eq s hunk))
                              (cl-typep s 'git-appraise-base-comment-section)
                              (progn (goto-char (oref s end)) t))))
                (git-appraise-comment--insert-obj-in-hunk hunk obj (point))
                t)
               ;; FILE-level comment: expansion washes it in via the
               ;; missing-comment pass, so no hunk insertion is needed.
               ((and expanded
                     (string-equal (or (oref obj line-type) "") "FILE"))
                t)
               ((string-equal (or (oref obj line-type) "") "FILE")
                (let* ((path (oref obj path))
                       (placeholder
                        (git-appraise-section--find-section
                         buff-name
                         (lambda (s)
                           (and (git-appraise-lazy-placeholder-section-p s)
                                (equal (plist-get (oref s value) :path) path))))))
                  (when placeholder
                    (with-current-buffer buf
                      (git-appraise-lazy--expand-at placeholder))
                    t)))
               (t nil)))))))))

(cl-defmethod git-appraise-comment-handler-commit ((obj git-appraise-reply-comment-section) default-buff-msg)
  "Commit the reply OBJ and clean the DEFAULT-BUFF-MSG from the text if any."
  (if (git-appraise-backend-repo-p (git-appraise-db-get-pullreq))
      (let* ((pr (git-appraise-db-get-pullreq))
             (clean-msg (git-appraise-utils--comment-clean-msg
                         (oref obj msg) default-buff-msg))
             (internal-id (uuidgen-4))
             (created-at (git-appraise-time-now-utc))
             (raw-comment `((author (login . ,(oref obj author)))
                            (state . "COMMENTED")
                            (comments (nodes ((internal-id . ,internal-id)
                                              (bodyHTML . ,(concat "<p>" (xml-escape-string clean-msg) "</p>"))
                                              (bodyText . ,clean-msg)
                                              (path . ,(oref obj path))
                                              (position . ,(oref obj position))
                                              (databaseId . ,(format "local-%s" (uuidgen-4)))
                                              (createdAt . ,created-at)
                                              (updatedAt . ,(git-appraise-time-now-utc))))))))
        (git-appraise-backend-send-reply pr (oref obj id) clean-msg)
        (oset obj createdAt created-at)
        (oset obj state "SENT")
        (oset obj internalId internal-id)
        (oset pr raw-comments (append (oref pr raw-comments) (list raw-comment)))
        (git-appraise-db-update pr)
        (if (git-appraise-comment--insert-reply-in-place obj (git-appraise-pr-buffer-name))
            (git-appraise-comment--goto-saved-comment obj)
          (let ((git-appraise-section-full-refresh? nil))
            (git-appraise--build-buffer))
          (git-appraise-comment--goto-saved-comment obj))
        (setq git-appraise-comment-uncommitted nil))
    (let* ((reply-pos (- (+ (oref obj position)
                          (length (split-string (oref obj msg) "\n")))
                       2))
         (clean-msg (git-appraise-utils--comment-clean-msg
                     (oref obj msg)
                     default-buff-msg))
         (created-at (if (slot-boundp obj 'createdAt)
                         (oref obj createdAt)
                       (git-appraise-time-now-utc)))
         (raw-comment `((author (login . ,(oref obj author)))
                        (state . ,(oref obj state))
                        (comments (nodes ((internal-id . ,(uuidgen-4))
                                          (bodyText . ,clean-msg)
                                          (path . ,(oref obj path))
                                          (position . ,reply-pos)
                                          (databaseId . ,(oref obj id))
                                          (diffHunk . ,(oref obj diffHunk))
                                          (createdAt . ,created-at)
                                          (outdated)
                                          (local?)
                                          (reply? . t)))))))
    (when (string-empty-p clean-msg)
      (user-error "Comment is empty"))
    (oset obj createdAt created-at)
    (git-appraise-comment--save-draft obj raw-comment)
    (if (git-appraise-comment--insert-reply-in-place obj (git-appraise-pr-buffer-name))
        (git-appraise-comment--goto-saved-comment obj)
      (let ((git-appraise-section-full-refresh? nil))
        (git-appraise--build-buffer))
      (git-appraise-comment--goto-saved-comment obj))
    (setq git-appraise-comment-uncommitted nil))))

(cl-defmethod git-appraise-comment-handler-commit ((obj git-appraise-local-comment-section) default-buff-msg)
  "Commit the local comment OBJ and clean the DEFAULT-BUFF-MSG from the text if any."
  (let* ((buff-name (if git-appraise-comment-commit-buffer?
                        git-appraise-commit-buffer-name
                      (git-appraise-pr-buffer-name)))
         (clean-msg (git-appraise-utils--comment-clean-msg
                     (oref obj msg)
                     default-buff-msg))
         (created-at (if (slot-boundp obj 'createdAt)
                         (oref obj createdAt)
                       (git-appraise-time-now-utc)))
         (raw-comment `((author (login . ,(oref obj author)))
                        (state . ,(oref obj state))
                        (comments (nodes ((internal-id . ,(uuidgen-4))
                                          (bodyText . ,clean-msg)
                                          (path . ,(oref obj path))
                                          ;; keep position slot out; internal uses may read it as nil
                                          (position . ,(oref obj position))
                                          (line . ,(oref obj line))
                                          (side . ,(ignore-errors (oref obj side)))
                                          (startLine . ,(ignore-errors (oref obj start-line)))
                                          (startSide . ,(ignore-errors (oref obj start-side)))
                                          (databaseId)
                                          (createdAt . ,created-at)
                                          (diffHunk)
                                          (outdated)
                                          (reply?)
                                          (line-type . ,(oref obj line-type))
                                          (local? . t)))))))

    (when (string-empty-p clean-msg)
      (user-error "Comment is empty"))
    (oset obj createdAt created-at)
    (if (oref obj send?)
        (let ((pr-id git-appraise-db--pullreq-id)
              (draft-id (and (slot-boundp obj 'internalId) (oref obj internalId))))
          (oset obj msg clean-msg)
          (when (git-appraise-backend-repo-p (git-appraise-db-get-pullreq))
            (git-appraise-comment--save-draft obj raw-comment)
            (setq draft-id (oref obj internalId)))
          (git-appraise-new-code-comment
           (git-appraise-db-get-pullreq)
           obj
           (lambda (&rest _)
             (let ((git-appraise-db--pullreq-id pr-id))
               (when draft-id
                 (git-appraise-db-delete-raw-comment draft-id))
               ;; async callback: mark THIS comment sent in place — no
               ;; full re-render, no re-fetch
               (let* ((buf (get-buffer buff-name))
                      (section (and buf
                                    (git-appraise-section--find-section
                                     buf
                                     (lambda (s)
                                       (and (memq (oref s type)
                                                  '(git-appraise-local-comment-section
                                                    git-appraise-reply-comment-section))
                                           (eq (oref s value) obj)))))))
                 (if (and section
                          (git-appraise-section--mark-comment-sent-in-place
                           buff-name section))
                     (message "Comment sent")
                   ;; no rendered draft section: an immediately sent
                   ;; file-level comment.  Read the canonical comment
                   ;; set once and re-render locally from the cached
                   ;; diff: no `show -diff', no second CLI call, no
                   ;; `Load review...'.
                   (let ((cursor-pos (or (and buf (window-live-p (get-buffer-window buf))
                                                (window-point (get-buffer-window buf)))
                                           (and buf (with-current-buffer buf (point)))
                                           git-appraise-comment-cursor-pos)))
                     (pcase (git-appraise-backend-refresh-comments
                             (git-appraise-db-get-pullreq))
                       ('ok
                        ;; DB refreshed; avoid a full diff rebuild.
                        ;; Patch the draft section in place when present;
                        ;; otherwise do a non-full rebuild (no diff reload).
                        (let ((patched
                               (and (buffer-live-p buf)
                                    (git-appraise-section--mark-comment-sent-in-place
                                     buff-name
                                     (git-appraise-section--find-section
                                      buf
                                      (lambda (s)
                                        (and (memq (oref s type)
                                                   '(git-appraise-local-comment-section
                                                     git-appraise-reply-comment-section))
                                             (eq (oref s value) obj))))))))
                          (unless patched
                            (let ((git-appraise-section-full-refresh? nil)
                                  (git-appraise-comment-cursor-pos cursor-pos))
                              (git-appraise--build-buffer buff-name))))
                        (message "Comment sent"))
                       ('head-changed
                        (let ((git-appraise-section-full-refresh? t)
                              (git-appraise-comment-cursor-pos cursor-pos))
                          (git-appraise--build-buffer buff-name)
                          (message "Comment sent")))
                       (_
                        (message "Comment sent; refresh display to see it"))))))))))
      (progn
        (git-appraise-comment--save-draft obj raw-comment)
        (let ((buf (get-buffer buff-name)))
          (when buf
            (with-current-buffer buf
              (setq git-appraise-section-grouped-comments
                    (git-appraise-utils-make-group
                     (git-appraise-db--pullreq-raw-comments)))))
          (let* ((cursor-pos git-appraise-comment-cursor-pos)
                 (id (and (slot-boundp obj 'internalId) (oref obj internalId)))
                 (already (and id
                               (not (oref obj edit?))
                               (git-appraise-comment--find-section-by-internal-id buff-name id)))
                 (done (or already
                           (if (oref obj edit?)
                               (git-appraise-comment--replace-local-comment-in-place obj buff-name)
                             (git-appraise-comment--insert-local-comment-in-place
                              obj buff-name cursor-pos)))))
            (if done
                (git-appraise-comment--goto-saved-comment obj buff-name)
              (let ((git-appraise-section-full-refresh? nil))
                (git-appraise--build-buffer buff-name))
              (git-appraise-comment--goto-saved-comment obj buff-name)))
          ;; Lazy wash can delay missing-comment insertion; drain so a
          ;; just-saved draft is visible immediately.
          (when (and buf (buffer-live-p buf))
            (with-current-buffer buf
              (when (or git-appraise-lazy--wash-pos git-appraise-lazy--queue
                        (bound-and-true-p deferred:queue))
                (let ((deadline (+ (float-time) 30)))
                  (catch 'git-appraise-lazy-drain-timeout
                    (while (or git-appraise-lazy--wash-pos git-appraise-lazy--queue
                               (bound-and-true-p deferred:queue))
                      (when (> (float-time) deadline)
                        (throw 'git-appraise-lazy-drain-timeout nil))
                      (sit-for 0.05)))))))
          (git-appraise-comment--goto-saved-comment obj buff-name))))))

(provide 'git-appraise-comment-draft)
;;; git-appraise-comment-draft.el ends here
