;;; git-appraise-comment-events.el --- Published comment actions -*- lexical-binding: t; -*-

(require 'a)
(require 'cl-lib)
(require 'json)
(require 'markdown-mode)
(require 'magit-section)
(require 'subr-x)
(require 'git-appraise-backend)
(require 'git-appraise-db)
(declare-function git-appraise-reload "git-appraise-actions")
(declare-function git-appraise-pr-buffer-name "git-appraise-section" (&optional pr))
(defvar git-appraise-review-buffer-pr-id)

(defconst git-appraise-comment-events--ref "refs/notes/devtools/discuss")

(defun git-appraise-comment-events--find (threads hash)
  "Find HASH among native comment THREADS, including replies."
  (catch 'found
    (cl-labels ((visit (thread)
                  (when (equal (a-get thread 'hash) hash)
                    (throw 'found thread))
                  (dolist (child (a-get thread 'children))
                    (visit child))))
      (dolist (thread threads)
        (visit thread)))
    nil))

(defun git-appraise-comment-events--context (pr hash)
  "Return current native thread for HASH in PR, or signal."
  (let* ((review (git-appraise-backend--read-review
                  (oref pr owner) (oref pr number)))
         (thread (git-appraise-comment-events--find
                  (a-get review 'comments) hash)))
    (or thread (user-error "Published comment is no longer in this review"))))

(defun git-appraise-comment-events--at-point ()
  "Return (PR HASH THREAD) for the published comment at point."
  (let* ((section (magit-current-section))
         (value (and section (oref section value)))
         (hash (and (eieio-object-p value)
                    (slot-exists-p value 'id)
                    (slot-boundp value 'id)
                    (oref value id)))
         (pr (or (and (bound-and-true-p git-appraise-review-buffer-pr-id)
                      (gethash git-appraise-review-buffer-pr-id
                               git-appraise-db--objects))
                 (git-appraise-db-get-pullreq))))
    (unless (and (git-appraise-backend-repo-p pr)
                 (stringp hash) (not (string-empty-p hash)))
      (user-error "No published comment at point"))
    (list pr hash (git-appraise-comment-events--context pr hash))))

(defun git-appraise-comment-events--assert-author (pr thread)
  "Require PR's current Git actor to have authored THREAD."
  (let ((actor (git-appraise-backend--git
                (oref pr owner) "config" "user.email"))
        (author (a-get-in thread '(original author))))
    (unless (equal actor author)
      (user-error "Only the original author can edit this comment"))))

(defun git-appraise-comment-events--same-version-p (a b)
  "Whether native comments A and B have the same effective version."
  (equal (mapcar (lambda (key) (a-get a key))
                 '(timestamp description resolved parent location))
         (mapcar (lambda (key) (a-get b key))
                 '(timestamp description resolved parent location))))

(defun git-appraise-comment-events--append-edit (pr hash expected body status)
  "Append an edit of HASH in PR, checking EXPECTED current version.
BODY is the replacement text.  STATUS is nil for FYI, :false for
needs-work, or t for approved."
  (let* ((thread (git-appraise-comment-events--context pr hash))
         (current (a-get thread 'comment))
         (original (a-get thread 'original))
         (old-time (string-to-number (or (a-get current 'timestamp) "0")))
         (stamp (number-to-string
                 (max (truncate (float-time)) (1+ old-time)))))
    (git-appraise-comment-events--assert-author pr thread)
    (unless (git-appraise-comment-events--same-version-p current expected)
      (user-error "Comment changed while editing; reopen it and retry"))
    (when (string-empty-p (string-trim body))
      (user-error "Published comment text cannot be empty"))
    (unless (and (equal body (a-get current 'description))
                 (equal status (a-get current 'resolved)))
      ;; FIXME: git-appraise's schema supports `original' edit events, but
      ;; its CLI has no edit command.  Use that upstream command when added.
      (let* ((json-false :false)
             (event `((timestamp . ,stamp)
                      (author . ,(a-get original 'author))
                      (original . ,hash)
                      ,@(when (a-get original 'parent)
                          `((parent . ,(a-get original 'parent))))
                      ,@(when (a-get original 'location)
                          `((location . ,(a-get original 'location))))
                      (description . ,(substring-no-properties body))
                      ,@(when status `((resolved . ,status)))
                      (v . 0))))
        (git-appraise-backend--git
         (oref pr owner) "notes"
         (concat "--ref=" git-appraise-comment-events--ref)
         "append" "-m" (json-encode event) (oref pr number))
        (let ((updated (git-appraise-comment-events--context pr hash)))
          (unless (and (equal body (a-get-in updated '(comment description)))
                       (equal status (a-get-in updated '(comment resolved))))
            (user-error "Edit was stored but is not the current version; reload the review")))
        t))))

(defun git-appraise-comment-events--refresh (pr)
  "Reload PR's review buffer after a published comment action."
  (when-let* ((buffer (get-buffer (git-appraise-pr-buffer-name pr))))
    (with-current-buffer buffer
      (git-appraise-reload))))

(defvar-local git-appraise-comment-events--edit-session nil)

(defvar git-appraise-published-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    (define-key map (kbd "C-c C-c") #'git-appraise-comment-events-commit-edit)
    (define-key map (kbd "C-c C-k") #'git-appraise-comment-events-cancel-edit)
    map))

(define-derived-mode git-appraise-published-edit-mode markdown-mode
  "Published Comment Edit"
  "Edit the text of a published git-appraise comment.")

;;;###autoload
(defun git-appraise-edit-published-comment ()
  "Edit the published comment at point, preserving its history."
  (interactive)
  (pcase-let* ((`(,pr ,hash ,thread) (git-appraise-comment-events--at-point))
               (current (a-get thread 'comment)))
    (git-appraise-comment-events--assert-author pr thread)
    (let ((buffer (generate-new-buffer "*Git Appraise Published Edit*")))
      (with-current-buffer buffer
        (git-appraise-published-edit-mode)
        (setq git-appraise-comment-events--edit-session
              (list :pr pr :hash hash :version (copy-tree current)))
        (insert (or (a-get current 'description) "")))
      (switch-to-buffer-other-window buffer))))

(defun git-appraise-comment-events-commit-edit ()
  "Publish the edit in the current published-comment editor."
  (interactive)
  (let* ((session git-appraise-comment-events--edit-session)
         (pr (plist-get session :pr))
         (hash (plist-get session :hash))
         (expected (plist-get session :version))
         (body (string-trim (buffer-substring-no-properties
                             (point-min) (point-max))))
         (window (get-buffer-window (current-buffer) t)))
    (unless (and pr hash expected)
      (user-error "No published comment edit is active"))
    (git-appraise-comment-events--append-edit
     pr hash expected body (a-get expected 'resolved))
    (git-appraise-comment-events--refresh pr)
    (kill-buffer (current-buffer))
    (when (and (window-live-p window)
               (get-buffer (git-appraise-pr-buffer-name pr)))
      (set-window-buffer window (git-appraise-pr-buffer-name pr)))))

(defun git-appraise-comment-events-cancel-edit ()
  "Discard the current published-comment edit."
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
(defun git-appraise-set-published-comment-status (status)
  "Set the authored root comment at point to FYI or Needs work STATUS."
  (interactive (list (completing-read "Comment status: "
                                      '("FYI" "Needs work") nil t)))
  (pcase-let* ((`(,pr ,hash ,thread) (git-appraise-comment-events--at-point))
               (current (a-get thread 'comment)))
    (when (a-get (a-get thread 'original) 'parent)
      (user-error "Change the status of a root comment, not a reply"))
    (git-appraise-comment-events--append-edit
     pr hash current (a-get current 'description)
     (pcase status
       ("FYI" nil)
       ("Needs work" :false)
       (_ (user-error "Unknown comment status: %s" status))))
    (git-appraise-comment-events--refresh pr)))

;;;###autoload
(defun git-appraise-reopen-finding (message)
  "Reopen the addressed finding at point with a needs-work reply MESSAGE."
  (interactive (list (read-string "Reopen finding with reply: ")))
  (pcase-let* ((`(,pr ,hash ,thread) (git-appraise-comment-events--at-point)))
    (unless (and (eq (a-get-in thread '(comment resolved)) :false)
                 (not (eq (a-get thread 'resolved) :false)))
      (user-error "Selected finding is not addressed"))
    (git-appraise-backend--send-comment
     pr message nil nil hash 'needs-work)
    (git-appraise-comment-events--refresh pr)))

(dolist (map (list git-appraise-code-comment-section-map
                   git-appraise-comment-section-map
                   git-appraise-outdated-comment-section-map))
  (define-key map (kbd "e") #'git-appraise-edit-published-comment)
  (define-key map (kbd "C-c C-s")
    #'git-appraise-set-published-comment-status)
  (define-key map (kbd "C-c C-u") #'git-appraise-reopen-finding))

(provide 'git-appraise-comment-events)
;;; git-appraise-comment-events.el ends here
