;;; git-appraise-section.el --- UI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski

;;; Commentary:
;;  Code to build the UI.

;;; Code:

(require 'deferred)
(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'shr)

(require 'git-appraise-faces)
(require 'git-appraise-db)
(require 'git-appraise-utils)
(require 'git-appraise-repo)
(require 'git-appraise-diff)
(require 'git-appraise-backend)
(require 'git-appraise-viewed)
(require 'git-appraise-viewed-changes)

(declare-function git-appraise--diff--classify-diff "git-appraise-diff")
(declare-function git-appraise--maybe-reorder-diff "git-appraise-diff")
(declare-function git-appraise-diff--strip-path "git-appraise-diff")

(defmacro git-appraise--stage-time (stage &rest body)
  "Run BODY, logging wall-clock seconds as STAGE when `git-appraise-stage-timing?'."
  (declare (indent 1))
  (let ((beg (make-symbol "beg")))
    `(let ((,beg (and git-appraise-stage-timing? (float-time))))
       (unwind-protect
           (progn ,@body)
         (when ,beg
           (git-appraise-utils--log
            "stage-timing"
            (format "%s: %.3fs" ,stage (- (float-time) ,beg))))))))

(require 'git-appraise-section-shared)
(require 'git-appraise-section-header)
(require 'git-appraise-section-comment)
(require 'git-appraise-render)
(require 'git-appraise-lazy)
(require 'git-appraise-section-history)
(require 'git-appraise-section-wash)
(require 'git-appraise-folding)

(defcustom git-appraise-buffer-name "*Git Appraise*"
  "Fallback name of the git-appraise main buffer."
  :group 'git-appraise
  :type 'string)

(defcustom git-appraise-commit-buffer-name "*Git Appraise Commit*"
  "Name of the git-appraise commit buffer."
  :group 'git-appraise
  :type 'string)

(defcustom git-appraise-new-buffer-window-strategy
  #'switch-to-buffer-other-window
  "Function used after creating a new Git Appraise buffer."
  :group 'git-appraise
  :type 'function)

(defcustom git-appraise-log-raw-request-responses nil
  "Log raw request responses."
  :type 'boolean
  :group 'git-appraise)

(defcustom git-appraise-stage-timing? nil
  "Non-nil logs stage timing to `git-appraise-log-file'."
  :type 'boolean
  :group 'git-appraise)

(defvar-local git-appraise-review-buffer-pr-id nil
  "Database id of the pull request displayed in this review buffer.")
(put 'git-appraise-review-buffer-pr-id 'permanent-local t)

(defvar-local git-appraise-comment-review-buffer nil
  "Review buffer a comment buffer was opened for.")
(put 'git-appraise-comment-review-buffer 'permanent-local t)

(defvar git-appraise-section--new-since-last-review nil)
(defvar git-appraise-section--just-refetched nil)
(defvar-local git-appraise-section--viewed nil)
(put 'git-appraise-section--viewed 'permanent-local t)

(defvar magit-root-section)
(defvar git-appraise-comment-commit-buffer?)
(defvar git-appraise-comment-cursor-pos)
(defvar git-appraise-post-hook)

(defclass git-appraise--root-section (magit-section)
  ((body :initform nil)))

(defun git-appraise-pr-buffer-name (&optional pr)
  "Return the review buffer name for PR."
  (let ((pr (or pr (ignore-errors (git-appraise-db-get-pullreq)))))
    (if (and pr
             (slot-boundp pr 'owner)
             (slot-boundp pr 'repo)
             (slot-boundp pr 'number))
        (format "*Git Appraise: %s/%s#%s*"
                (oref pr owner)
                (replace-regexp-in-string "%2F" "/" (oref pr repo))
                (oref pr number))
      git-appraise-buffer-name)))

(defun git-appraise-review-buffer ()
  "Return the review buffer for the PR being acted on, or nil."
  (cond ((and (bound-and-true-p git-appraise-review-buffer-pr-id)
              (buffer-live-p (current-buffer)))
         (current-buffer))
        ((and (bound-and-true-p git-appraise-comment-review-buffer)
              (buffer-live-p git-appraise-comment-review-buffer))
         git-appraise-comment-review-buffer)
        (t (get-buffer (git-appraise-pr-buffer-name)))))

(defun git-appraise--sync-db-pullreq ()
  "Make the DB current pullreq match the PR shown in this buffer."
  (when (and git-appraise-review-buffer-pr-id
             (not (equal git-appraise-db--pullreq-id
                         git-appraise-review-buffer-pr-id)))
    (setq git-appraise-db--pullreq-id git-appraise-review-buffer-pr-id)))

(defun git-appraise-section--setup-worktree ()
  "Bind `git-appraise-repo-worktree' and `default-directory'."
  (when (and git-appraise-repo-enable
             (not git-appraise-repo-worktree))
    (setq git-appraise-repo-worktree
          (or (let ((root (ignore-errors (oref (git-appraise-db-get-pullreq) host))))
                (and root (file-name-as-directory root)))
              (magit-toplevel))))
  (when git-appraise-repo-worktree
    (setq default-directory
          (file-name-as-directory git-appraise-repo-worktree))))

(defun git-appraise-section--insert-pr-text ()
  "Insert the raw PR diff text into the review buffer."
  (save-excursion
    (setq git-appraise-section--file-classifications
          (git-appraise--diff--classify-diff
           (git-appraise-db--pullreq-raw-diff)))
    ;; viewed marks: project the loaded note onto this diff and flag
    ;; the classifications (shared by the washer and lazy placeholders)
    (git-appraise-viewed--render-prepare
     git-appraise-section--file-classifications)
    (git-appraise-viewed-changes--prepare
     (git-appraise-db--pullreq-raw-diff))
    (git-appraise-fold-state--prepare)
    (erase-buffer)
    (insert (git-appraise--maybe-reorder-diff
             (git-appraise-db--pullreq-raw-diff)
             git-appraise-section--file-classifications))
    (insert ?\n)))

(defun git-appraise-section--render-diff-tree (commit-focus? &optional fresh-render?)
  "Render the section tree over the inserted diff text."
  (magit-insert-section section (git-appraise--root-section)
    (magit-insert-section (git-appraise)
      (git-appraise--stage-time "sections-hook"
        (magit-run-section-hook (if commit-focus?
                                    'git-appraise-sections-commit-hook
                                  'git-appraise-sections-hook))))
    (magit-insert-section (git-appraise-files-report-section)
      (git-appraise--stage-time "insert-files-changed"
        (git-appraise-section-insert-files-changed))
      (magit-insert-section (git-appraise-files-chnged)
        (save-restriction
          (narrow-to-region (point) (point-max))
          (git-appraise--stage-time "diff-wash-hook"
            (run-hooks 'magit-diff-wash-diffs-hook))
          (git-appraise--stage-time "lazy-wash-precheck"
            (git-appraise-lazy--precheck))
          (if git-appraise-lazy-enabled
              (git-appraise-lazy--schedule commit-focus? fresh-render?)
            (git-appraise--stage-time "lazy-wash-files"
              (progn
                (git-appraise-lazy--wash-all)
                (unless (eobp)
                  (error "Code review stopped before the end of the diff")))))))
    (git-appraise-history-insert-unmatched))))

(defun git-appraise-section--fold-and-mark (fresh-render? &optional commit-focus?)
  "Fold bot comments and hunks as configured."
  (when (and git-appraise-collapse-bot-comments
             fresh-render?)
    (git-appraise--collapse-bot-comments magit-root-section))
  (when magit-root-section
    (git-appraise-section--fold-hunks
     magit-root-section
     (not (or fresh-render? git-appraise-section--just-refetched))))
  (when git-appraise-comment-fringe-markers
    (git-appraise--mark-comment-lines))
  (git-appraise-viewed-changes--paint))

(defun git-appraise--update-header-line (&rest _)
  "Update sticky header line for the current review buffer."
  nil)

(defun git-appraise-section--activate-review-buffer (buff-name window ws commit-focus?)
  "Display BUFF-NAME and set up its hooks."
  (if window
      (progn
        (pop-to-buffer buff-name)
        (set-window-start window ws))
    (progn
      (funcall git-appraise-new-buffer-window-strategy buff-name)
      (goto-char (point-min))))
  (setq git-appraise-review-buffer-pr-id git-appraise-db--pullreq-id)
  (add-hook 'post-command-hook #'git-appraise--sync-db-pullreq nil t)
  (setq header-line-format nil)
  (add-hook 'post-command-hook #'git-appraise--update-header-line nil t)
  (add-hook 'window-scroll-functions #'git-appraise--update-header-line nil t)
  (when commit-focus?
    (git-appraise-commit-minor-mode 1))
  (git-appraise-section-insert-header-title))

(defun git-appraise--trigger-hooks (buff-name &optional commit-focus? msg)
  "Render BUFF-NAME."
  (when commit-focus?
    (setq git-appraise-section--new-since-last-review nil))
  (with-current-buffer (get-buffer-create buff-name)
    (git-appraise--stage-time "grouped-comments"
      (setq git-appraise-section-grouped-comments
            (git-appraise-utils-make-group
             (git-appraise-db--pullreq-raw-comments))
            git-appraise-section-hold-written-comment-count nil
            git-appraise-section-hold-written-comment-ids nil))
    (git-appraise--stage-time "mode-body"
      (unless (derived-mode-p 'git-appraise-mode)
        (git-appraise-mode)))
    (git-appraise--stage-time "setup-worktree"
      (git-appraise-section--setup-worktree))
    (let* ((window (get-buffer-window buff-name))
           (ws (window-start window))
           (inhibit-read-only t)
           (fresh-render? (not magit-root-section))
           ;; the anchor may be a marker that no longer belongs to this
           ;; buffer (stale state left behind by an earlier flow): only
           ;; a live position here is restorable
           (anchor-pos (let ((anchor (or git-appraise-comment-cursor-pos
                                         (if window
                                             (window-point window)
                                           (point)))))
                         (cond ((numberp anchor) anchor)
                               ((and (markerp anchor)
                                     (eq (marker-buffer anchor)
                                         (current-buffer)))
                                (marker-position anchor)))))
           (target (and magit-root-section anchor-pos
                        (git-appraise-lazy--position-chain
                         (min anchor-pos (point-max))))))
      (git-appraise-render--call
       (lambda ()
         (git-appraise--stage-time "insert-pr-text"
           (git-appraise-section--insert-pr-text))
         (git-appraise--stage-time "render-diff-tree"
           (git-appraise-section--render-diff-tree commit-focus? fresh-render?))
         (git-appraise--stage-time "fold-and-mark"
           (git-appraise-section--fold-and-mark fresh-render? commit-focus?))))
      (git-appraise--stage-time "activate-review-buffer"
        (git-appraise-section--activate-review-buffer
         buff-name window ws commit-focus?))
      ;; resolve the cursor's logical position once, here: the
      ;; background loader then only carries it along with the text
      (when target
        (git-appraise-lazy--restore-position target)))
      (when msg
        (message nil)
        (message msg))
      (dolist (post-fn (delq nil (if (functionp git-appraise-post-hook)
                                     (list git-appraise-post-hook)
                                   (copy-sequence git-appraise-post-hook))))
        (git-appraise--stage-time
            (format "post-hook:%s" post-fn)
          (funcall post-fn)))))

(defun git-appraise--report-build-error (err)
  "Log build-chain ERR and message the user."
  (git-appraise-utils--log "git-appraise--build-buffer" (prin1-to-string err))
  (message "Got an error. Check `git-appraise-log-file'."))

(defun git-appraise--build-buffer (&optional buf-name commit-focus? msg)
  "Build BUF-NAME.  Appraise-only."
  (let ((buff-name (or buf-name (git-appraise-pr-buffer-name))))
    (if (not git-appraise-section-full-refresh?)
        (git-appraise--trigger-hooks buff-name commit-focus? msg)
      (let ((obj (git-appraise-db-get-pullreq))
            (progress (make-progress-reporter "Load review..." 1 3)))
        (progress-reporter-update progress 1)
        (condition-case err
            (progn
              (git-appraise-backend-refresh obj buff-name msg)
              (progress-reporter-done progress))
          (user-error
           (progress-reporter-done progress)
           (signal (car err) (cdr err)))
          (error
           (progress-reporter-done progress)
           (git-appraise--report-build-error err)))))))

;;; Patch line helpers

(defun git-appraise--section-in-review-subsection-p (&optional section)
  "Return non-nil when SECTION (or current section) is inside a review UI subsection."
  (let ((cur (or section (magit-current-section)))
        found)
    (while (and cur (not found))
      (setq found (or (and (fboundp 'git-appraise-code-comment-section-p)
                           (git-appraise-code-comment-section-p cur))
                      (and (fboundp 'git-appraise-reply-comment-section-p)
                           (git-appraise-reply-comment-section-p cur))
                      (and (fboundp 'git-appraise-local-comment-section-p)
                           (git-appraise-local-comment-section-p cur))
                      (and (fboundp 'git-appraise-outdated-comment-section-p)
                           (git-appraise-outdated-comment-section-p cur))))
      (setq cur (and (slot-boundp cur 'parent) (oref cur parent))))
    found))

(defun git-appraise--patch-line-p ()
  "Return non-nil if point is on a patch line inside a Magit hunk."
  (and (magit-hunk-section-p (magit-current-section))
       (not (git-appraise--section-in-review-subsection-p))
       (let ((c (char-after (line-beginning-position))))
         (or (eq c ?\s) (eq c ?+) (eq c ?-)))))

(defun git-appraise--hunk-content-start (hunk)
  "Return buffer position of first content line for HUNK."
  (save-excursion
    (goto-char (marker-position (oref hunk start)))
    (forward-line 1)
    (line-beginning-position)))

(defun git-appraise--nearest-patch-line-in-hunk (hunk &optional pos)
  "Find nearest patch line position within HUNK from POS."
  (let ((here (or pos (point)))
        prev next)
    (save-excursion
      (goto-char here)
      (while (and (>= (point) (marker-position (oref hunk start)))
                  (not prev))
        (when (git-appraise--patch-line-p)
          (setq prev (line-beginning-position)))
        (forward-line -1))
      (goto-char here)
      (while (and (< (point) (marker-position (oref hunk end)))
                  (not next))
        (when (git-appraise--patch-line-p)
          (setq next (line-beginning-position)))
        (forward-line 1)))
    (or prev next)))

(defun git-appraise--count-patch-advances (hunk start-pos end-pos)
  "Count old/new advances across patch lines from START-POS to END-POS in HUNK."
  (let ((old 0) (new 0))
    (save-excursion
      (goto-char start-pos)
      (while (< (point) end-pos)
        (when (git-appraise--patch-line-p)
          (pcase (char-after (line-beginning-position))
            (?\s (setq old (1+ old) new (1+ new)))
            (?+  (setq new (1+ new)))
            (?-  (setq old (1+ old)))))
        (forward-line 1)))
    (cons old new)))

;;; In-place updates

(defun git-appraise-section--find-section (buf pred)
  "The first magit-section in BUF matching PRED, or nil."
  (let (found)
    (with-current-buffer buf
      (magit-map-sections
       (lambda (s)
         (when (and (not found) (funcall pred s))
           (setq found s)))))
    found))

(defun git-appraise-section--by-you-heading-regexp ()
  "The heading text a local or reply comment section renders with."
  "\\(Comment\\|Reply\\) by YOU · \\(DRAFT\\)")

(defun git-appraise-section--mark-comment-sent-in-place (buff-name section)
  "Mark SECTION's draft heading as sent, in place."
  (let ((start (and (buffer-live-p (get-buffer buff-name))
                    (ignore-errors (oref section start)))))
    (when (and start
               (memq (oref section type)
                     '(git-appraise-local-comment-section
                       git-appraise-reply-comment-section)))
      (let ((inhibit-read-only t))
        (save-excursion
          (with-current-buffer buff-name
            (goto-char start)
            (let* ((lend (line-end-position))
                   (hit (re-search-forward
                         (git-appraise-section--by-you-heading-regexp)
                         lend t)))
              (when hit
                (let* ((beg (match-beginning 2))
                       (end (match-end 2))
                       (props (text-properties-at beg)))
                  (goto-char end)
                  (insert (apply #'propertize
                                 "SENT"
                                 props))
                  (delete-region beg end)))
              hit)))))))

(defun git-appraise-section--mark-local-comments-sent (buff-name)
  "Mark EVERY pending local/reply comment section in BUFF-NAME sent."
  (let ((sent 0))
    (when (get-buffer buff-name)
      (with-current-buffer buff-name
        (magit-map-sections
         (lambda (s)
           (when (git-appraise-section--mark-comment-sent-in-place
                  buff-name s)
             (setq sent (1+ sent)))))))
    sent))

(defun git-appraise-section--update-feedback-in-place (buff-name feedback)
  "Replace the feedback body in BUFF-NAME with FEEDBACK, in place.
Return non-nil when an existing feedback section was updated."
  (let ((buf (get-buffer buff-name))
        section)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (magit-map-sections
         (lambda (s)
           (when (and (not section)
                      (eq (oref s type) 'git-appraise-feedback-section)
                      (let ((p (and (slot-boundp s 'parent) (oref s parent))))
                        (and p (eq (oref p type) 'git-appraise-feedback-section))))
             (setq section s)))
         magit-root-section)
        (when section
          (let* ((inhibit-read-only t)
                 (start (copy-marker (oref section start)))
                 (end (copy-marker (oref section end) t))
                 (parent (oref section parent))
                 (obj (oref section value)))
            (when (and (eieio-object-p obj) (slot-boundp obj 'msg))
              (oset obj msg feedback))
            (save-excursion
              (goto-char start)
              (set-marker-insertion-type start nil)
              (set-marker-insertion-type end t)
              (when parent
                (oset parent children (delq section (oref parent children))))
              (delete-region start end)
              (goto-char start)
              (let ((magit-insert-section--parent parent)
                    (magit-insert-section--current parent)
                    (magit-insert-section--oldroot magit-root-section))
                (magit-insert-section (git-appraise-feedback-section obj)
                  (if feedback
                      (insert feedback)
                    (insert (propertize "No draft yet."
                                        'font-lock-face 'magit-dimmed)))))
              (git-appraise-section--set-feedback-badge-in-place
               section feedback)
              (set-marker start nil)
              (set-marker end nil))
            t))))))

(provide 'git-appraise-section)
;;; git-appraise-section.el ends here
