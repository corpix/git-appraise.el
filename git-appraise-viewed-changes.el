;;; git-appraise-viewed-changes.el --- Changed lines since a viewed snapshot -*- lexical-binding: t; -*-

;;; Commentary:
;; Compare the current file patch with the patch last marked fully viewed.
;; Render one margin cue per changed current diff line.  Review metadata
;; remains in git-appraise; these snapshots are personal SQLite state.

;;; Code:

(require 'dash)
(require 'magit-section)
(require 'git-appraise-store)
(require 'git-appraise-viewed)
(declare-function git-appraise--patch-line-p "git-appraise-section")

(defface git-appraise-viewed-change-bar
  '((t :inherit warning :weight bold))
  "Margin bar for lines changed since a file was viewed."
  :group 'git-appraise)

(defvar-local git-appraise-viewed-changes--targets nil
  "Hash of path to changed current patch lines, prepared before wash.")
(put 'git-appraise-viewed-changes--targets 'permanent-local t)

(defun git-appraise-viewed-changes--restore-window-margins ()
  "Restore margins this feature added to windows no longer showing a review."
  (--each (frame-list)
    (--each (window-list it 'no-minibuf)
      (when-let* ((old (window-parameter it 'git-appraise-viewed-old-margins)))
        (unless (with-current-buffer (window-buffer it)
                  (derived-mode-p 'git-appraise-mode))
          (set-window-margins it (car old) (cdr old))
          (set-window-parameter it 'git-appraise-viewed-old-margins nil))))))

(add-hook 'window-configuration-change-hook
          #'git-appraise-viewed-changes--restore-window-margins)

(defun git-appraise-viewed-changes--same-content-p (path old current)
  "Whether OLD and CURRENT have the same hunk content for PATH."
  (let ((old-hunks (git-appraise-viewed--block-hunks path old))
        (new-hunks (git-appraise-viewed--block-hunks path current)))
    (and old-hunks new-hunks
         (equal (--map (plist-get it :fp) old-hunks)
                (--map (plist-get it :fp) new-hunks)))))

(defun git-appraise-viewed-changes--patch-line-p (line)
  "Whether LINE is a content line of a raw patch."
  (and (string-match-p "\\`[ +-]" line)
       (not (string-prefix-p "+++" line))
       (not (string-prefix-p "---" line))))

(defun git-appraise-viewed-changes--patch-ordinals (block changed)
  "Return patch-line ordinals in BLOCK whose raw line is in CHANGED."
  (let ((line-number 0)
        (ordinal 0)
        targets)
    (--each (split-string block "\n" t)
      (cl-incf line-number)
      (when (git-appraise-viewed-changes--patch-line-p it)
        (cl-incf ordinal)
        (when (member line-number changed)
          (push ordinal targets))))
    (nreverse targets)))

(defun git-appraise-viewed-changes--line-numbers (old current)
  "Return CURRENT line numbers changed from OLD using Git's zero-context diff."
  (let ((old-file (make-temp-file "git-appraise-viewed-old-"))
        (new-file (make-temp-file "git-appraise-viewed-new-"))
        result)
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old))
          (with-temp-file new-file (insert current))
          (with-temp-buffer
            (let ((status (call-process "git" nil t nil "diff" "--no-index"
                                        "--no-ext-diff" "--unified=0" "--"
                                        old-file new-file)))
              (unless (memq status '(0 1))
                (error "Could not compare viewed diff snapshots")))
            (goto-char (point-min))
            (while (re-search-forward
                    "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
                    nil t)
              (let ((start (string-to-number (match-string 1)))
                    (count (if (match-string 2)
                               (string-to-number (match-string 2)) 1)))
                (if (zerop count)
                    (push (max 1 start) result)
                  (dotimes (offset count)
                    (push (+ start offset) result)))))))
      (delete-file old-file)
      (delete-file new-file))
    (delete-dups (sort result #'<))))

(defun git-appraise-viewed-changes--prepare (diff)
  "Prepare changed line targets for the rendered DIFF."
  (let* ((repo (git-appraise-viewed--repo-dir))
         (revision (git-appraise-viewed--revision))
         (actor (and repo (git-appraise-viewed--actor repo)))
         (saved (and actor
                     (condition-case err
                         (git-appraise-store-viewed-snapshots
                          repo revision actor)
                       (error
                        (git-appraise-utils--log
                         "viewed-changes" (error-message-string err))
                        nil))))
         (table (make-hash-table :test #'equal)))
    (when (and saved diff)
      (--each (git-appraise--diff--split-by-files diff)
        (pcase-let ((`(,path . ,block) it))
          (condition-case err
              (when-let* ((old (gethash path saved)))
                (unless (or (equal old block)
                            (git-appraise-viewed-changes--same-content-p
                             path old block))
                  (let* ((changed (git-appraise-viewed-changes--line-numbers
                                   old block))
                         (targets (git-appraise-viewed-changes--patch-ordinals
                                   block changed)))
                    (puthash path (or targets '(:heading)) table))))
            (error
             (git-appraise-utils--log
              "viewed-changes" (error-message-string err)))))))
    (setq git-appraise-viewed-changes--targets table)))

(defun git-appraise-viewed-changes--mark (pos)
  "Place an orange margin bar at POS, replacing an earlier one there."
  (remove-overlays pos (min (1+ pos) (point-max))
                   'git-appraise-viewed-change-bar t)
  (let ((ov (make-overlay pos (min (1+ pos) (point-max)))))
    (overlay-put ov 'git-appraise-viewed-change-bar t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'before-string
                 (propertize " " 'display
                             `((margin left-margin)
                               ,(propertize "▏" 'face
                                            'git-appraise-viewed-change-bar))))))

(defun git-appraise-viewed-changes--paint ()
  "Paint prepared changed lines in the current review buffer."
  (--each (overlays-in (point-min) (point-max))
    (when (overlay-get it 'git-appraise-viewed-change-bar)
      (delete-overlay it)))
  (when (hash-table-p git-appraise-viewed-changes--targets)
    (magit-map-sections
     (lambda (section)
       (when (magit-file-section-p section)
         (let* ((path (git-appraise-viewed--file-path section))
                (targets (copy-sequence
                          (gethash path git-appraise-viewed-changes--targets)))
                (limit (oref section end)))
           (when targets
             (if (eq (car targets) :heading)
                 (git-appraise-viewed-changes--mark (oref section start))
               (save-excursion
                 (goto-char (oref section start))
                 (let ((ordinal 0))
                   (while (and (< (point) limit) targets)
                     (when (git-appraise--patch-line-p)
                       (cl-incf ordinal)
                       (when (= ordinal (car targets))
                         (git-appraise-viewed-changes--mark
                          (line-beginning-position))
                         (pop targets)))
                     (forward-line 1))))))))))
    (--each (get-buffer-window-list (current-buffer) nil t)
      (let ((margins (window-margins it)))
        (when (< (or (car margins) 0) 1)
          (set-window-parameter it 'git-appraise-viewed-old-margins margins)
          (set-window-margins it 1 (cdr margins)))))))

(provide 'git-appraise-viewed-changes)
;;; git-appraise-viewed-changes.el ends here
