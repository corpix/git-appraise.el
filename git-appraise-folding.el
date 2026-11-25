;;; git-appraise-folding.el --- section folding for the review buffer -*- lexical-binding: t; -*-

;; The fold commands, extracted from git-appraise-actions.el
;; (the file-size guideline: actions.el was past 800 lines and
;; growing).  One coherent concern: the global level folds (phase
;; 9) and the per-file fold (2026-10-09).

;;; Code:

(require 'git-appraise-fold-state)

(require 'magit-section)
(require 'cl-lib)
(require 'a)
(require 'git-appraise-faces)
(declare-function git-appraise-next-item "git-appraise-actions")
(declare-function git-appraise-lazy-placeholder-section-p "git-appraise-lazy"
                  (section))

(defun git-appraise-eldoc--file-section (section)
  "Nearest file SECTION at or above SECTION, or nil."
  (while (and section (not (magit-file-section-p section)))
    (setq section (and (slot-boundp section 'parent) (oref section parent))))
  section)

(defcustom git-appraise-fold-hunks-by-default t
  "Default fold choice for hunk sections without a diff identity
(rename and mode header sections).  Diff hunks without a saved
choice always follow their file's fold."
  :group 'git-appraise
  :type 'boolean)
(defcustom git-appraise-fold-files-by-default t
  "Start the review with every changed file folded."
  :group 'git-appraise
  :type 'boolean)

(defcustom git-appraise-commented-hunk-indicator "  [comment]"
  "Text shown after a folded hunk heading when that hunk has comments.
Set to nil to disable the marker.  Customize
`git-appraise-commented-hunk-face' to change its appearance."
  :group 'git-appraise
  :type '(choice (const :tag "No marker" nil)
                 (string :tag "Marker text")))

(defun git-appraise-folding--hunk-has-comment-p (section)
  "Whether SECTION contains a rendered inline comment."
  (cl-some (lambda (child)
             (or (memq (oref child type)
                       '(git-appraise-code-comment-section
                         git-appraise-local-comment-section
                         git-appraise-reply-comment-section
                         git-appraise-outdated-comment-section))
                 (git-appraise-folding--hunk-has-comment-p child)))
           (oref section children)))

(defun git-appraise-folding--mark-commented-hunk (section)
  "Update SECTION's collapsed comment marker after a visibility change."
  (when (and (derived-mode-p 'git-appraise-mode)
             (eq (oref section type) 'hunk))
    (save-excursion
      (goto-char (oref section start))
      (let* ((bol (line-beginning-position))
             (eol (line-end-position)))
        (dolist (ov (overlays-in bol (min (point-max) (1+ eol))))
          (when (overlay-get ov 'git-appraise-commented-hunk)
            (delete-overlay ov)))
        (when (and git-appraise-commented-hunk-indicator
                   (oref section hidden)
                   (git-appraise-folding--hunk-has-comment-p section))
          (let ((ov (make-overlay (max bol (1- eol)) eol)))
            (overlay-put ov 'git-appraise-commented-hunk t)
            (overlay-put ov 'after-string
                         (propertize git-appraise-commented-hunk-indicator
                                     'face 'git-appraise-commented-hunk-face
                                     'help-echo "This folded hunk contains comments"))))))))

(defun git-appraise-folding--after-visibility (section)
  "Refresh the comment marker on hunk SECTION."
  (git-appraise-folding--mark-commented-hunk section))

(defun git-appraise-folding--fix-hidden-overlay (section)
  "Ensure SECTION's hidden overlay extends as content is appended.
magit-section-hide creates the invisibility overlay with rear-advance
nil, so text inserted at section end leaks visible.  Set rear-advance t
on the overlay covering section content."
  (when-let ((content (and (slot-boundp section 'content)
                           (oref section content)))
             (end (and (slot-boundp section 'end) (oref section end))))
    (dolist (ov (overlays-in content (min end (1+ content))))
      (when (overlay-get ov 'invisible)
        (setf (overlay-get ov 'rear-advance) t)))))

(advice-add 'magit-section-hide :after #'git-appraise-folding--after-visibility)
(advice-add 'magit-section-hide :after #'git-appraise-folding--fix-hidden-overlay)
(advice-add 'magit-section-show :after #'git-appraise-folding--after-visibility)

(defun git-appraise-folding--file-hidden-p (section)
  "Non-nil when SECTION is inside a hidden file section."
  (let ((file (git-appraise-folding--file-section section)))
    (and file
         (not (eq file section))
         (oref file hidden))))

(defun git-appraise-section--fold-hunks (section &optional keep-visible?)
  "Fold every hunk section under SECTION, recursively.
The default open state: the buffer shows the affected files'
headings, one TAB away from the diffs.  Hunks without a saved
choice are left open, so opening their file reveals them.  With
KEEP-VISIBLE? (a plain re-render), hunks that inherited an
EXPANDED state from the old tree stay open — the re-render only
re-applies the invisible overlay of the folded ones, because
`hidden' alone conceals nothing outside a `magit-refresh-buffer'
pass."
  (dolist (c (oref section children))
    (if (cl-typep c 'magit-hunk-section)
        (let ((folded (git-appraise-fold-state--hunk-folded-p c))
              (file-hidden (git-appraise-folding--file-hidden-p c)))
          (cond
           (file-hidden
            ;; Do not manipulate invisibility overlays inside a hidden
            ;; file: Emacs remove-overlays splits overlapping invisibility
            ;; overlays, which would punch through the file fold.  Still
            ;; update the slot and comment marker.
            (oset c hidden folded)
            (git-appraise-folding--mark-commented-hunk c))
           (folded (magit-section-hide c))
           (t (magit-section-show c))))
      (git-appraise-section--fold-hunks c keep-visible?))))

(defun git-appraise-folding--file-section (section)
  "Return the file or lazy file placeholder containing SECTION."
  (while (and section
              (not (or (magit-file-section-p section)
                       (git-appraise-lazy-placeholder-section-p section))))
    (setq section (and (slot-boundp section 'parent)
                       (oref section parent))))
  section)

(defun git-appraise-folding--file-path (section)
  "Return the canonical path of file SECTION."
  (if (git-appraise-lazy-placeholder-section-p section)
      (plist-get (oref section value) :path)
    (git-appraise-viewed--file-path section)))

(defun git-appraise-folding--file-target (section)
  "Return the persistent target for file SECTION."
  (let* ((path (git-appraise-folding--file-path section))
         (info (and path (hash-table-p git-appraise-viewed--buffer-inventory)
                    (gethash path git-appraise-viewed--buffer-inventory))))
    (when info
      (list "file" path
            (git-appraise-fold-state--file-fingerprint info) nil))))

(defun git-appraise-folding--set-file (section folded)
  "Set SECTION's FOLDED state and persist it first."
  (let ((target (git-appraise-folding--file-target section)))
    (unless target (user-error "No changed file at point"))
    (git-appraise-fold-state--write (list target) folded)
    (if folded (magit-section-hide section)
      (magit-section-show section)
      ;; Unset hunk choices inherit the now open file.  Saved hunk
      ;; choices remain independent of the file choice.
      (--each (oref section children)
        (when (and (magit-hunk-section-p it)
                   (not (git-appraise-fold-state--hunk-folded-p it)))
          (magit-section-show it))))))

(defun git-appraise-folding--set-hunk (section folded)
  "Set hunk SECTION to FOLDED and persist its choice."
  (pcase (git-appraise-fold-state--hunk-identity section)
    (`(,path ,fp ,range)
     (git-appraise-fold-state--write
      (list (list "hunk" path fp range)) folded)))
  (if folded (magit-section-hide section)
    (magit-section-show section)))

(defun git-appraise-folding--file-hunk-targets (section)
  "Return all persistent hunk targets of file SECTION."
  (let* ((path (git-appraise-folding--file-path section))
         (info (and path (hash-table-p git-appraise-viewed--buffer-inventory)
                    (gethash path git-appraise-viewed--buffer-inventory))))
    (--map (list "hunk" path (plist-get it :fp) (plist-get it :range))
           (plist-get info :hunks))))

(defun git-appraise-folding--set-file-hunks (folded)
  "Set every hunk of the file containing point to FOLDED."
  (let* ((file (git-appraise-folding--file-section (magit-current-section)))
         (targets (and file (git-appraise-folding--file-hunk-targets file))))
    (unless targets (user-error "No hunks in the file at point"))
    (git-appraise-fold-state--write
     (if folded targets
       (cons (git-appraise-folding--file-target file) targets))
     folded)
    (when (git-appraise-lazy-placeholder-section-p file)
      (setq file (git-appraise-lazy--expand-at file)))
    (unless file (user-error "Could not load the file's hunks"))
    (--each (oref file children)
      (when (magit-hunk-section-p it)
        (if folded (magit-section-hide it)
          (magit-section-show it))))
    (unless folded (magit-section-show file))))

(defun git-appraise-fold-hunks-in-file ()
  "Fold all hunks of the file containing point."
  (interactive)
  (git-appraise-folding--set-file-hunks t))

(defun git-appraise-unfold-hunks-in-file ()
  "Unfold all hunks of the file containing point."
  (interactive)
  (git-appraise-folding--set-file-hunks nil))

(defun git-appraise-folding--set-all-files (folded)
  "Set every changed file to FOLDED in one note write."
  (let (files)
    (magit-map-sections
     (lambda (section)
       (when (or (magit-file-section-p section)
                 (git-appraise-lazy-placeholder-section-p section))
         (push section files)))
     magit-root-section)
    (setq files (nreverse files))
    (git-appraise-fold-state--write
     (--keep (git-appraise-folding--file-target it) files) folded)
    (--each files
      (if folded (magit-section-hide it)
        (magit-section-show it)))))

(defun git-appraise-fold-all-files ()
  "Fold every changed file in this review."
  (interactive)
  (git-appraise-folding--set-all-files t))

(defun git-appraise-unfold-all-files ()
  "Unfold every changed file in this review."
  (interactive)
  (git-appraise-folding--set-all-files nil))

(defun git-appraise-folding--commits-section (section)
  "Return the commits group containing SECTION, if any."
  (while (and section
              (not (eq (oref section type)
                       'git-appraise-commits-header-section)))
    (setq section (and (slot-boundp section 'parent)
                       (oref section parent))))
  section)

(defun git-appraise-folding--body-visually-hidden-p (section)
  "Non-nil when SECTION's body is actually hidden by an invisible overlay."
  (let ((content (and (slot-boundp section 'content) (oref section content))))
    (and content
         (cl-some (lambda (ov) (overlay-get ov 'invisible))
                  (overlays-at content)))))

(defun git-appraise-folding--effective-hidden-p (section)
  "Return SECTION's effective hidden state, repairing slot/overlay drift.
When the `hidden' slot disagrees with the actual invisible overlay,
sync the slot to the visual state so toggling behaves as the user sees."
  (let ((visually (git-appraise-folding--body-visually-hidden-p section))
        (slot (oref section hidden)))
    (unless (eq visually slot)
      (oset section hidden visually))
    visually))

(defun git-appraise-fold-toggle-at-point ()
  "Toggle the section at point, persisting file and hunk choices."
  (interactive)
  (let* ((section (magit-current-section))
         (commits (git-appraise-folding--commits-section section)))
    (cond
     (commits
      (magit-section-toggle commits)
      (when (oref commits hidden)
        (goto-char (oref commits start))))
     ((or (magit-file-section-p section)
          (git-appraise-lazy-placeholder-section-p section))
      (git-appraise-folding--set-file
       section (not (git-appraise-folding--effective-hidden-p section))))
     ((magit-hunk-section-p section)
      (git-appraise-folding--set-hunk
       section (not (git-appraise-folding--effective-hidden-p section))))
     (t (magit-section-toggle section)))))

(defvar-local git-appraise-fold-level nil
  "Current global fold level, nil when unset.
1 shows only the top-level container, 2 collapses files, 3
collapses hunks, 4 expands everything.  Managed by
`git-appraise-fold-less'/`git-appraise-fold-more'.")

(defun git-appraise-fold-show-level (level)
  "Set the global fold LEVEL for this review buffer.
The scale is by section KIND, not raw nesting depth, so it is
stable across layouts (the real render wraps files in TWO
containers, files-report and files-chnged, while tests use one):
5 everything visible, 4 threads folded, 3 hunks folded,
2 files folded, 1 only the top-level containers.
Bypasses magit's visibility cache so a fold never gets remembered
across renders."
  (interactive "nCode-review fold level (1 folded ... 5 expanded): ")
  (let ((magit-section-cache-visibility nil))
    (magit-map-sections
     (lambda (s)
       (let ((type (oref s type)))
         (oset s hidden
               ;; a kind is folded AT its fold level and below:
               ;; threads at 4, hunks at 3, files at 2, the
               ;; top-level containers at 1
               (or (and (< level 5)
                        (cl-typep s 'git-appraise-base-comment-section))
                   (and (< level 4) (eq type 'hunk))
                   (and (< level 3) (eq type 'file))
                   (and (< level 2)
                        (not (eq s magit-root-section))
                        (slot-boundp s 'parent)
                        (eq (oref s parent) magit-root-section)))))))
    (magit-section-show magit-root-section)
    (setq git-appraise-fold-level level)))

(defun git-appraise-fold-less ()
  "Fold one level more (\\[git-appraise-fold-less]).
5 = everything visible, 4 = threads folded behind hunks,
3 = files expanded but hunks folded, 2 = files folded, 1 = only
the top-level containers."
  (interactive)
  (git-appraise-fold-show-level (max 1 (1- (or git-appraise-fold-level 5)))))

(defun git-appraise-fold-more ()
  "Expand one level more (\\[git-appraise-fold-more]).
See `git-appraise-fold-less' for the level scale."
  (interactive)
  (git-appraise-fold-show-level (min 5 (1+ (or git-appraise-fold-level 5)))))

(defun git-appraise-fold-file-at-point ()
  "Toggle the file section at or around point.
S-<tab> from inside a hunk folds the WHOLE file: it collapses to
its heading line, a quick way to mark a file as reviewed and move
on; pressing it again expands it, and each hunk keeps its own
fold state.  Works from anywhere inside the file (hunk,
comment thread, heading); with point outside any file it falls
back to magit's global section cycle
(`magit-section-cycle-global').  Unlike the level folds this
keeps magit's visibility cache ON, so a file fold survives
re-renders — that persistence is the point: a reviewed-and-folded
file stays folded after a reload."
  (interactive)
  (let ((file (git-appraise-folding--file-section (magit-current-section))))
    (if file
        (let ((folded (not (git-appraise-folding--effective-hidden-p file))))
          (git-appraise-folding--set-file file folded)
          (when folded (goto-char (oref file start))))
      (magit-section-cycle-global))))

(defun git-appraise-folding--advance-after (section)
  "Move from folded SECTION to the next visible file or hunk heading."
  (goto-char (oref section start))
  (condition-case nil
      (git-appraise-next-item)
    (user-error nil)))

(defun git-appraise-viewed-mark-and-fold ()
  "Toggle the viewed mark at point and keep its fold state in step.
Marking a hunk — or a whole file from its heading — folds it and
moves to the next visible heading; removing the mark reopens it
and leaves point in place.  With an active region, toggle the
viewed state of the selected files only."
  (interactive)
  (if (use-region-p)
      (git-appraise-viewed-toggle)
    (let* ((section (magit-current-section))
           (file (git-appraise-folding--file-section section))
           (hunk (and file (git-appraise-viewed--current-hunk))))
      (unless file
        (user-error "No file or hunk at point to mark viewed"))
      (let ((viewed? (if hunk
                         (nth 3 (git-appraise-viewed--current-hunk-target))
                       (git-appraise-viewed--file-viewed-p file))))
        (if hunk
            (git-appraise-viewed--toggle-hunk)
          (git-appraise-viewed--toggle-file file))
        (if viewed?
            (if hunk
                (git-appraise-folding--set-hunk hunk nil)
              (git-appraise-folding--set-file file nil))
          (if hunk
              (git-appraise-folding--set-hunk hunk t)
            (git-appraise-folding--set-file file t))
          (git-appraise-folding--advance-after (or hunk file)))))))

(defun git-appraise-viewed-mark-file-and-fold ()
  "Toggle the whole-file viewed mark and keep the file's fold in step.
Marking the file folds it and moves to the next visible heading;
removing the mark reopens it and leaves point in place.  With an
active region, toggle the viewed state of the selected files only."
  (interactive)
  (if (use-region-p)
      (git-appraise-viewed-toggle-file)
    (let ((file (git-appraise-folding--file-section
                 (magit-current-section))))
      (unless file
        (user-error "No file at point to mark viewed"))
      (let ((viewed? (git-appraise-viewed--file-viewed-p file)))
        (git-appraise-viewed--toggle-file file)
        (if viewed?
            (git-appraise-folding--set-file file nil)
          (git-appraise-folding--set-file file t)
          (git-appraise-folding--advance-after file))))))

(provide 'git-appraise-folding)
;;; git-appraise-folding.el ends here
