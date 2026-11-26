;;; git-appraise-lazy.el --- Wash the diff lazily, one file block at a time -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2026 Dmitry Moskowski
;;
;; Keywords: git, tools, vc
;; Homepage: https://github.com/corpix/git-appraise.el
;;
;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 30: the diff wash used to be synchronous over the WHOLE raw
;; diff, so opening a large PR blocked Emacs.  The file block is the
;; lazy unit: every block gets a lightweight file section before the
;; render returns, so a draft can attach while hunks are loading.  A
;; deferred queue expands placeholders one file per tick, and TAB
;; expands a requested file immediately.  A failed expansion restores
;; its placeholder instead of leaving a half-washed file.
;;
;; All buffer-locals here are `permanent-local': the deferred render
;; tail re-runs the mode body and `kill-all-local-variables' wipes every
;; non-permanent local mid-cycle (the 2026-10-30 counter bug).

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'deferred)
(require 'magit-section)
(require 'git-appraise-render)
(require 'git-appraise-db)

(declare-function git-appraise-wash-diff "git-appraise-section-wash")
(declare-function git-appraise-wash--insert-file-heading "git-appraise-section-wash")
(declare-function git-appraise-section--fold-and-mark "git-appraise-section")
(declare-function git-appraise-section--fold-hunks "git-appraise-folding")
(declare-function git-appraise-folding--file-path "git-appraise-folding"
                  (section))
(declare-function git-appraise--mark-comment-lines "git-appraise-section")
(declare-function git-appraise-utils--log "git-appraise-utils")

(defcustom git-appraise-lazy-enabled t
  "When non-nil, show file headings first and load their hunks lazily."
  :group 'git-appraise
  :type 'boolean)

(defcustom git-appraise-lazy-threshold 8192
  "Compatibility threshold for the legacy diff washer.
The normal lazy renderer queues every file after creating its heading,
regardless of this value."
  :group 'git-appraise
  :type 'integer)

;; invalidate, never increment: chunks capture the epoch OBJECT and
;; compare with `eq', so a rollback that restores older buffer-locals
;; also invalidates them.
(defvar-local git-appraise-lazy--epoch nil
  "Token naming the current lazy state of this review buffer.")
(put 'git-appraise-lazy--epoch 'permanent-local t)

(defvar-local git-appraise-lazy--review-id nil
  "Review id captured when this buffer's deferred wash was scheduled.")
(put 'git-appraise-lazy--review-id 'permanent-local t)

(defvar-local git-appraise-lazy--queue nil
  "Placeholder sections still to load, in wash order.")
(put 'git-appraise-lazy--queue 'permanent-local t)

(defvar-local git-appraise-lazy--commit-focus? nil
  "Whether the render that built the placeholders was commit-focus.")
(put 'git-appraise-lazy--commit-focus? 'permanent-local t)

(defvar-local git-appraise-lazy--fresh-render? nil
  "Whether the render that queued the wash is the buffer's first.
Captured at schedule time: the deferred drain runs long after the
render's dynamic bindings are gone.  Drives the fresh-render folds
at the drain.")
(put 'git-appraise-lazy--fresh-render? 'permanent-local t)

(defvar-local git-appraise-lazy--pending-position nil
  "Exact line a render parked the cursor on inside a still-loading
file: (PATH LINE-DELTA COLUMN).  Applied by
`git-appraise-lazy--settle-position' when the background loader
expands that file, and only while the cursor still rests on the
parked heading.")
(put 'git-appraise-lazy--pending-position 'permanent-local t)

(defvar-local git-appraise-lazy--just-refetched? nil
  "Captured `git-appraise-section--just-refetched' for the drain.")
(put 'git-appraise-lazy--just-refetched? 'permanent-local t)

(defvar-local git-appraise-lazy--wash-pos nil
  "Marker at the next unwashed diff block, nil when nothing to wash.")
(put 'git-appraise-lazy--wash-pos 'permanent-local t)

(defvar-local git-appraise-lazy--wash-limit nil
  "Marker at the end of the wash region (the files-chnged body).")
(put 'git-appraise-lazy--wash-limit 'permanent-local t)

(defvar-local git-appraise-lazy--insert-parent nil
  "The section the render was inserting into when scheduled.
The deferred ticks create file sections: magit's insertion context
is dynamic, and after the render it is gone — a placeholder created
without it gets a nil parent AND silently REPLACES
`magit-root-section' (magit treats a parent-less insert as a new
root), corrupting the whole section tree.")
(put 'git-appraise-lazy--insert-parent 'permanent-local t)

(defvar-local git-appraise-lazy--end-fixups nil
  "Enclosing sections whose `end' markers the wash must extend.
The render finishes the files-chnged/report/root sections BEFORE
the deferred wash paints the hunks: their end markers (insertion
type nil) stay at the pre-wash position, pointing INTO the painted
content.  The drain moves them to the wash's true end.")
(put 'git-appraise-lazy--end-fixups 'permanent-local t)

(defvar git-appraise-lazy--wash-limit-dyn nil
  "Dynamic (non-buffer-local) wash limit for `--block-end' during a step.")

(defvar git-appraise-section--just-refetched)

(defclass git-appraise-lazy-placeholder-section (magit-section)
  ())

(defun git-appraise-lazy--invalidate ()
  "Drop every pending lazy chunk for the current buffer."
  (setq git-appraise-lazy--epoch (cons 'git-appraise-lazy nil)
        git-appraise-lazy--queue nil)
  ;; the wash stepper shares the epoch: drop its state too, or a
  ;; stale marker keeps a drain loop waiting forever
  (when (markerp git-appraise-lazy--wash-pos)
    (set-marker git-appraise-lazy--wash-pos nil))
  (when (markerp git-appraise-lazy--wash-limit)
    (set-marker git-appraise-lazy--wash-limit nil))
  (setq git-appraise-lazy--wash-pos nil
        git-appraise-lazy--wash-limit nil))

(defun git-appraise-lazy--render-call (origin &rest args)
  "ORIGIN is `git-appraise-render--call'; ARGS its arguments.
A failed render rolls the buffer text AND buffer-locals back, so
pending chunk callbacks would otherwise re-match their captured
epoch against the restored state.  Invalidate before the render
(kills chunks of older renders) and again after a failure (kills
the failed render's own chunks)."
  (git-appraise-lazy--invalidate)
  (let ((ok nil))
    (unwind-protect
        (prog1 (apply origin args)
          (setq ok t))
      (unless ok
        (git-appraise-lazy--invalidate)))))
(advice-add 'git-appraise-render--call :around
            #'git-appraise-lazy--render-call)

(defun git-appraise-lazy--toggle-around (origin section)
  "Load a placeholder SECTION instead of toggling its (empty) body.
ORIGIN is the advised `magit-section-toggle' or `magit-section-show'."
  (if (and git-appraise-lazy-enabled
           (git-appraise-lazy-placeholder-section-p section))
      (let ((git-appraise-db--pullreq-id
             (or git-appraise-lazy--review-id git-appraise-db--pullreq-id)))
        (git-appraise-lazy--expand-at section))
    (funcall origin section)))
(advice-add 'magit-section-toggle :around #'git-appraise-lazy--toggle-around)
(advice-add 'magit-section-show :around #'git-appraise-lazy--toggle-around)

;;; Placeholder construction (the synchronous wash loop replaces
;;; large raw blocks with these)

(defun git-appraise-lazy--block-end ()
  "End position of the `diff --git' block starting at point.
Bounded by `git-appraise-lazy--wash-limit-dyn' when the deferred
stepper runs (the text after the wash region is other sections)."
  (save-excursion
    (forward-line 1)
    (if (re-search-forward "^diff --git " git-appraise-lazy--wash-limit-dyn 'move)
        (line-beginning-position)
      (or git-appraise-lazy--wash-limit-dyn (point-max)))))

(defun git-appraise-lazy--block-status (text)
  "Change status word for one raw diff block TEXT, as the washer prints it."
  (cond
   ((string-match-p "^Binary files" text) "binary")
   ((string-match-p "^new file" text) "new file")
   ((string-match-p "^deleted file" text) "deleted")
   ((string-match-p "^rename from" text) "renamed")
   (t "modified")))

(defun git-appraise-lazy--block-orig (text)
  "Original path of a renamed block TEXT, or nil."
  (when (string-match "^rename from \\(.+\\)$" text)
    (match-string 1 text)))

(defun git-appraise-lazy--insert-placeholder ()
  "Replace the raw block at point with a placeholder file section.
Point ends after the inserted section, ready for the next block."
  (let* ((beg (point))
         (end (git-appraise-lazy--block-end))
         (text (buffer-substring-no-properties beg end))
         (header (car (split-string text "\n" t)))
         (path (or (git-appraise--diff--extract-b-path header) ""))
         (orig (git-appraise-lazy--block-orig text))
         (status (git-appraise-lazy--block-status text))
         (info (and (hash-table-p git-appraise-section--file-classifications)
                    (gethash path
                             git-appraise-section--file-classifications)))
         (display (if (or (not orig) (equal orig path))
                      path
                    (format "%s -> %s" orig path)))
         (spec (list :path path :status status :text text :orig orig)))
    (delete-region beg end)
    (goto-char beg)
    (let ((section
           (magit-insert-section
               (git-appraise-lazy-placeholder-section
                spec (git-appraise-fold-state--file-folded-p
                      path (or git-appraise-fold-files-by-default
                               (git-appraise-section--file-collapse-p
                                path info))))
             (git-appraise-wash--insert-file-heading
              status display path info t)
             (magit-insert-heading)
             (insert ?\n))))
      (push section git-appraise-lazy--queue)
      (git-appraise-section--hide-if-hidden section))))

(defun git-appraise-lazy--wash-all ()
  "Wash the whole diff synchronously (the `git-appraise-lazy-enabled'
nil path): every block inline, large ones through the placeholder
machinery, no deferred ticks in between."
  (let ((git-appraise-lazy--wash-limit-dyn nil))
    (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
      (if (not (looking-at "^diff --git"))
          (error "Code review stopped before the end of the diff")
        (if (and git-appraise-lazy-enabled
                 (> (- (git-appraise-lazy--block-end) (point))
                    git-appraise-lazy-threshold))
            (git-appraise-lazy--insert-placeholder)
          (unless (git-appraise-wash-diff)
            (error "Code review stopped before the end of the diff")))))))

(defun git-appraise-lazy--precheck ()
  "Validate the inserted diff is a sequence of `diff --git' blocks.
Pure motion over the raw text (no washing): milliseconds instead of
the multi-second wash, so a malformed diff still fails the
synchronous render and `git-appraise-render--call' rolls it back.
The wash itself then runs through the deferred stepper."
  (save-excursion
    (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
      (if (not (looking-at "^diff --git"))
          (error "Code review stopped before the end of the diff")
        (goto-char (git-appraise-lazy--block-end))))))

(defun git-appraise-lazy--wash-block ()
  "Wash ONE file block at point: placeholder when large, inline else.
Return non-nil when a block was washed, nil when the raw text is
exhausted."
  (skip-chars-forward " \t\r\n")
  (if (eobp)
      nil
    (if (not (looking-at "^diff --git"))
        (error "Code review stopped before the end of the diff")
      (if (and git-appraise-lazy-enabled
               (> (- (git-appraise-lazy--block-end) (point))
                  git-appraise-lazy-threshold))
          (progn (git-appraise-lazy--insert-placeholder) t)
        (unless (git-appraise-wash-diff)
          (error "Code review stopped before the end of the diff"))
        t))))

(defun git-appraise-lazy--wash-step (buffer epoch)
  "Wash one file block in BUFFER per tick while EPOCH is current.
When the raw text is exhausted, hand over to the placeholder
expansion phase."
  (when (and (buffer-live-p buffer)
             (eq epoch (buffer-local-value 'git-appraise-lazy--epoch buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (git-appraise-db--pullreq-id
             (or git-appraise-lazy--review-id git-appraise-db--pullreq-id))
            (git-appraise-lazy--wash-limit-dyn git-appraise-lazy--wash-limit)
            done err)
        (if (null git-appraise-lazy--wash-pos)
            ;; wash phase already drained
            (git-appraise-lazy--expand-step buffer epoch)
          (save-excursion
            (save-restriction
              (narrow-to-region (point-min) git-appraise-lazy--wash-limit)
              (goto-char git-appraise-lazy--wash-pos)
              ;; re-install the render's magit insertion context: the
              ;; sections created here must become children of the
              ;; files-chnged section, not a new root
              (let ((magit-insert-section--parent git-appraise-lazy--insert-parent)
                    (magit-insert-section--current git-appraise-lazy--insert-parent)
                    (magit-insert-section--oldroot magit-root-section))
                (setq done
                      (condition-case e
                          (git-appraise-lazy--wash-block)
                        (error (setq err (error-message-string e)) nil)))
                (when done
                  (set-marker git-appraise-lazy--wash-pos (point))))))
          (cond
           (err
            (git-appraise-lazy--invalidate)
            (message "Code review lazy wash failed: %s" err))
           ((not done)
            (setq git-appraise-lazy--wash-pos nil)
            (set-marker git-appraise-lazy--wash-limit nil)
            ;; hand over to the expansion phase on the NEXT tick (not
            ;; inline: a test or observer must be able to freeze the
            ;; buffer between the wash and the expansion phases)
            (deferred:nextc (deferred:next)
              (lambda (_)
                (git-appraise-lazy--expand-step buffer epoch))))
           (t
            (deferred:nextc (deferred:next)
              (lambda (_)
                (git-appraise-lazy--wash-step buffer epoch))))))))))

;;; Restoring the cursor across a render

(defun git-appraise-lazy--position-chain (pos)
  "Describe POS as logical anchors, innermost section first.
Every element is (IDENT LINE-DELTA COLUMN PATH): IDENT identifies
the section in a later render, LINE-DELTA and COLUMN locate POS
within it, and PATH is the enclosing file's path at the file
level.  Stops at the first ancestor whose start marker no longer
belongs to this buffer: a stale tree has no restorable position."
  (save-excursion
    (goto-char pos)
    (let ((section (magit-current-section))
          (line (line-number-at-pos pos))
          (column (- pos (line-beginning-position)))
          chain)
      (while section
        (let ((start (oref section start)))
          (if (not (and (markerp start)
                        (eq (marker-buffer start) (current-buffer))))
              (setq section nil)
            (push (list (magit-section-ident section)
                        (- line (line-number-at-pos start))
                        column
                        (and (magit-file-section-p section)
                             (git-appraise-folding--file-path section)))
                  chain)
            (setq section (and (slot-boundp section 'parent)
                               (oref section parent))))))
      (nreverse chain))))

(defun git-appraise-lazy--exact-section (ident)
  "The section IDENT identifies in this tree, or nil.
`magit-get-section' alone also returns a matching ANCESTOR once
the leaf is gone; the cursor must not mistake that for the leaf."
  (let ((section (magit-get-section ident)))
    (and section (equal (magit-section-ident section) ident) section)))

(defun git-appraise-lazy--section-by-path (path)
  "The file or placeholder section for PATH in this buffer, or nil."
  (let (found)
    (magit-map-sections
     (lambda (s)
       (unless found
         (when (and (or (magit-file-section-p s)
                        (git-appraise-lazy-placeholder-section-p s))
                    (equal (git-appraise-folding--file-path s) path))
           (setq found s))))
     magit-root-section)
    found))

(defun git-appraise-lazy--goto-entry (section entry)
  "Move to SECTION's start plus ENTRY's line delta and column."
  (goto-char (oref section start))
  (forward-line (min (nth 1 entry)
                     (1- (max 1 (count-lines (oref section start)
                                             (oref section end))))))
  (forward-char (min (nth 2 entry)
                     (max 0 (1- (- (line-end-position)
                                   (line-beginning-position)))))))

(defun git-appraise-lazy--restore-position (chain)
  "Move point to CHAIN's position in the freshly rendered tree.
The innermost surviving section wins; a file that is still loading
parks the cursor on its heading and remembers the exact line for
when the background loader expands it.  Nothing surviving leaves
point at the top."
  (setq git-appraise-lazy--pending-position nil)
  (catch 'git-appraise-lazy--restored
    (--each chain
      (let ((section (git-appraise-lazy--exact-section (nth 0 it))))
        (if (nth 3 it)
            ;; the file level: by ident, else by path (loaded or not)
            (cond (section
                   (git-appraise-lazy--goto-entry section it)
                   (throw 'git-appraise-lazy--restored t))
                  (t
                   (let ((file (git-appraise-lazy--section-by-path
                                (nth 3 it))))
                     (when file
                       (if (git-appraise-lazy-placeholder-section-p file)
                           (progn
                             (goto-char (oref file start))
                             (setq git-appraise-lazy--pending-position
                                   (list (nth 3 it) (nth 1 it) (nth 2 it))))
                         (git-appraise-lazy--goto-entry file it))
                       (throw 'git-appraise-lazy--restored t)))))
          (when section
            (git-appraise-lazy--goto-entry section it)
            (throw 'git-appraise-lazy--restored t)))))
    (goto-char (point-min))))

(defun git-appraise-lazy--settle-position (section)
  "Refine a parked cursor now that SECTION's file finished loading.
Applies only while the cursor still rests on the heading the render
parked it on: the reviewer's own navigation wins."
  (when (and git-appraise-lazy--pending-position
             (equal (git-appraise-folding--file-path section)
                    (nth 0 git-appraise-lazy--pending-position))
             (= (point) (oref section start)))
    (git-appraise-lazy--goto-entry
     section
     (list nil (nth 1 git-appraise-lazy--pending-position)
           (nth 2 git-appraise-lazy--pending-position)))
    (setq git-appraise-lazy--pending-position nil)))

;;; Loading

(defun git-appraise-lazy--expand-at (section)
  "Replace the placeholder SECTION with its fully washed file section.
A failed wash rolls the placeholder back (text, properties and tree
slot) and returns nil.  This is deliberately NOT
`git-appraise-render--call': that swaps the whole buffer text to a
backup, which would re-point the placeholder's markers into the
backup buffer — it is a full-rebuild tool, not a partial-edit tool.
Point at or after the block's end stays with the text that follows:
an ordinary `save-excursion' would park it before the inserted block
instead, dragging the cursor onto the loaded file's heading."
  (let* ((spec (oref section value))
         (parent (oref section parent))
         (text (plist-get spec :text))
         (inhibit-read-only t)
         (new-section nil)
         (parked (copy-marker (point) (>= (point) (oref section end)))))
    (unwind-protect
        (let* ((beg (progn (goto-char (oref section start)) (point-marker)))
               (heading (buffer-substring (oref section start)
                                          (oref section end)))
               (old-children (oref parent children)))
          (oset parent children (delq section (oref parent children)))
          (delete-region beg (oref section end))
          (insert text)
          (let* ((end (copy-marker (point)))
                 (magit-insert-section--parent parent))
            (goto-char beg)
            (save-restriction
              (narrow-to-region beg end)
              (if (condition-case err
                      (progn
                        (git-appraise-render--wash-sequence
                         #'git-appraise-wash-diff)
                        (skip-chars-forward " \t\r\n")
                        (unless (eobp)
                          (error "Code review stopped before the end of the diff"))
                        t)
                    (error
                     ;; roll the placeholder back: drop everything the
                     ;; wash inserted, restore its heading and its slot
                     (delete-region beg end)
                     (insert heading)
                     (oset parent children old-children)
                     (message "Code review lazy load failed: %s"
                              (error-message-string err))
                     nil))
                  (setq new-section (car (last (oref parent children))))))))
      (goto-char parked)
      (set-marker parked nil))
    (when (and new-section
               (not (git-appraise-lazy-placeholder-section-p new-section)))
      (git-appraise-section--fold-hunks new-section nil)
      (when git-appraise-comment-fringe-markers
        (git-appraise--mark-comment-lines)))
    (git-appraise-lazy--settle-position new-section)
    new-section))

(defun git-appraise-lazy--load-next ()
  "Load the first loadable placeholder from the queue.
Return non-nil when a file was loaded."
  (let ((inhibit-read-only t)
        (loaded nil))
    (while (and git-appraise-lazy--queue (not loaded))
      (let ((section (pop git-appraise-lazy--queue)))
        (setq loaded
              (and (object-of-class-p section 'magit-section)
                   (markerp (oref section start))
                   (marker-buffer (oref section start))
                   (git-appraise-lazy--expand-at section)))))
    loaded))

(defun git-appraise-lazy--expand-step (buffer epoch)
  "Expand queued placeholders in BUFFER, one per tick, while EPOCH
is current.  When the queue drains, run the whole-buffer visibility
passes (bot folds, viewed folds, fringe markers) that the render
could not apply: the hunks did not exist yet at render time.  Never
moves point: `git-appraise-lazy--expand-at' parks the cursor with
the text it inserts, and where the cursor rests belongs to the
render or the reviewer."
  (when (and (buffer-live-p buffer)
             (eq epoch (buffer-local-value 'git-appraise-lazy--epoch buffer)))
    (with-current-buffer buffer
      (let ((git-appraise-db--pullreq-id
             (or git-appraise-lazy--review-id git-appraise-db--pullreq-id)))
        (if (git-appraise-lazy--load-next)
            (deferred:nextc (deferred:next)
                            (lambda (_)
                              (git-appraise-lazy--expand-step buffer epoch)))
          (condition-case err
              (let ((inhibit-read-only t)
                    ;; the render's dynamic binding is long gone; the
                    ;; drain re-installs the captured value so
                    ;; fold-and-mark sees the render-time flags
                    (git-appraise-section--just-refetched
                     git-appraise-lazy--just-refetched?))
                (dolist (sec git-appraise-lazy--end-fixups)
                  (when (and (object-of-class-p sec 'magit-section)
                             (slot-boundp sec 'end)
                             (marker-buffer (oref sec end)))
                    (set-marker (oref sec end) (point-max))))
                (when (and (boundp 'magit-root-section) magit-root-section)
                  (git-appraise-section--fold-and-mark
                   git-appraise-lazy--fresh-render?
                   git-appraise-lazy--commit-focus?)))
            (error
             (git-appraise-utils--log
              "lazy-drain" (error-message-string err)))))))))

(defun git-appraise-lazy--schedule (commit-focus? fresh-render?)
  "Create file placeholders, then queue their deferred expansion.
FRESH-RENDER? is the render's own first-render flag.  Materializing
all file headings before returning gives comments a stable target
even while the hunks are still loading."
  (setq git-appraise-lazy--commit-focus? commit-focus?
        git-appraise-lazy--review-id git-appraise-db--pullreq-id
        git-appraise-lazy--fresh-render? fresh-render?
        git-appraise-lazy--just-refetched?
        (and (boundp 'git-appraise-section--just-refetched)
             git-appraise-section--just-refetched)
        git-appraise-lazy--wash-pos nil
        git-appraise-lazy--wash-limit nil
        git-appraise-lazy--insert-parent magit-insert-section--parent
        git-appraise-lazy--end-fixups
        (let ((secs nil)
              (sec magit-insert-section--parent))
          (while sec
            (push sec secs)
            (setq sec (and (object-of-class-p sec 'magit-section)
                           (ignore-errors (oref sec parent)))))
          secs))
  (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
    (unless (looking-at "^diff --git")
      (error "Code review stopped before the end of the diff"))
    (git-appraise-lazy--insert-placeholder))
  (setq git-appraise-lazy--queue (nreverse git-appraise-lazy--queue))
  (let ((buffer (current-buffer))
        (epoch git-appraise-lazy--epoch))
    (deferred:nextc (deferred:next)
      (lambda (_)
        (git-appraise-lazy--expand-step buffer epoch)))))

(provide 'git-appraise-lazy)
;;; git-appraise-lazy.el ends here
