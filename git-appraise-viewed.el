;;; git-appraise-viewed.el --- persistent viewed state for review diffs -*- lexical-binding: t; -*-

;; Viewed marks for files and hunks (2026-11-24 viewed-state plan),
;; backed by the personal SQLite store (`git-appraise-store.el').
;;
;; Identity layer: every hunk gets a content fingerprint (canonical
;; path + hunk body with the @@ range numbers normalized away), every
;; hunk-less diff block (binary, mode, rename) gets a fingerprint of
;; its whole block.  A mark survives line shifts and dies with the
;; content it marked.  The inventory and the owned diff washer go
;; through the same parse/serialize helpers, so they can never
;; disagree about identities.
;;
;; Projection layer: reduces the stored events (last write wins
;; per reviewer, item and fingerprint) onto the current diff's
;; inventory.  Everything this file computes is a cache of the
;; store, never the authoritative copy.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'a)
(require 'dash)
(require 'magit-section)
(require 'uuidgen)
(require 'git-appraise-store)
(require 'git-appraise-viewed-core)
(require 'git-appraise-time)
(require 'git-appraise-utils)

(declare-function git-appraise--diff--split-by-files "git-appraise-diff")
(defvar git-appraise-viewed-indicator)
(defvar git-appraise-section--changed-tag)
(defvar git-appraise-show-file-change-summary)

;;; Reviewer identity

(defun git-appraise-viewed--actor (repo)
  "The reviewer identity for REPO: its configured author email.
Nil when unset -- marks are then not loaded or written, rather
than mixing every user under an anonymous ID."
  (let ((email (git-appraise-utils--git repo "config" "user.email")))
    (and email (not (string-empty-p email)) email)))

;;; Load boundary and render integration

(defvar-local git-appraise-viewed--buffer-inventory nil
  "This render's identity inventory (`git-appraise-viewed--inventory').
Same buffer lifetime as `git-appraise-section--viewed'.")
(put 'git-appraise-viewed--buffer-inventory 'permanent-local t)

(defvar git-appraise-section--viewed)
(defvar git-appraise-repo-worktree)
(declare-function git-appraise-db--pullreq-raw-diff "git-appraise-db")
(declare-function git-appraise-db-get-pullreq "git-appraise-db")
(declare-function git-appraise-diff--mark-viewed-files "git-appraise-diff")

(defun git-appraise-viewed--repo-dir ()
  "The reviewed repository worktree for the current review, or nil."
  (or (bound-and-true-p git-appraise-repo-worktree)
      (ignore-errors
        (let ((dir (oref (git-appraise-db-get-pullreq) host)))
          (and dir (file-name-as-directory dir))))))

(defun git-appraise-viewed--revision ()
  "The review revision the marks are anchored on, or nil."
  (ignore-errors (oref (git-appraise-db-get-pullreq) number)))

(defun git-appraise-viewed--load (repo revision)
  "The stored viewed events for REPO and REVISION.
A repository without state, or a store failure, is no events: a
broken store must never masquerade as a provider error."
  (or (and repo revision
           (condition-case err
               (git-appraise-store-events repo revision 'viewed)
             (error
              (git-appraise-utils--log
               "git-appraise-viewed--load" (prin1-to-string err))
              (message "Viewed marks unavailable (%s); showing all unviewed"
                       (error-message-string err))
              nil)))
      nil))

(defun git-appraise-viewed--render-prepare (classifications)
  "Project the loaded viewed events onto the current diff.
CLASSIFICATIONS is the file-classification hash built by
`git-appraise-section--insert-pr-text'.  Sets this render's
buffer-local index (`git-appraise-section--viewed') and inventory,
then flags the classifications: the same hash feeds the file
washer and the lazy placeholders, so full and delayed washes see
the same marks."
  (let* ((repo (git-appraise-viewed--repo-dir))
         (revision (git-appraise-viewed--revision)))
    (setq git-appraise-viewed--buffer-inventory
          (git-appraise-viewed--inventory
           (git-appraise-db--pullreq-raw-diff)))
    (setq git-appraise-section--viewed
          (git-appraise-viewed--project
           (if (and repo revision)
               (git-appraise-viewed--load repo revision)
             nil)
           (and repo (git-appraise-viewed--actor repo))
           git-appraise-viewed--buffer-inventory))
    (git-appraise-diff--mark-viewed-files
     classifications git-appraise-section--viewed)))

(defun git-appraise-viewed--hunk-viewed-p (path ranges)
  "Whether the hunk of PATH with parsed RANGES pairs is viewed.
RANGES is the washer's number pairs; the canonical range key is
the same helper the inventory used, so the lookup can never
disagree with the projection."
  (and git-appraise-section--viewed
       git-appraise-viewed--buffer-inventory
       (gethash path git-appraise-viewed--buffer-inventory)
       (member (git-appraise-viewed--range-key ranges)
               (plist-get (gethash path git-appraise-section--viewed)
                          :hunk-viewed))))

(defun git-appraise-viewed--counter-string ()
  "The N/M viewed piece for the Files changed heading, or nil.
Hunks and hunk-less files are the countable items; a file with
hunks is not counted separately (its mark is the rollup)."
  (when (hash-table-p git-appraise-section--viewed)
    (let ((viewed 0)
          (total 0))
      (maphash (lambda (_ info)
                 (setq viewed (+ viewed (or (plist-get info :viewed-ct) 0))
                       total (+ total (or (plist-get info :total) 0))))
               git-appraise-section--viewed)
      (when (> total 0)
        (format git-appraise-section--viewed-counter-format viewed total)))))

;;; Actions

(declare-function git-appraise--trigger-hooks "git-appraise-section"
                  (buff-name &optional commit-focus? msg))
(declare-function git-appraise-eldoc--file-section "git-appraise-folding"
                  (section))
(declare-function git-appraise-wash-hunk--ranges "git-appraise-section-wash"
                  (raw-ranges))

(defun git-appraise-viewed--current-hunk ()
  "The nearest magit hunk section at or above point, or nil."
  (let ((sec (magit-current-section)))
    (while (and sec (not (magit-hunk-section-p sec)))
      (setq sec (and (slot-boundp sec 'parent) (oref sec parent))))
    sec))

(defun git-appraise-viewed--file-path (file)
  "The canonical path of the file section FILE, or nil.
The heading text carries `git-appraise-path'; the section start
marker sits on the blank line before the heading, so the next
line is scanned as a fallback."
  (or (get-text-property (oref file start) 'git-appraise-path)
      (get-text-property (+ (oref file start) 1) 'git-appraise-path)
      (save-excursion
        (goto-char (oref file start))
        (forward-line 1)
        (get-text-property (point) 'git-appraise-path))))

(defun git-appraise-viewed--current-hunk-target ()
  "The (PATH FP RANGE VIEWED?) identity of the hunk at point, or nil."
  (let* ((sec (git-appraise-viewed--current-hunk))
         (spec (and sec (oref sec value)))
         (path (and spec (a-get spec 'path)))
         (entry (and path
                     git-appraise-viewed--buffer-inventory
                     (gethash path git-appraise-viewed--buffer-inventory))))
    (when (and path entry)
      (let* ((ranges (git-appraise-viewed--parse-ranges (a-get spec 'ranges)))
             (key (git-appraise-viewed--range-key ranges))
             (h (--first (equal (plist-get it :range) key)
                         (plist-get entry :hunks))))
        (when h
          (list path
                (plist-get h :fp)
                key
                (and git-appraise-section--viewed
                     (member key
                             (plist-get (gethash path
                                                   git-appraise-section--viewed)
                                        :hunk-viewed)))))))))

(defun git-appraise-viewed--file-target (file)
  "The (PATH ITEMS NO-HUNK?) identity of the file section FILE.
ITEMS is one (FP . RANGE) per current hunk; a hunk-less file has
a single (FP) item and NO-HUNK? t.  Nil when the file has no
identity in the current diff inventory."
  (let* ((path (git-appraise-viewed--file-path file))
         (entry (and path
                     git-appraise-viewed--buffer-inventory
                     (gethash path git-appraise-viewed--buffer-inventory))))
    (when entry
      (if (plist-get entry :has-hunks)
          (list path
                (--map (cons (plist-get it :fp) (plist-get it :range))
                       (plist-get entry :hunks))
                nil)
        (list path (list (cons (plist-get entry :file-fp) nil)) t)))))

(defun git-appraise-viewed--make-event (actor path fp range viewed)
  "One valid viewed EVENT: a hunk record when RANGE is set,
otherwise a file record.  The timestamp carries microseconds:
last-write-wins compares `at' lexicographically, so a fast
mark/unmark within one second must still order deterministically
(the ID tie-break is random)."
  `((v . 1)
    (type . "viewed")
    (id . ,(uuidgen-4))
    (actor . ,actor)
    (item . ,(if range "hunk" "file"))
    (path . ,path)
    (fingerprint . ,fp)
    ,@(when range `((range . ,range)))
    (viewed . ,(and viewed t))
    (at . ,(git-appraise-time-now-utc t))))

(defun git-appraise-viewed--reproject ()
  "Recompute viewed projection and mark classifications in place."
  (let* ((repo (git-appraise-viewed--repo-dir))
         (revision (git-appraise-viewed--revision))
         (classifications git-appraise-section--file-classifications))
    (setq git-appraise-section--viewed
          (git-appraise-viewed--project
           (if (and repo revision)
               (git-appraise-viewed--load repo revision)
             nil)
           (and repo (git-appraise-viewed--actor repo))
           git-appraise-viewed--buffer-inventory))
    (when (hash-table-p classifications)
      (maphash (lambda (path info)
                 (when (plist-member info :viewed)
                   (puthash path (plist-put info :viewed nil) classifications))
                 (when (plist-member info :changed)
                   (puthash path (plist-put info :changed nil) classifications)))
               classifications)
      (git-appraise-diff--mark-viewed-files
       classifications git-appraise-section--viewed))))

(defun git-appraise-viewed--update-counter-in-place ()
  "Update the N/M viewed counter line in the current buffer."
  (let ((counter (git-appraise-viewed--counter-string))
        (inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^ *[0-9]+/[0-9]+ viewed$" nil t)
        (let ((bol (line-beginning-position))
              (eol (line-end-position)))
          (if counter
              (let ((props (text-properties-at bol)))
                (delete-region bol eol)
                (goto-char bol)
                (insert "  " counter)
                (add-text-properties bol (point) props))
            (delete-region bol (min (1+ eol) (point-max)))))))))

(defun git-appraise-viewed--apply-file-heading-in-place (section path)
  "Update file SECTION heading for PATH to reflect viewed state."
  (let* ((info (and git-appraise-section--file-classifications
                    (gethash path git-appraise-section--file-classifications)))
         (viewed? (plist-get info :viewed))
         (face (if viewed? 'git-appraise-dimmed 'magit-diff-file-heading))
         (inhibit-read-only t))
    (save-excursion
      (goto-char (oref section start))
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        ;; reface only heading content carrying git-appraise-path.
        (let ((p bol))
          (while (< p eol)
            (let ((f (get-text-property p 'font-lock-face))
                  (next (next-single-property-change p 'font-lock-face nil eol)))
              (when (and (get-text-property p 'git-appraise-path)
                         (memq f '(magit-diff-file-heading git-appraise-dimmed)))
                (put-text-property p next 'font-lock-face face))
              (setq p next))))
        ;; Find the column by its own property, not a byte offset into
        ;; the heading: an offset can delete the start of a file path.
        (let* ((viewed-col (text-property-any bol eol
                                              'git-appraise-viewed-column t))
               (viewed-str (if (and git-appraise-viewed-indicator viewed?)
                               git-appraise-viewed-indicator
                             "")))
          (when (and viewed-col (<= (+ viewed-col 3) (line-end-position)))
            (let ((sec-prop (get-text-property viewed-col 'magit-section))
                  (path-prop (get-text-property viewed-col 'git-appraise-path)))
              (delete-region viewed-col (+ viewed-col 3))
              (goto-char viewed-col)
              (insert (propertize (format "%-3s" viewed-str)
                                  'font-lock-face 'git-appraise-dimmed
                                  'git-appraise-viewed-column t))
              (when sec-prop
                (put-text-property viewed-col (+ viewed-col 3) 'magit-section sec-prop))
              (when path-prop
                (put-text-property viewed-col (+ viewed-col 3) 'git-appraise-path path-prop)))))
        ;; rebuild CHANGED trailing tag.
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (changed-match (string-match "  \\[CHANGED\\]" line)))
          (when changed-match
            (delete-region (+ (line-beginning-position) (match-beginning 0))
                           (+ (line-beginning-position) (match-end 0))))
          (when (plist-get info :changed)
            (goto-char (line-end-position))
            (let ((sec-prop (get-text-property (line-beginning-position) 'magit-section))
                  (tag-start (point)))
              (insert (propertize git-appraise-section--changed-tag
                                  'font-lock-face 'git-appraise-dimmed))
              (when sec-prop
                (put-text-property tag-start (point) 'magit-section sec-prop)))))))))

(defun git-appraise-viewed--apply-hunk-heading-in-place (section path)
  "Update hunk SECTION heading for PATH to reflect viewed state."
  (let ((raw-ranges (and (listp (oref section value))
                         (consp (car-safe (oref section value)))
                         (a-get (oref section value) 'ranges))))
    (when raw-ranges
      (let* ((ranges (git-appraise-wash-hunk--ranges raw-ranges))
             (viewed? (git-appraise-viewed--hunk-viewed-p path ranges))
             (face (if viewed? 'git-appraise-dimmed 'magit-diff-hunk-heading))
             (inhibit-read-only t))
        (save-excursion
          (goto-char (oref section start))
          (let ((eol (line-end-position))
                (p (line-beginning-position)))
            (while (< p eol)
              (let ((f (get-text-property p 'font-lock-face))
                    (next (next-single-property-change p 'font-lock-face nil eol)))
                (when (memq f '(magit-diff-hunk-heading git-appraise-dimmed))
                  (put-text-property p next 'font-lock-face face))
                (setq p next)))))))))

(defun git-appraise-viewed--apply-in-place ()
  "Update viewed visuals in place without a full buffer rebuild."
  (let ((inhibit-read-only t))
    (git-appraise-viewed--update-counter-in-place)
    (magit-map-sections
     (lambda (sec)
       (cond
        ((or (magit-file-section-p sec)
             (git-appraise-lazy-placeholder-section-p sec))
         (when-let* ((path (git-appraise-viewed--file-path sec)))
           (git-appraise-viewed--apply-file-heading-in-place sec path)))
        ((magit-hunk-section-p sec)
         (let* ((file (oref sec parent))
                (path (and (magit-file-section-p file)
                           (git-appraise-viewed--file-path file))))
           (when path
             (git-appraise-viewed--apply-hunk-heading-in-place sec path)))))))
    (when (and (fboundp 'git-appraise-viewed-changes--prepare)
               (fboundp 'git-appraise-viewed-changes--paint))
      (git-appraise-viewed-changes--prepare
       (git-appraise-db--pullreq-raw-diff))
      (git-appraise-viewed-changes--paint))))

(defun git-appraise-viewed--completion-snapshot (path items viewed actor)
  "Return a snapshot row when marking ITEMS completes viewing PATH."
  (when viewed
    (let* ((entry (and (hash-table-p git-appraise-section--viewed)
                       (gethash path git-appraise-section--viewed)))
           (seen (copy-sequence (plist-get entry :hunk-viewed)))
           (total (plist-get entry :total)))
      (--each items
        (when (cdr it)
          (cl-pushnew (cdr it) seen :test #'equal)))
      (when (or (and (equal total 1) (null (cdr (car items))))
                (and total (= (length seen) total)))
        (when-let* ((block (cdr (assoc path
                                       (git-appraise--diff--split-by-files
                                        (git-appraise-db--pullreq-raw-diff))))))
          (list actor path block))))))

(defun git-appraise-viewed--write-events (path items viewed)
  "Store one event per (FP . RANGE) in ITEMS for PATH, then update UI.
The store is updated only through a committed transaction; a
failed write leaves the rendered state unchanged and reports the
error."
  (let* ((repo (git-appraise-viewed--repo-dir))
         (revision (git-appraise-viewed--revision))
         (actor (and repo (git-appraise-viewed--actor repo))))
    (cond ((not (and repo revision))
           (user-error "No review repository context to store viewed marks"))
          ((not actor)
           (user-error "Set git user.email in %s to store viewed marks" repo))
          (t
           (condition-case err
               (git-appraise-store-append-events
                repo revision 'viewed
                (--map (git-appraise-viewed--make-event
                        actor path (car it) (cdr it) viewed)
                       items)
                (when-let* ((snapshot
                             (git-appraise-viewed--completion-snapshot
                              path items viewed actor)))
                  (list snapshot)))
             (error (user-error "Could not save the viewed mark: %s"
                                (error-message-string err))))
           (git-appraise-viewed--reproject)
           (git-appraise-viewed--apply-in-place)))))

(defun git-appraise-viewed--region-file-sections ()
  "File sections in the active region, deduped in buffer order."
  (when (use-region-p)
    (let (files seen)
      (dolist (sec (magit-region-sections))
        (let ((f (git-appraise-eldoc--file-section sec)))
          (when (and f (not (memq f seen)))
            (push f seen)
            (push f files))))
      (nreverse files))))

(defun git-appraise-viewed--file-viewed-p (file)
  "Return non-nil when FILE is fully viewed."
  (pcase (git-appraise-viewed--file-target file)
    (`(,path ,items ,no-hunk?)
     (let ((idx (and git-appraise-section--viewed
                     (gethash path git-appraise-section--viewed))))
       (if no-hunk?
           (plist-get idx :file)
         (--all? (member (cdr it) (plist-get idx :hunk-viewed))
                 items))))
    (_ nil)))

(defun git-appraise-viewed--write-bulk-events (groups)
  "Write GROUPS of (PATH ITEMS VIEWED) in one store transaction.
GROUPS with nil ITEMS are ignored; the projection and the visible
buffer update only after the commit."
  (let* ((groups (--filter (nth 1 it) groups))
         (repo (git-appraise-viewed--repo-dir))
         (revision (git-appraise-viewed--revision))
         (actor (and repo (git-appraise-viewed--actor repo))))
    (cond
     ((null groups) nil)
     ((not (and repo revision))
      (user-error "No review repository context to store viewed marks"))
     ((not actor)
      (user-error "Set git user.email in %s to store viewed marks" repo))
     (t
      (let (events snapshots)
        (dolist (group groups)
          (pcase-let ((`(,path ,items ,viewed) group))
            (when-let* ((snapshot (git-appraise-viewed--completion-snapshot
                                   path items viewed actor)))
              (push snapshot snapshots))
            (dolist (item items)
              (push (git-appraise-viewed--make-event
                     actor path (car item) (cdr item) viewed)
                    events))))
        (condition-case err
            (git-appraise-store-append-events
             repo revision 'viewed (nreverse events) (nreverse snapshots))
          (error (user-error "Could not save the viewed mark: %s"
                             (error-message-string err))))
        (git-appraise-viewed--reproject)
        (git-appraise-viewed--apply-in-place))))))

(defun git-appraise-viewed--toggle-files (files)
  "Toggle viewed state for FILES as a group.
If every file is fully viewed, unmark all; otherwise mark all
missing items in every file."
  (let* ((targets (--keep (git-appraise-viewed--file-target it) files))
         (all-viewed? (and targets (--all? (git-appraise-viewed--file-viewed-p it) files))))
    (unless targets
      (user-error "No file identities in selection to mark viewed"))
    (let (groups)
      (dolist (target targets)
        (pcase-let ((`(,path ,items ,no-hunk?) target))
          (let* ((idx (and git-appraise-section--viewed
                           (gethash path git-appraise-section--viewed)))
                 (items-to-write
                  (cond
                   (all-viewed? items)
                   (no-hunk?
                    (unless (plist-get idx :file) items))
                   (t (--filter (not (member (cdr it)
                                             (plist-get idx :hunk-viewed)))
                                items)))))
            (when items-to-write
              (push (list path items-to-write (not all-viewed?)) groups)))))
      (git-appraise-viewed--write-bulk-events (nreverse groups)))))

(defun git-appraise-viewed--toggle-hunk ()
  "Toggle the viewed mark of the hunk at point."
  (pcase (git-appraise-viewed--current-hunk-target)
    (`(,path ,fp ,range ,viewed?)
     (git-appraise-viewed--write-events path (list (cons fp range))
                                        (not viewed?)))
    (_ (user-error "No current hunk identity at point to mark viewed"))))

(defun git-appraise-viewed--toggle-file (file)
  "Toggle the whole file section FILE: a file with hunks applies
the action to every current hunk in one batch; a hunk-less file
gets its own mark.  A fully viewed file is unmarked, an incomplete
or unviewed file is marked (only the missing items are written)."
  (pcase (git-appraise-viewed--file-target file)
    (`(,path ,items ,no-hunk?)
     (let* ((idx (and git-appraise-section--viewed
                      (gethash path git-appraise-section--viewed)))
            (viewed? (if no-hunk?
                         (plist-get idx :file)
                       (--all? (member (cdr it)
                                       (plist-get idx :hunk-viewed))
                               items)))
            (targets (if (or viewed? no-hunk?)
                         items
                       (--filter (not (member (cdr it)
                                              (plist-get idx :hunk-viewed)))
                                 items))))
       (git-appraise-viewed--write-events path targets (not viewed?))))
    (_ (user-error "No current file identity at point to mark viewed"))))

(defun git-appraise-viewed-toggle ()
  "Toggle the viewed mark at point: the hunk under the cursor, or
the whole file when point is on a file heading.
With an active region, toggle viewed state for every selected file
as a group: unmark all when every selected file is fully viewed,
otherwise mark all selected files viewed."
  (interactive)
  (if (use-region-p)
      (let ((region-files (git-appraise-viewed--region-file-sections)))
        (if region-files
            (progn
              (git-appraise-viewed--toggle-files region-files)
              (deactivate-mark))
          (user-error "No file sections in region to mark viewed")))
    (let* ((sec (magit-current-section))
           (file (and sec (git-appraise-eldoc--file-section sec))))
      (cond
       ((and file (eq sec file))
        (git-appraise-viewed--toggle-file file))
       ((git-appraise-viewed--current-hunk)
        (git-appraise-viewed--toggle-hunk))
       (file (git-appraise-viewed--toggle-file file))
       (t (user-error "No file or hunk at point to mark viewed"))))))

(defun git-appraise-viewed-toggle-file ()
  "Toggle the viewed mark for the whole file at or around point.
With an active region, toggle viewed state for every selected file
as a group."
  (interactive)
  (if (use-region-p)
      (let ((region-files (git-appraise-viewed--region-file-sections)))
        (if region-files
            (progn
              (git-appraise-viewed--toggle-files region-files)
              (deactivate-mark))
          (user-error "No file sections in region to mark viewed")))
    (if-let* ((file (git-appraise-eldoc--file-section (magit-current-section))))
        (git-appraise-viewed--toggle-file file)
      (user-error "No file at point to mark viewed"))))

(provide 'git-appraise-viewed)
;;; git-appraise-viewed.el ends here
