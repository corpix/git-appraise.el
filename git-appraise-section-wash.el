;;; git-appraise-section-wash.el --- the owned diff wash (phase 11b) and diff report -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski
;;
;; Keywords: git, tools, vc
;; Homepage: https://github.com/corpix/git-appraise.el
;;
;; Split from git-appraise-section.el (phase 15c):
;; git-appraise-section.el, moved verbatim by the split; the fork and

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; the owned diff wash (phase 11b) and diff report (part of the git-appraise section rendering, split from
;; git-appraise-section.el in phase 15c).

;;; Code:

(require 'git-appraise-render)

(require 'git-appraise-section-shared)
(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'a)
(require 'git-appraise-faces)
(require 'git-appraise-db)
(require 'git-appraise-utils)
(require 'git-appraise-diff)
(require 'git-appraise-file-types)
(require 'git-appraise-viewed)

(defcustom git-appraise-show-file-change-summary t
  "Show added and deleted line counts on changed file headings."
  :group 'git-appraise
  :type 'boolean)

(declare-function git-appraise-section-insert-comment "git-appraise-section-comment")
(declare-function git-appraise-viewed--hunk-viewed-p "git-appraise-viewed"
                  (path ranges))
(declare-function git-appraise-viewed--counter-string "git-appraise-viewed")
(declare-function git-appraise-diff--pr-stats-summary "git-appraise-diff")



;; files report

(defun git-appraise-section--focus-summary ()
  "Summary of files hidden from the review diff, or nil."
  (when git-appraise-section--file-classifications
    (let ((tags nil)
          (total 0))
      (maphash (lambda (_path info)
                 (let ((hidden? (or (and git-appraise-focus-mode
                                         (plist-get info :hide))
                                    (and git-appraise-exclude-gitattributes-generated
                                         (plist-get info :gitattributes-generated)))))
                   (when hidden?
                     (setq total (1+ total))
                     (let ((tag (or (plist-get info :tag) "noise")))
                       (let ((cell (assoc tag tags)))
                         (if cell
                             (setcdr cell (1+ (cdr cell)))
                           (push (cons tag 1) tags)))))))
               git-appraise-section--file-classifications)
      (when (> total 0)
        (format "%d file%s hidden (%s)"
                total
                (if (= total 1) "" "s")
                (mapconcat (lambda (x)
                             (format "%d %s" (cdr x) (car x)))
                           tags
                           ", "))))))
(defun git-appraise-section-insert-files-changed ()
  (let* ((files (a-get (git-appraise-db--pullreq-raw-infos) 'files))
         (stats-summary (git-appraise-diff--pr-stats-summary
                         files
                         (ignore-errors (git-appraise-db--pullreq-raw-diff)))))
    (insert (propertize
             (concat "Files changed"
                     (when stats-summary
                       (format " (%s)" stats-summary)))
             'font-lock-face
             'magit-section-heading))
    (magit-insert-heading)
    (when-let* ((summary (git-appraise-section--focus-summary)))
      (insert "  " summary "\n"))
    (let ((counter (git-appraise-viewed--counter-string)))
      (when counter
        (insert "  " counter "\n")))))
(defun git-appraise-wash-diff ()
  "Wash one `diff --git' block of a raw unified diff at point.
Owned copy of magit-diff's file-block washer (phase 11b): it
parses the diff header (status, rename, binary) from plain diff
text and delegates to `git-appraise-wash-insert-file-section'.
The only inputs are the text at point and the `magit-wash-sequence'
loop contract: returns non-nil while a block was washed, nil when
nothing matches (which ends the loop)."
  (when (looking-at
         ;; The file names on this line may be ambiguous due to
         ;; whitespace; that is fine, the subsequent `---'/`+++'
         ;; headers are authoritative.  The backreference group
         ;; is optional (as in magit's own washer): renames put
         ;; two different paths on this line, so the group never
         ;; matches there and the file name comes from
         ;; `rename to'/`+++' instead.  Without the optional
         ;; wrapper the whole pattern never matches any standard
         ;; `a/X b/X' line, the wash stops at the first block and
         ;; the rest of the diff lands as raw, uncolored text.
         "^diff --\\(?:\\(?1:git\\) \\(?:\\(?2:.+?\\) \\2\\)?\\|\\(?3:cc\\|combined\\) \\(?4:.+\\)\\)")
    (let* ((status (cond ((equal (match-string 1) "git") "modified")
                        ((match-string 3)              "resolved")
                        (t                            "unmerged")))
          (orig nil)
          (file (or (match-string 2) (match-string 4)))
          (header (list (buffer-substring-no-properties
                         (line-beginning-position) (1+ (line-end-position)))))
          (modes nil)
          (rename nil)
          (binary nil)
          ;; Canonical provenance comes from the diff header, not
          ;; from read-tracking state or a path's leading directory.
          (canonical-path
           (or (git-appraise--diff--extract-b-path
                (string-trim-right (car header)))
               file)))
      (git-appraise-wash--delete-line)
      ;; a blank line ends the block: it trails the raw diff (the
      ;; render inserts one after the last block) and carries no
      ;; extended header.  `git-appraise-lazy--wash-all' skips it
      ;; between blocks.
      (while (not (or (eobp)
                      (looking-at "^@@\\|^diff --\\|^Submodule\\|^[ \t]*$")))
        (cond
          ((looking-at "old mode \\(?:[^\n]+\\)\nnew mode \\(?:[^\n]+\\)\n")
           (setq modes (match-string 0)))
          ((looking-at "deleted file .+\n")
           (setq status "deleted"))
          ((looking-at "new file .+\n")
           (setq status "new file"))
          ((looking-at "rename from \\(.+\\)\nrename to \\(.+\\)\n")
           (setq rename (match-string 0))
           (setq orig (match-string 1))
           (setq file (match-string 2))
           (setq status "renamed"))
          ((looking-at "copy from \\(.+\\)\ncopy to \\(.+\\)\n")
           (setq orig (match-string 1))
           (setq file (match-string 2))
           (setq status "copied"))
          ((looking-at "similarity index .+\n"))
          ((looking-at "dissimilarity index .+\n"))
          ((looking-at "index .+\n"))
          ((looking-at "--- \\(.+?\\)\t?\n")
           (unless (equal (match-string 1) "/dev/null")
             (setq orig (match-string 1))))
          ((looking-at "\\+\\+\\+ \\(.+?\\)\t?\n")
           (unless (equal (match-string 1) "/dev/null")
             (setq file (match-string 1))))
          ((looking-at "Binary files .+ and .+ differ\n")
           (setq binary t))
          ((looking-at "Binary files differ\n")
           (setq binary t))
          ;; TODO Use all combined diff extended headers.
          ((looking-at "mode .+\n"))
          (t (error "git-appraise-wash-diff: unknown extended header: %S"
                    (buffer-substring (point) (line-end-position)))))
        ;; `old mode' and `rename' headers are shown as special
        ;; hunks, not part of the section header text.
        (unless (or (string-prefix-p "old mode" (match-string 0))
                    (string-prefix-p "rename" (match-string 0)))
          (push (match-string 0) header))
        (delete-region (point) (match-end 0)))
      (when orig
        (setq orig (git-appraise-wash--decode-git-path orig)))
      (setq file (git-appraise-wash--decode-git-path (or file canonical-path)))
      (setq canonical-path (git-appraise-wash--decode-git-path canonical-path))
      (setq header (string-join (nreverse header)))
      (git-appraise-wash-insert-file-section
       file orig status modes rename header binary nil canonical-path))))
(defun git-appraise-wash--decode-git-path (path)
  "Decode git-quoted PATH (\"\\226...\" style) to its raw form.
Delegates to magit's utility when available; identity otherwise."
  (if (fboundp 'magit-decode-git-path)
      (magit-decode-git-path path)
    path))
(defun git-appraise-wash--insert-file-tags (clean-path info)
  "Append status badges for CLEAN-PATH and classification INFO."
  (when (plist-get info :new)
    (insert (propertize "  [NEW]"
                        'font-lock-face 'git-appraise-diff-tag-face)))
  (when (plist-get info :changed)
    (insert (propertize git-appraise-section--changed-tag
                        'font-lock-face 'git-appraise-dimmed))))

(defun git-appraise-wash--insert-file-heading
    (status display clean-path info &optional lazy)
  "Insert a file row for STATUS, DISPLAY and CLEAN-PATH.
INFO supplies its classification; LAZY marks a deferred file."
  (let* ((beg (point))
         (tag (or (plist-get info :tag) ""))
         (entry (and (hash-table-p git-appraise-viewed--buffer-inventory)
                     (gethash clean-path git-appraise-viewed--buffer-inventory)))
         (added (or (plist-get entry :added) 0))
         (deleted (or (plist-get entry :deleted) 0)))
    (let ((heading-face (if (plist-get info :viewed)
                            'git-appraise-dimmed
                          'magit-diff-file-heading)))
      (insert (propertize (format "%-10s " status)
                          'font-lock-face heading-face))
      (insert (propertize
               (concat tag
                       (make-string
                        (max 0 (- (git-appraise-file-types--tag-width)
                                  (string-width tag))) ?\s)
                       " ")
               'font-lock-face (or (plist-get info :face)
                                   'git-appraise-diff-tag-face)))
      (when git-appraise-show-file-change-summary
        (if (or (> added 0) (> deleted 0))
            (let* ((add-str (format "+%d" added))
                   (del-str (format "-%d" deleted))
                   (text (concat add-str " " del-str))
                   (pad (max 0 (- 12 (length text)))))
              (insert (propertize add-str
                                  'font-lock-face
                                  (if (> added 0) 'magit-diff-added 'git-appraise-dimmed)))
              (insert " ")
              (insert (propertize del-str
                                  'font-lock-face
                                  (if (> deleted 0) 'magit-diff-removed 'git-appraise-dimmed)))
              (insert (make-string pad ?\s)))
          (insert (format "%-12s" ""))))
      ;; viewed column
      (let ((viewed-str (if (and git-appraise-viewed-indicator
                                 (plist-get info :viewed))
                            git-appraise-viewed-indicator
                          "")))
        (insert (propertize (format "%-3s" viewed-str)
                            'font-lock-face 'git-appraise-dimmed
                            'git-appraise-viewed-column t)))
      ;; comment count column
      (let* ((count (git-appraise-history--file-count clean-path))
             (comment-str (if (and git-appraise-comment-indicator (> count 0))
                              (format "%s%d" git-appraise-comment-indicator count)
                            "")))
        (insert (propertize (format "%-5s" comment-str)
                            'font-lock-face 'git-appraise-dimmed)))
      (insert (propertize display 'font-lock-face heading-face
                          'help-echo (when-let* ((language (plist-get info :language)))
                                       (format "Linguist language: %s" language)))))
    (add-text-properties beg (point)
                         `(git-appraise-path ,clean-path
                           ,@(when lazy '(git-appraise-lazy t))))
    (git-appraise-wash--insert-file-tags clean-path info)))

(defun git-appraise-wash--refresh-file-comment-count (section path)
  "Refresh PATH's comment count in rendered file SECTION."
  (let* ((count (git-appraise-history--file-count path))
         (label (if (and git-appraise-comment-indicator (> count 0))
                    (format "%s%d" git-appraise-comment-indicator count)
                  ""))
         (inhibit-read-only t))
    (save-excursion
      (goto-char (oref section start))
      (let* ((column (+ (line-beginning-position) 11 5
                        (if git-appraise-show-file-change-summary 12 0) 3))
             (properties (text-properties-at column)))
        (when (<= (+ column 5) (line-end-position))
          (delete-region column (+ column 5))
          (goto-char column)
          (insert (format "%-5s" label))
          (add-text-properties column (point) properties))))))

(defun git-appraise-wash-insert-file-section
    (file orig status modes rename header binary long-status
          &optional canonical-path)
  "Insert the file section for FILE and wash its hunks.
ORIG is the original file name (renames), STATUS the change type,
MODES a mode-change header, RENAME a rename header, HEADER the
raw extended headers, BINARY non-nil for binary files,
LONG-STATUS an alternative status text, and CANONICAL-PATH the
repository-relative path from the diff header.  This is an owned copy of
magit-diff's file inserter (phase 11b); ours adds the
git-appraise specifics: file classification (tags, collapse),
comment bookkeeping and the trailing `missing comments' pass.
Returns the section: `magit-wash-sequence' keeps washing while
this is non-nil."

  ;;; --- beg -- git-appraise specific code.
  ;;; I need to set a reference point for the first hunk header
  ;;; so the positioning of comments is done correctly.
  ;;; Also apply file classification (tags, collapse) from
  ;;; `git-appraise--diff--classify-diff'.
  (let* ((raw-path-name (substring-no-properties file))
         (clean-path (or canonical-path
                         (git-appraise-diff--strip-path raw-path-name)))
         (info (and git-appraise-section--file-classifications
                    (gethash clean-path
                             git-appraise-section--file-classifications)))
         (collapse (git-appraise-section--file-collapse-p clean-path info))
         (folded (git-appraise-fold-state--file-folded-p
                  clean-path (or git-appraise-fold-files-by-default collapse))))
    (git-appraise-db--curr-path-update clean-path)
    ;;; --- end -- git-appraise specific code.
    ;; the HIDE flag above only sets the section's `hidden' slot;
    ;; git-appraise renders directly (no `magit-refresh-buffer'
    ;; pass), so collapse the body here as well.  Return the
    ;; section afterwards: `magit-wash-sequence' keeps washing
    ;; files only while this function returns non-nil, so the
    ;; collapse wrapper must not be the last form (a `when'
    ;; around a visible section evaluates to nil, which stopped
    ;; the wash after the first file and left the rest of the
    ;; diff as uncolored raw text).
    (let ((section (magit-insert-section section
      (file file (or (equal status "deleted")
                    (derived-mode-p 'magit-status-mode)
                    folded))
      (git-appraise-wash--insert-file-heading
       status (if (or (not orig) (equal orig file))
                  file
                (format "%s -> %s" orig file))
       clean-path info)
      (when long-status
        (insert (format " (%s)" long-status)))
      (magit-insert-heading)
      (insert ?\n)
    (unless (equal orig file)
      (oset section source orig))
    (oset section header header)
    (when modes
      (magit-insert-section (hunk '(chmod))
        (insert modes)
        (magit-insert-heading)))
    (when rename
      (magit-insert-section (hunk '(rename))
        (insert rename)
        (magit-insert-heading)))
    (when (string-match-p "Binary files.*" header)
      (magit-insert-section (git-appraise-binary-file-section file)
        (insert (propertize "Visit file"
                            'face 'git-appraise-url-header-face
                            'mouse-face 'git-appraise-hover-face
                            'help-echo "Visit the file in Dired buffer"
                            'keymap 'git-appraise-binary-file-section-map))
        (magit-insert-heading)))
    (git-appraise-render--wash-sequence #'git-appraise-wash-hunk)
    ;; After washing all hunks for this file, insert any remaining
    ;; comments (e.g., local or outdated ones keyed by side/line)
    ;; that weren’t anchored to a concrete diff position.
    (let* ((raw-path-name (substring-no-properties file))
           (clean-path (if (string-prefix-p "b/" raw-path-name)
                           (replace-regexp-in-string "^b/" "" raw-path-name)
                         raw-path-name))
           (missing-paths (git-appraise-utils--missing-outdated-commments?
                           clean-path
                           git-appraise-section-hold-written-comment-ids
                           git-appraise-section-grouped-comments)))
      (when (and missing-paths git-appraise-section--display-all-comments)
        (git-appraise-section-insert-outdated-comment-missing
         clean-path missing-paths git-appraise-section-grouped-comments))
      (git-appraise-history-insert-for-file clean-path)
      (when-let* ((language (plist-get info :language)))
        (magit-insert-section (git-appraise-file-language language)
          (insert (propertize (format "Language: %s" language)
                              'font-lock-face 'git-appraise-file-language-face))
          (magit-insert-heading)))))))
      (git-appraise-section--hide-if-hidden section)
      section)))
(defun git-appraise-wash-hunk--comment-line-keys (path-name ch old-line new-line)
  "The side/line comment grouping keys for one washed hunk line.
PATH-NAME is the file path, CH the line's +/-/space sigil, and
OLD-LINE / NEW-LINE the line numbers the line carries on each
side (nil on a side the line does not exist in)."
  (cond
   ((string= ch " ")
    (list (git-appraise-utils--comment-key-from-line path-name "RIGHT" new-line)
          (git-appraise-utils--comment-key-from-line path-name "LEFT" old-line)))
   ((string= ch "+")
    (list (git-appraise-utils--comment-key-from-line path-name "RIGHT" new-line)))
   ((string= ch "-")
    (list (git-appraise-utils--comment-key-from-line path-name "LEFT" old-line)))
   (t nil)))
(defun git-appraise-wash-hunk--insert-line-comments (keys path-name)
  "Insert the grouped comments anchored at the comment group KEYS
of the current hunk line (PATH-NAME), marking each consumed key
written.  Returns t when any comment was inserted (insertion
moves point, so the caller must then not advance a line)."
  (let (to-insert)
    (dolist (key keys)
      (let ((grouped (git-appraise-utils--comment-get
                      git-appraise-section-grouped-comments key))
            (written? (-contains-p git-appraise-section-hold-written-comment-ids
                                   key)))
        (when (and grouped (not written?)
                   git-appraise-section--display-all-comments)
          (push key git-appraise-section-hold-written-comment-ids)
          (setq to-insert (append to-insert (copy-sequence grouped))))))
    (when to-insert
      (let ((comment-written-pos
             (or (alist-get path-name
                            git-appraise-section-hold-written-comment-count
                            nil nil 'equal)
                 0)))
        (git-appraise-section-insert-comment to-insert comment-written-pos))
      t)))
(defun git-appraise-wash-hunk--advance-counters (ch old-line new-line)
  "The (OLD . NEW) line counters after washing the line with sigil CH.
OLD-LINE / NEW-LINE are the counters before this line (nil on a
side the hunk does not cover)."
  (cond
   ((string= ch " ")
    (cons (and old-line (1+ old-line))
          (and new-line (1+ new-line))))
   ((string= ch "+")
    (cons old-line (and new-line (1+ new-line))))
   ((string= ch "-")
    (cons (and old-line (1+ old-line)) new-line))
   (t (cons old-line new-line))))
(defun git-appraise-wash-hunk--ranges (raw-ranges)
  "Parse RAW-RANGES (the @@ header ranges text) into number pairs.
A single line is +1 rather than +1,1."
  (mapcar (lambda (str)
            (let ((range
                   (mapcar #'string-to-number
                           (split-string (substring str 1) ","))))
              (if (length= range 1)
                  (nconc range (list 1))
                range)))
          (split-string raw-ranges)))
(defun git-appraise-wash-hunk--ensure-head-pos (path path-name)
  "The head-pos for PATH (its hunk-header reference line), fixing
it on the db (PATH-NAME) when missing so comment positions anchor."
  (or (oref path head-pos)
      (let ((adjusted-pos (+ (git-appraise--line-number-at-pos) 1)))
        (git-appraise-db--curr-path-head-pos-update path-name adjusted-pos)
        adjusted-pos)))
(defun git-appraise-wash-hunk--insert-heading (heading badge notest &optional viewed?)
  "Insert the hunk HEADING line, the phase 17 NO-TEST tag NOTEST
and, when non-nil, the delicacy BADGE (phase 15), then close the
heading (`magit-insert-heading').  VIEWED? (phase 13 slice 2)
dims the heading instead of the regular hunk heading face: the
reviewer marked this hunk read."
  (insert (propertize heading
                      'font-lock-face
                      (if viewed? 'git-appraise-dimmed
                        'magit-diff-hunk-heading)))
  (when notest
    (insert (propertize (format "  [%s]" notest)
                        'font-lock-face 'font-lock-warning-face)))
  (when badge
    (insert (propertize badge
                        'font-lock-face
                        'git-appraise-delicate-hunk-face)))
  (insert ?\n)
  (magit-insert-heading))
(defun git-appraise-wash-hunk--interleave-comments (path-name head-pos
                                                  old-line new-line)
  "Wash the hunk body lines from point, interleaving the grouped
comments (`git-appraise-section-grouped-comments', keyed by path
and diff position or by side/line) inline as the lines are
consumed.  OLD-LINE / NEW-LINE are the starting line counters from
the hunk header; each line advances them per its +/-/space
sigil.  The wash stops at the next file header or buffer end."
  (while (not (or (eobp) (looking-at "^[^-+\s\\]")))
    ;; --- git-appraise specific code: add code comments
    (let* ((line-text (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position)))
           (ch (if (> (length line-text) 0) (substring line-text 0 1) ""))
           ;; Position-keyed comments (original API)
           (diff-pos (+ 1 (- (git-appraise--line-number-at-pos)
                             (or head-pos 0)
                             (or (alist-get path-name git-appraise-section-hold-written-comment-count nil nil 'equal) 0))))
           (path-pos (git-appraise-utils--comment-key path-name diff-pos))
           ;; Side/line-keyed comments (locals / GraphQL
           ;; line API).  Context lines can anchor
           ;; comments on either side, so try both keys.
           (line-keys (git-appraise-wash-hunk--comment-line-keys
                       path-name ch old-line new-line))
           (did-insert
            (git-appraise-wash-hunk--insert-line-comments
             (cons path-pos line-keys) path-name)))
      ;; Advance line only when nothing was inserted
      ;; (insertion moves point)
      (unless did-insert
        (forward-line))
      ;; Update counters for old/new line numbers following
      ;; this patch line
      (pcase-let ((`(,old . ,new)
                   (git-appraise-wash-hunk--advance-counters
                    ch old-line new-line)))
        (setq old-line old
              new-line new)))))
(defun git-appraise-wash-hunk ()
  "Wash the hunk at point, inserting PR comment sections inline.
Owned copy of magit-diff's hunk washer (phase 11b), interleaving
the grouped comments (`git-appraise-section-grouped-comments',
keyed by path and diff position or by side/line) into the hunk
body as it is washed.  Returns t when a hunk was washed, nil
otherwise, per the `magit-wash-sequence' contract."
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
    ;; Bind the match data BEFORE any database access below: the db
    ;; writes clobber the global match data
    ;; statements with string-matches on a cold cache, leaving
    ;; string-relative positions), which made the `match-string'
    ;; reads below return garbage or even signal args-out-of-range
    ;; on the first hunk of a fresh Emacs.  Bind first, use after.
    (let* ((heading    (match-string 0))
           (raw-ranges (match-string 1))
           (about      (match-string 2))
    ;;; --- beg -- git-appraise specific code.
    ;;; I need to set a reference point for the first hunk header
    ;;; so the positioning of comments is done correctly.
           (path (git-appraise-db--curr-path))
           (path-name (oref path name))
           (head-pos (git-appraise-wash-hunk--ensure-head-pos path path-name)))
      (when (not head-pos)
        (git-appraise-utils--log
         "git-appraise-wash-hunk"
         (format "Every diff is associated with a PATH (the file). Head pos nil for %S"
                 (prin1-to-string path)))
        (message "ERROR: Head position for path %s was not found.
Please Report this Bug" path-name))
    ;;; --- end -- git-appraise specific code.

      (let* ((ranges   (git-appraise-wash-hunk--ranges raw-ranges))
             (combined (= (length ranges) 3))
             (value    (cons about ranges)))
        (magit-delete-line)
        ;; Paint the hunk (diff colors) as soon as it is washed:
        ;; `magit-insert-section' returns the section object, and by
        ;; then its `end' marker is set, which the painter needs.
        (let ((hunk-section
               (magit-insert-section
            ( hunk
              `((value . ,value) ;; TODO not sure if this has to diverge as well
                (path . ,path-name)
                ;; the hunk KEY: byte-identical to the delicacy
                ;; entry's :ranges (phase 15 badge/jump list; the
                ;; phase 13 read-tracking key too)
                (ranges . ,raw-ranges)
                (head-pos . ,head-pos))
              nil
              :combined combined
              :from-range (if combined (butlast ranges) (car ranges))
              :to-range (car (last ranges))
              :about about)
          ;; phase 15: the delicacy badge; phase 17: the NO-TEST tag
          ;; (cache hits: the Analysis section runs before the diff
          ;; wash in the sections hook); phase 13 slice 2: a hunk
          ;; the reviewer marked read dims its heading
          (git-appraise-wash-hunk--insert-heading
           heading
           nil
           nil
           (git-appraise-viewed--hunk-viewed-p path-name ranges))
          ;; Keep track of old/new line numbers from the hunk header so we
          ;; can anchor local comments keyed by SIDE/LINE inline.
          (git-appraise-wash-hunk--interleave-comments
           path-name head-pos
           (and (car ranges) (car (car ranges)))
           (and (car (last ranges)) (car (car (last ranges)))))

          ;; Remaining comments for this file are handled once per file
          ;; in git-appraise-wash-insert-file-section.
          )))
          (git-appraise-wash--paint-hunk hunk-section))))
    t))
(provide 'git-appraise-section-wash)
;;; git-appraise-section-wash ends here
