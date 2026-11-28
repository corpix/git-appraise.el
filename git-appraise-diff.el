;;; git-appraise-diff.el --- Diff classification and file ordering -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski
;;
;; This file is part of git-appraise.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either under version 3, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; Commentary:
;;
;;  The phase-3 classification engine: pure functions that tag,
;;  collapse, hide and order the files of a raw unified diff.
;;  Comment anchors refer to the API diff, so this only classifies;
;;  it never rewrites the diff content being reviewed.
;;
;;; Code:

(require 'dash)


(require 'a)
(require 'git-appraise-db)
(require 'git-appraise-repo)
(require 'git-appraise-file-types)

(declare-function git-appraise--build-buffer "git-appraise-section")
(defvar git-appraise-focus-mode nil)


(defcustom git-appraise-diff-file-order-rules nil
  "Regexps for files that should appear first, in rule order.
File classification, labels, folding, and focus hiding are
configured through `git-appraise-file-types'."
  :group 'git-appraise
  :type '(repeat regexp))

(defcustom git-appraise-exclude-gitattributes-generated t
  "When non-nil, exclude files marked generated or vendored by git attributes.
Reads `linguist-generated' and `linguist-vendored' attributes from
.gitattributes.  Excluded files are hidden from the review diff and
reported in the Files changed summary."
  :group 'git-appraise
  :type 'boolean)

;;; Classification functions

(defun git-appraise--diff--extract-b-path (header-line)
  "Extract the new-file path from a diff HEADER-LINE.
Handles both the usual \"diff --git a/old b/new\" form and the
no-prefix \"diff --git old new\" form some tools return."
  (cond
   ((string-match "^diff --git a/\\(.+?\\) b/\\(.+\\)$" header-line)
    (match-string 2 header-line))
   ((string-match "^diff --git \\(.+?\\) \\(.+\\)$" header-line)
    (match-string 2 header-line))))

(defun git-appraise--diff--split-by-files (diff-text)
  "Split DIFF-TEXT into a list of (path . block) per file."
  (let ((pos 0)
        (len (length diff-text))
        blocks)
    (while (and (< pos len)
                (string-match "^diff --git .+$" diff-text pos))
      (let* ((start (match-beginning 0))
             ;; advance to next header or end
             (next (if (string-match "^diff --git .+$" diff-text (match-end 0))
                       (match-beginning 0)
                     len))
             (block (substring diff-text start next))
             (first-line-end (string-match "\n" block))
             (header (if first-line-end
                         (substring block 0 first-line-end)
                       block))
             (path (or (git-appraise--diff--extract-b-path header) "")))
        (push (cons path block) blocks)
        (setq pos next)))
    (nreverse blocks)))

(defun git-appraise--diff--classify-path (path &optional attrs)
  "Return PATH's stable file type and appearance using Linguist ATTRS."
  (git-appraise-file-types--classify path attrs))

(defun git-appraise-diff--attribute-context ()
  "Return (REPO SOURCE) for reading this review's Git attributes."
  (let* ((pr (git-appraise-db-get-pullreq))
         (repo (or (and (boundp 'git-appraise-repo-worktree)
                        git-appraise-repo-worktree)
                   default-directory))
         (source (and pr (slot-boundp pr 'sha)
                      (stringp (oref pr sha)) (oref pr sha))))
    (list repo source)))

(defun git-appraise-diff--stats (diff-text)
  "Compute the add/del line stats over raw DIFF-TEXT.
Return a plist (:files N :add A :del D :add-doc AD :del-doc DD):
`:add'/`:del' count every changed line, `:add-doc'/`:del-doc' only
the lines of DOC-classified files (the \"excluding docs\"
parenthetical).  Computed from the same diff text the wash renders.
Binary blocks
(\"GIT binary patch\" / \"Binary files ... differ\") carry no
line counts."
  (let* ((blocks (git-appraise--diff--split-by-files diff-text))
         (context (git-appraise-diff--attribute-context))
         (attrs (git-appraise-file-types--attributes
                 (mapcar #'car blocks) (car context) (cadr context)))
         (files 0) (add 0) (del 0) (add-doc 0) (del-doc 0))
    (dolist (blk blocks)
      (let* ((path (car blk))
             (block (cdr blk))
             (doc? (eq (plist-get
                        (git-appraise--diff--classify-path
                         path (gethash path attrs)) :type)
                       'documentation))
            (a 0) (d 0))
        (setq files (1+ files))
        (unless (string-match-p "^GIT binary patch\\|^Binary files " block)
          (dolist (line (split-string block "\n"))
            (cond
             ((string-prefix-p "+++" line)) ; file header
             ((string-prefix-p "---" line)) ; file header
             ((string-prefix-p "+" line) (setq a (1+ a)))
             ((string-prefix-p "-" line) (setq d (1+ d))))))
        (setq add (+ add a)
              del (+ del d))
        (when doc?
          (setq add-doc (+ add-doc a)
                del-doc (+ del-doc d)))))
    (list :files files :add add :del del
          :add-doc add-doc :del-doc del-doc)))

(defun git-appraise-diff--stats-string (stats)
  "Format STATS (`git-appraise-diff--stats' output) as one line.
The DOC share lands in parentheses only when docs actually
changed: no parens noise on pure-code diffs."
  (concat (format "%d files; %d additions, %d deletions"
                  (plist-get stats :files)
                  (plist-get stats :add)
                  (plist-get stats :del))
          (when (> (+ (plist-get stats :add-doc)
                      (plist-get stats :del-doc))
                   0)
            (format " (%d additions, %d deletions excluding docs)"
                    (plist-get stats :add-doc)
                    (plist-get stats :del-doc)))))

(defun git-appraise-diff--pr-stats-summary (files raw-diff)
  "Return a PR stats summary string from FILES info or RAW-DIFF.
FILES is the files alist with `nodes', or a nodes list.  RAW-DIFF
is the raw unified diff text.  Returns nil when no stats are available."
  (let ((nodes (cond ((and (listp files) (alist-get 'nodes files))
                      (alist-get 'nodes files))
                     ((and (listp files) (consp (car files))
                           (alist-get 'additions (car files)))
                      files)
                     (t nil))))
    (cond
     (nodes
      (format "%d files; %s additions, %s deletions"
              (length nodes)
              (apply #'+ (mapcar (lambda (x) (alist-get 'additions x)) nodes))
              (apply #'+ (mapcar (lambda (x) (alist-get 'deletions x)) nodes))))
     ((and raw-diff (not (string-empty-p raw-diff)))
      (let ((stats (git-appraise-diff--stats raw-diff)))
        (when (and stats (> (plist-get stats :files) 0))
          (git-appraise-diff--stats-string stats)))))))

(defun git-appraise--diff--ws-normalize (line)
  "Collapse whitespace in LINE for whitespace-only comparisons."
  (string-join (split-string line "\\s-+" t) " "))

(defun git-appraise--diff--hunk-ws-only-p (minus plus)
  "Non-nil when MINUS/PLUS line lists differ only in whitespace."
  (and (= (length minus) (length plus))
       (let ((a (mapcar #'git-appraise--diff--ws-normalize minus))
             (b (mapcar #'git-appraise--diff--ws-normalize plus)))
         (while (and a b (string-equal (car a) (car b)))
           (setq a (cdr a) b (cdr b)))
         (null a))))

(defun git-appraise--diff--block-ws-only-p (block)
  "Non-nil when every hunk of BLOCK only changes whitespace.
Blocks without hunks (pure renames, binary, mode-only) return nil."
  (let ((lines (split-string block "\n"))
        (saw-hunk nil)
        (in-hunk nil)
        (minus nil)
        (plus nil))
    (catch 'done
      (dolist (line lines)
        (cond
         ((string-prefix-p "@@" line)
          (when in-hunk
            (unless (git-appraise--diff--hunk-ws-only-p (nreverse minus)
                                                        (nreverse plus))
              (throw 'done nil)))
          (setq in-hunk t
                saw-hunk t
                minus nil
                plus nil))
         (t
          (when in-hunk
            (cond
             ((string-prefix-p "-" line) (push (substring line 1) minus))
             ((string-prefix-p "+" line) (push (substring line 1) plus)))))))
      (when in-hunk
        (unless (git-appraise--diff--hunk-ws-only-p (nreverse minus)
                                                   (nreverse plus))
          (throw 'done nil)))
      saw-hunk)))

(defun git-appraise--diff--block-pure-rename-p (block)
  "Non-nil when BLOCK is a pure rename with no content hunks."
  (and (string-match-p "^rename from " block)
       (not (string-match-p "^@@" block))))

(defun git-appraise--diff--block-has-content-p (block)
  "Non-nil when BLOCK has at least one added or removed line."
  (or (string-match-p "\n\\+[^+]" block)
      (string-match-p "\n-[^-]" block)))

(defun git-appraise--diff--classify-diff (diff-text)
  "Classify every file of DIFF-TEXT.
Return a hash of path to stable type, style, and Linguist metadata.
The diff content and comment anchors are never rewritten."
  (let* ((table (make-hash-table :test #'equal))
         (blocks (git-appraise--diff--split-by-files diff-text))
         (context (git-appraise-diff--attribute-context))
         (attributes (git-appraise-file-types--attributes
                      (mapcar #'car blocks) (car context) (cadr context)))
         (substantive
          (ignore-errors
            (let ((pr (git-appraise-db-get-pullreq)))
              (and (slot-boundp pr 'number)
                   (git-appraise-repo-substantive-files
                    (format "%s" (oref pr number))))))))
    (dolist (blk blocks)
      (let* ((path (car blk))
             (block (cdr blk))
             (attrs (gethash path attributes))
             (info (git-appraise--diff--classify-path path attrs))
             (generated? (or (eq (plist-get attrs :generated) 'set)
                             (eq (plist-get attrs :vendored) 'set))))
        (cond
         ((git-appraise--diff--block-pure-rename-p block)
          (setq info (git-appraise-file-types--info nil attrs)
                info (plist-put info :collapse t)))
         ((git-appraise--diff--block-ws-only-p block)
          (setq info (git-appraise-file-types--info 'whitespace attrs)))
         ((and substantive
               (git-appraise--diff--block-has-content-p block)
               (not (member path substantive)))
          (setq info (git-appraise-file-types--info 'whitespace attrs))))
        (when generated?
          (setq info (plist-put info :linguist-excluded t)))
        (puthash path info table)))
    table))

(defun git-appraise-diff--new-since-last-review (old-diff new-diff old-shas new-shas)
  "Compare the PREVIOUS review snapshot with the newly loaded one.
OLD-DIFF/NEW-DIFF are raw unified diff texts, OLD-SHAS/NEW-SHAS
the abbreviated commit oids of the previous and the fresh
raw-infos commit lists.  Return an alist
`((files . PATHS) (commits . SHAS))' describing what a returning
reviewer has not seen yet: files whose diff block changed (a new
path, different hunks or different context) and commits missing
from the snapshot.  Nil only when there is no previous snapshot
(OLD-DIFF nil): an existing snapshot yields the alist even when
nothing changed (an EMPTY files/commits list is the \"no new
changes\" signal for the render).  Phase 13: the db's stored diff
IS \"what the reviewer last saw\", so diffing the snapshots needs
no per-file viewed state (the 2026-11-24 viewed marks live in git
notes, independently of the NEW pass)."
  (when (and (stringp old-diff) (stringp new-diff))
    (let* ((old-blocks (git-appraise--diff--split-by-files old-diff))
           (new-blocks (git-appraise--diff--split-by-files new-diff))
           (old-map (make-hash-table :test 'equal))
           (new-files nil))
      (dolist (blk old-blocks)
        (puthash (car blk) (cdr blk) old-map))
      (dolist (blk new-blocks)
        (unless (equal (gethash (car blk) old-map) (cdr blk))
          (push (car blk) new-files)))
      (let ((files (nreverse new-files))
            ;; commits ≤ 100 per PR query: `member' is fine here and
            ;; keeps this file free of a cl-lib dependency
            (commits (let ((new nil))
                       (dolist (s new-shas)
                         (unless (member s old-shas)
                           (push s new)))
                       (nreverse new))))
        `((files . ,files) (commits . ,commits))))))

(defun git-appraise-diff--mark-new-files (classifications new-paths)
  "Flag NEW-PATHS in CLASSIFICATIONS without changing their classification.
The additional NEW badge does not replace existing tags or override
their collapse and focus-mode rules."
  (when (and (hash-table-p classifications) new-paths)
    (dolist (path new-paths)
      (let ((info (gethash path classifications 'missing)))
        (unless (eq info 'missing)
          (puthash path (plist-put info :new t) classifications))))))

(defun git-appraise-diff--mark-viewed-files (classifications index)
  "Flag the phase 13 read-viewed files in the CLASSIFICATIONS hash.
INDEX is the render index (`git-appraise-viewed--index'): path ->
plist (:file FILE-ROW-P :hunks VIEWED-KEYS :all-hunks ALL-P).
A file gets :viewed t when explicitly marked (the file row) or
when ALL its hunks are marked read (the rollup).  Only entries
present in CLASSIFICATIONS are flagged: a stale row for a path
that left the diff must not create a classification."
  (when (and (hash-table-p classifications) (hash-table-p index))
    (maphash (lambda (path vinfo)
               (let ((info (gethash path classifications 'missing)))
                 (unless (eq info 'missing)
                   (when (or (plist-get vinfo :file)
                             (plist-get vinfo :all-hunks))
                     (setq info (plist-put info :viewed t)))
                   (when (plist-get vinfo :changed)
                     (setq info (plist-put info :changed t)))
                   (puthash path info classifications))))
             index)))

(defun git-appraise--diff--any-new-p (classifications)
  "Return t when any entry in the CLASSIFICATIONS hash is :new."
  (let ((found nil))
    (when (hash-table-p classifications)
      (maphash (lambda (_path info)
                 (when (plist-get info :new)
                   (setq found t)))
               classifications))
    found))

(defun git-appraise-diff--strip-path (path)
  "Strip a leading a/ or b/ diff prefix from PATH.
The one canonical copy of the stripper that had grown four
identical ones across actions, browse, testimpact and viewed."
  (cond ((string-prefix-p "a/" path) (substring path 2))
        ((string-prefix-p "b/" path) (substring path 2))
        (t path)))

(defun git-appraise--diff--file-order-index (path &optional info)
  "Return the ordering index for PATH.
User regexps come first; otherwise INFO's type sort band applies."
  (let ((user git-appraise-diff-file-order-rules)
        (idx 0)
        (found nil))
    (while (and user (not found))
      (let ((rule (pop user)))
        (if (string-match-p rule path)
            (setq found idx)
          (setq idx (1+ idx)))))
    (or found (+ idx 1 (or (plist-get info :order) 0)))))

(defun git-appraise--maybe-reorder-diff (diff-text &optional classifications)
  "Reorder DIFF-TEXT per the file rules, and honor focus mode.
CLASSIFICATIONS is the hash table produced by
`git-appraise--diff--classify-diff'.  When focus mode is on, files
whose classification has a non-nil :hide are omitted from the
buffer; they remain counted in the \"Files changed\" heading."
  (if (and (not git-appraise-diff-file-order-rules)
           (not git-appraise-file-types)
           (not git-appraise-focus-mode)
           (not git-appraise-exclude-gitattributes-generated)
           ;; phase 13: new-since-last-review files jump the queue
           (not (git-appraise--diff--any-new-p classifications)))
      diff-text
    (let* ((first-pos (string-match "^diff --git .+$" diff-text 0))
           (prefix (if (and first-pos (> first-pos 0))
                       (substring diff-text 0 first-pos)
                     ""))
           (blocks (git-appraise--diff--split-by-files
                    (if first-pos (substring diff-text first-pos) diff-text)))
           (sorted (sort blocks
                         (lambda (a b)
                           (let* ((ia (git-appraise--diff--file-order-index
                                       (car a) (and classifications
                                                    (gethash (car a) classifications))))
                                  (ib (git-appraise--diff--file-order-index
                                       (car b) (and classifications
                                                    (gethash (car b) classifications))))
                                  (na (plist-get (and classifications
                                                      (gethash (car a)
                                                               classifications))
                                                 :new))
                                  (nb (plist-get (and classifications
                                                      (gethash (car b)
                                                               classifications))
                                                 :new)))
                             (cond
                              ;; phase 13: the returning reviewer reads
                              ;; the new files first
                              ((not (eq (null na) (null nb))) (null nb))
                              ((/= ia ib) (< ia ib))
                              (t (string-lessp (car a) (car b))))))))
           (kept (if classifications
                     (let (acc)
                       (dolist (blk sorted)
                         (let* ((info (gethash (car blk) classifications))
                                (hide? (or (and git-appraise-focus-mode
                                                (plist-get info :hide))
                                           (and git-appraise-exclude-gitattributes-generated
                                                (plist-get info :linguist-excluded)))))
                           (unless hide?
                             (push blk acc))))
                       (nreverse acc))
                   sorted)))
      (concat prefix
              (mapconcat
               (lambda (blk)
                 ;; a block can lose its trailing newline when the
                 ;; diff was trimmed: never glue two blocks together
                 (let ((text (cdr blk)))
                   (if (string-suffix-p "\n" text) text (concat text "\n"))))
               kept "")))))

;;;###autoload
(defun git-appraise-toggle-exclude-gitattributes-generated ()
  "Toggle exclusion of git-attribute generated/vendored files in this review."
  (interactive)
  (setq-local git-appraise-exclude-gitattributes-generated
              (not git-appraise-exclude-gitattributes-generated))
  (let ((git-appraise-section-full-refresh? nil))
    (git-appraise--build-buffer))
  (message "Exclude gitattributes generated files: %s"
           (if git-appraise-exclude-gitattributes-generated "on" "off")))

(provide 'git-appraise-diff)
;;; git-appraise-diff.el ends here
