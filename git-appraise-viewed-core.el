;;; git-appraise-viewed-core.el --- Diff identities and viewed projection -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure inventory and event projection for viewed state.

;;; Code:

(require 'cl-lib)
(require 'a)
(require 'dash)
(require 'git-appraise-utils)
(declare-function git-appraise--diff--split-by-files "git-appraise-diff")

;;; Identities

(defun git-appraise-viewed--parse-ranges (raw-ranges)
  "Parse the RAW-RANGES text of an @@ header into number pairs.
\"-7,6 +8,7\" -> ((7 6) (8 7)); a missing count means one line.
The same canonical shape the washer's
`git-appraise-wash-hunk--ranges' produces, so both sides feed
`git-appraise-viewed--range-key'."
  (mapcar
   (lambda (token)
     (let ((numbers (mapcar #'string-to-number
                            (split-string (substring token 1) "," t))))
       (if (length= numbers 1)
           (list (car numbers) 1)
         numbers)))
   (split-string raw-ranges " " t)))

(defun git-appraise-viewed--range-key (ranges)
  "Canonical string for the parsed RANGES number pairs.
((-7 6) (8 7))-shaped pairs -> \"-7,6 +8,7\"; the last pair is
the head side, any earlier pairs are combined-diff sides.
Inventory and washer both serialize through here, so a stored
`range' always compares byte-equal against the current hunk's key."
  (let ((n (length ranges)))
    (mapconcat
     (lambda (i)
       (pcase-let ((`(,start ,count) (nth i ranges)))
         (format "%c%d,%d" (if (= i (1- n)) ?+ ?-) start count)))
     (number-sequence 0 (1- n))
     " ")))

(defun git-appraise-viewed--fingerprint (path lines)
  "Hash PATH and the normalized hunk LINES into a fingerprint.
LINES starts with the @@ header whose range numbers were replaced
by literal `@@' (its function-context text survives); everything
else -- sigils, whitespace, `\\ No newline' markers -- is kept
verbatim.  A pure line-number shift keeps the fingerprint; any
content change invalidates the mark."
  (concat "sha256:"
          (secure-hash 'sha256
                       (concat path "\0" (mapconcat #'identity lines "\n")))))

(defun git-appraise-viewed--file-fingerprint (block)
  "Hash the whole raw diff BLOCK of a hunk-less file.
Headers included, so a mode change, rename side or binary payload
change invalidates the file's mark."
  (concat "sha256:" (secure-hash 'sha256 block)))

;;; Inventory

(defconst git-appraise-viewed--hunk-header-regexp
  "^@@ \\(-[0-9]+\\(?:,[0-9]+\\)? \\+[0-9]+\\(?:,[0-9]+\\)?\\) @@\\([^\n]*\\)$"
  "An @@ hunk header: group 1 the ranges text, group 2 the context.
The same header shape the washer and the hunk-key matcher use.")

(defun git-appraise-viewed--block-hunks (path block)
  "List the hunks of BLOCK, PATH for hashing.
Each hunk is a plist (:fp :range :sides :dup) with :fp the
content fingerprint of the header plus its body lines, :range the
canonical range key, :sides the number of range sides (3 on
combined diffs) and :dup t when the same fingerprint occurs more
than once in this file."
  (let ((staged nil)
        (pos 0))
    (while (string-match git-appraise-viewed--hunk-header-regexp block pos)
      ;; capture this header's groups BEFORE the body-end search
      ;; below clobbers the match data
      (let* ((raw (match-string 1 block))
             (ctx (match-string 2 block))
             (header-end (match-end 0))
             ;; the next header (or the block end) closes this hunk
             (body-end (if (string-match
                            git-appraise-viewed--hunk-header-regexp
                            block header-end)
                           (match-beginning 0)
                         (length block))))
        (push (list :raw raw
                    :lines (cons (format "@@ @@%s" ctx)
                                 (split-string
                                  (substring block header-end body-end)
                                  "\n" t)))
              staged)
        (setq pos body-end)))
    (let* ((basic (nreverse
                   (mapcar (lambda (stg)
                             (let ((ranges (git-appraise-viewed--parse-ranges
                                            (plist-get stg :raw))))
                               (list :fp (git-appraise-viewed--fingerprint
                                          path (plist-get stg :lines))
                                     :range (git-appraise-viewed--range-key ranges)
                                     :sides (length ranges))))
                           staged)))
           (counts nil))
      (dolist (h basic)
        (cl-incf (alist-get (plist-get h :fp) counts 0 nil #'equal)))
      (mapcar (lambda (h)
                (list :fp (plist-get h :fp)
                      :range (plist-get h :range)
                      :sides (plist-get h :sides)
                      :dup (> (alist-get (plist-get h :fp) counts 0 nil
                                         #'equal)
                              1)))
              basic))))

(defun git-appraise-viewed--block-line-counts (block)
  "Return (ADDED DELETED) changed-line counts in raw diff BLOCK."
  (let ((added 0)
        (deleted 0))
    (--each (split-string block "\n" t)
      (cond
       ((string-prefix-p "+++" it))
       ((string-prefix-p "---" it))
       ((string-prefix-p "+" it) (cl-incf added))
       ((string-prefix-p "-" it) (cl-incf deleted))))
    (list added deleted)))

(defun git-appraise-viewed--inventory (diff)
  "Inventory the raw DIFF text for viewed identities.
Return a hash of canonical path -> plist:

  :has-hunks  BOOL        the file has at least one hunk
  :combined   BOOL        any hunk has more than two range sides
  :file-fp    FP|nil      whole-block hash, hunk-less files only
  :added      INTEGER     added content lines
  :deleted    INTEGER     deleted content lines
  :hunks      plist list  (:fp :range :sides :dup) per hunk
  :dup-fps    (FP...)     fingerprints occurring more than once here

Nil or empty DIFF yields an empty inventory."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (blk (and diff
                      (not (string-empty-p diff))
                      (git-appraise--diff--split-by-files diff)))
      (let* ((path (car blk))
             (block (cdr blk))
             (hunks (git-appraise-viewed--block-hunks path block))
             (counts (git-appraise-viewed--block-line-counts block))
             (dup-fps (-map (lambda (h) (plist-get h :fp))
                            (-filter (lambda (h) (plist-get h :dup))
                                     hunks))))
        (puthash
         path
         (list :has-hunks (not (null hunks))
               :combined (and hunks
                              (-any? (lambda (h)
                                       (/= (plist-get h :sides) 2))
                                     hunks))
               :file-fp (unless hunks
                          (git-appraise-viewed--file-fingerprint block))
               :added (car counts)
               :deleted (cadr counts)
               :hunks hunks
               :dup-fps dup-fps)
         table)))
    table))

;;; Reduction

(defun git-appraise-viewed--dedup (events)
  "Drop repeated event IDs from EVENTS, first occurrence wins."
  (let ((seen (make-hash-table :test #'equal))
        (out nil))
    (dolist (e events)
      (let ((id (a-get e 'id)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push e out))))
    (nreverse out)))

(defun git-appraise-viewed--later-p (cand winner)
  "Non-nil when event CAND supersedes event WINNER.
Last write wins by the ISO-8601 `at' timestamp (lexicographic ==
chronological for the canonical UTC form); equal timestamps fall
back to the record ID as a deterministic tie-break."
  (or (string-greaterp (a-get cand 'at) (a-get winner 'at))
      (and (string-equal (a-get cand 'at) (a-get winner 'at))
           (string-greaterp (a-get cand 'id) (a-get winner 'id)))))

(defun git-appraise-viewed--project (events actor inventory)
  "Reduce the stored EVENTS for ACTOR onto the current INVENTORY.
Return a hash of canonical path -> plist:

  :total        INTEGER  countable items: hunks, or 1 for a hunk-less file
  :viewed-ct    INTEGER  viewed items among :total
  :file         BOOL     explicit mark on a hunk-less file
  :all-hunks    BOOL     every current item viewed (file rollup)
  :changed      BOOL     was viewed before; its content changed since
  :hunk-viewed  (KEY...) range-keys of the viewed hunks (wash lookup)
  :hunk-changed (KEY...) hunks viewed before under other content

Events for other reviewers are ignored.  Ambiguous duplicate hunk
fingerprints only match a mark whose stored `range' still agrees
with the current range -- a mark is never guessed onto one of two
identical hunks.  A `file' event never matches a file that has
hunks."
  (let* ((mine (git-appraise-viewed--dedup
                (-filter (lambda (e)
                           (and actor (equal (a-get e 'actor) actor)))
                         events)))
         (winners (make-hash-table :test #'equal))
         (seen-fps (make-hash-table :test #'equal))
         (seen-ranges (make-hash-table :test #'equal))
         (index (make-hash-table :test #'equal)))
    (dolist (e mine)
      (let* ((path (a-get e 'path))
             (fp (a-get e 'fingerprint))
             (range (a-get e 'range))
             (info (gethash path inventory))
             (dup? (and info (member fp (plist-get info :dup-fps))))
             (key (if dup?
                      (and range (list path fp range))
                    (list path fp))))
        (when (equal (a-get e 'viewed) t)
          (cl-pushnew fp (gethash path seen-fps) :test #'equal)
          (when (and (equal (a-get e 'item) "hunk") range)
            (let ((entry (assoc range (gethash path seen-ranges))))
              (if entry
                  (cl-pushnew fp (cdr entry) :test #'equal)
                (push (list range fp) (gethash path seen-ranges))))))
        (when key
          (let ((winner (gethash key winners 'none)))
            (when (or (eq winner 'none)
                      (git-appraise-viewed--later-p e winner))
              (puthash key e winners))))))
    (maphash
     (lambda (path info)
       (let* ((hunks (plist-get info :hunks))
              (cur-fps (-map (lambda (h) (plist-get h :fp)) hunks))
              (has-hunks? (plist-get info :has-hunks))
              (total (if has-hunks? (length hunks) 1))
              (viewed-keys nil)
              (changed-keys nil)
              ;; content this reviewer saw that the current diff no
              ;; longer contains: the file was viewed, then changed
              (history (-filter (lambda (fp) (not (member fp cur-fps)))
                                (gethash path seen-fps)))
              (file-mark
               (and (not has-hunks?)
                    (let ((w (gethash (list path (plist-get info :file-fp))
                                      winners 'none)))
                      (and (not (eq w 'none))
                           (equal (a-get w 'viewed) t))))))
         (dolist (h hunks)
           (let* ((fp (plist-get h :fp))
                  (range (plist-get h :range))
                  (w (if (plist-get h :dup)
                         (gethash (list path fp range) winners 'none)
                       (gethash (list path fp) winners 'none)))
                  (viewed? (and (not (eq w 'none))
                                (equal (a-get w 'viewed) t))))
             (when viewed?
               (push range viewed-keys))
             ;; the same position was viewed under different content:
             ;; a soft "was viewed, changed" signal for this hunk
             (when (and (not viewed?)
                        (-any? (lambda (fp0) (not (member fp0 cur-fps)))
                               (cdr (assoc range
                                           (gethash path seen-ranges)))))
               (push range changed-keys))))
         (let ((viewed-ct (+ (length viewed-keys)
                             (if file-mark 1 0))))
           (puthash path
                    (list :total total
                          :viewed-ct viewed-ct
                          :file file-mark
                          :all-hunks (= viewed-ct total)
                          :changed (and history (/= viewed-ct total))
                          :hunk-viewed viewed-keys
                          :hunk-changed changed-keys)
                    index))))
     inventory)
    index))

(provide 'git-appraise-viewed-core)
;;; git-appraise-viewed-core.el ends here
