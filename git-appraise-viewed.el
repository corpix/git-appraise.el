;;; git-appraise-viewed.el --- persistent viewed state for review diffs -*- lexical-binding: t; -*-

;; Viewed marks for files and hunks (2026-11-24 viewed-state plan),
;; backed by personal git notes (`git-appraise-metadata.el').
;;
;; Identity layer: every hunk gets a content fingerprint (canonical
;; path + hunk body with the @@ range numbers normalized away), every
;; hunk-less diff block (binary, mode, rename) gets a fingerprint of
;; its whole block.  A mark survives line shifts and dies with the
;; content it marked.  The inventory and the owned diff washer go
;; through the same parse/serialize helpers, so they can never
;; disagree about identities.
;;
;; Projection layer: reduces the note's event lines (last write wins
;; per reviewer, item and fingerprint) onto the current diff's
;; inventory.  Everything this file computes is a cache of the note,
;; never the authoritative copy.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'a)
(require 'dash)
(require 'magit-section)
(require 'uuidgen)
(require 'git-appraise-metadata)
(require 'git-appraise-time)
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
  "Reduce the note's EVENTS for ACTOR onto the current INVENTORY.
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

;;; Reviewer identity

(defun git-appraise-viewed--actor (repo)
  "The reviewer identity for REPO: its configured author email.
Nil when unset -- marks are then not loaded or written, rather
than mixing every user under an anonymous ID."
  (let ((email (git-appraise-utils--git repo "config" "user.email")))
    (and email (not (string-empty-p email)) email)))

;;; Load boundary and render integration

(defvar git-appraise-viewed--events-cache nil
  "Alist ((REPO-DIR . REVISION) . EVENTS) of loaded note lines.
A cache of the git note, never the authoritative copy:
`git-appraise-backend-refresh' invalidates it so a reload re-reads
the note, and the mark commands update it only after a successful
write.")

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
  "The review revision the notes are anchored on, or nil."
  (ignore-errors (oref (git-appraise-db-get-pullreq) number)))

(defun git-appraise-viewed--key (repo revision)
  "The events-cache key for REPO and REVISION.
The repo path is canonicalized: callers pass the worktree with a
trailing slash (`git-appraise-viewed--repo-dir') and the backend's
owner without, and both must hit the same cache entry."
  (cons (directory-file-name (expand-file-name repo)) revision))

(defun git-appraise-viewed--invalidate (repo revision)
  "Drop the cached note lines for REPO and REVISION."
  (let ((key (git-appraise-viewed--key repo revision)))
    (setq git-appraise-viewed--events-cache
          (--filter (not (equal (car it) key))
                    git-appraise-viewed--events-cache))))

(defun git-appraise-viewed--load (repo revision)
  "The note lines for REPO and REVISION, read once into the cache.
A missing note is no events.  A git failure is logged with a
specific diagnostic and yields no events: a malformed note must
never masquerade as a provider error."
  (let ((key (git-appraise-viewed--key repo revision)))
    (if-let* ((hit (assoc key git-appraise-viewed--events-cache #'equal)))
        (cdr hit)
      (let ((read (condition-case err
                      (git-appraise-metadata-read
                       repo git-appraise-viewed-notes-ref revision)
                    (error
                     (git-appraise-utils--log
                      "git-appraise-viewed--load" (prin1-to-string err))
                     (message "Viewed note unreadable (%s); showing all unviewed"
                              (error-message-string err))
                     (cons nil 0)))))
        (when (> (cdr read) 0)
          (message "Viewed note: %d unreadable records ignored" (cdr read)))
        (push (cons key (car read)) git-appraise-viewed--events-cache)
        (car read)))))

(defun git-appraise-viewed--refresh (repo revision)
  "Re-read the note for REPO and REVISION on the next projection."
  (git-appraise-viewed--invalidate repo revision)
  (git-appraise-viewed--load repo revision))

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
    (viewed . ,(if viewed t :json-false))
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
        ;; reface only heading content carrying git-appraise-path; tags
        ;; (NEW/viewed/changed/comments) are appended outside that property.
        (let ((p bol))
          (while (< p eol)
            (let ((f (get-text-property p 'font-lock-face))
                  (next (next-single-property-change p 'font-lock-face nil eol)))
              (when (and (get-text-property p 'git-appraise-path)
                         (memq f '(magit-diff-file-heading git-appraise-dimmed)))
                (put-text-property p next 'font-lock-face face))
              (setq p next))))
        ;; rebuild viewed/changed tags, preserving comments badge.
        (let* ((line (buffer-substring-no-properties bol (line-end-position)))
               (vc-match (string-match "\\(  \\[viewed\\]\\|  \\[CHANGED\\]\\)+" line))
               (comments-match (string-match "  \\[comments [0-9]+\\]" line))
               (insert-at (cond
                           (vc-match (+ bol (match-beginning 0)))
                           (comments-match (+ bol (match-beginning 0)))
                           (t (line-end-position)))))
          (when vc-match
            (delete-region (+ bol (match-beginning 0))
                           (+ bol (match-end 0))))
          (goto-char (min insert-at (line-end-position)))
          (let ((sec-prop (get-text-property (line-beginning-position) 'magit-section))
                (tag-start (point)))
            (when (plist-get info :viewed)
              (insert (propertize git-appraise-section--viewed-tag
                                  'font-lock-face 'git-appraise-dimmed)))
            (when (plist-get info :changed)
              (insert (propertize git-appraise-section--changed-tag
                                  'font-lock-face 'git-appraise-dimmed)))
            (when sec-prop
              (put-text-property tag-start (point) 'magit-section sec-prop))))))))

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
        ((magit-file-section-p sec)
         (when-let* ((path (git-appraise-viewed--file-path sec)))
           (git-appraise-viewed--apply-file-heading-in-place sec path)))
        ((magit-hunk-section-p sec)
         (let* ((file (oref sec parent))
                (path (and (magit-file-section-p file)
                           (git-appraise-viewed--file-path file))))
           (when path
             (git-appraise-viewed--apply-hunk-heading-in-place sec path)))))))))

(defun git-appraise-viewed--write-events (path items viewed)
  "Append one event per (FP . RANGE) in ITEMS for PATH, then update UI.
The in-memory index is updated only after the git write succeeds;
a failed write leaves the rendered state unchanged and reports the
git error."
  (let* ((repo (git-appraise-viewed--repo-dir))
         (revision (git-appraise-viewed--revision))
         (actor (and repo (git-appraise-viewed--actor repo))))
    (cond ((not (and repo revision))
           (user-error "No review repository context to store viewed marks"))
          ((not actor)
           (user-error "Set git user.email in %s to store viewed marks" repo))
          (t
           (condition-case err
               (git-appraise-metadata-append
                repo git-appraise-viewed-notes-ref revision
                (--map (git-appraise-viewed--make-event
                        actor path (car it) (cdr it) viewed)
                       items))
             (error (user-error "Could not save the viewed mark: %s"
                                (error-message-string err))))
           (git-appraise-viewed--invalidate repo revision)
           (git-appraise-viewed--load repo revision)
           (git-appraise-viewed--reproject)
           (git-appraise-viewed--apply-in-place)))))

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
the whole file when point is on a file heading."
  (interactive)
  (let* ((sec (magit-current-section))
         (file (and sec (git-appraise-eldoc--file-section sec))))
    (cond
     ((and file (eq sec file))
      (git-appraise-viewed--toggle-file file))
     ((git-appraise-viewed--current-hunk)
      (git-appraise-viewed--toggle-hunk))
     (file (git-appraise-viewed--toggle-file file))
     (t (user-error "No file or hunk at point to mark viewed")))))

(defun git-appraise-viewed-toggle-file ()
  "Toggle the viewed mark for the whole file at or around point."
  (interactive)
  (if-let* ((file (git-appraise-eldoc--file-section (magit-current-section))))
      (git-appraise-viewed--toggle-file file)
    (user-error "No file at point to mark viewed")))

(provide 'git-appraise-viewed)
;;; git-appraise-viewed.el ends here
