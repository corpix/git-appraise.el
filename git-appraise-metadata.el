;;; git-appraise-metadata.el --- Git notes transport for review metadata -*- lexical-binding: t; -*-

;; Storage adapter for persistent review metadata (2026-11-24
;; viewed-state plan): newline-delimited JSON records in a git notes
;; ref, one note per review revision.  Personal by default: the
;; default ref lives outside the `refs/notes/devtools/*' pattern that
;; `git-appraise push/pull' carries, so marks are never published
;; silently.  Parsing, validation and encoding are pure functions;
;; this file owns the process execution only (`call-process', never a
;; shell), and is independent of Magit sections.

;;; Code:

(require 'json)
(require 'a)

(defcustom git-appraise-viewed-notes-ref "refs/notes/git-appraise-el/viewed"
  "The git notes ref review metadata events are stored under.
Personal by default: ordinary `git-appraise push' does not carry
this ref.  Pointing it at a ref inside the `refs/notes/devtools/*'
push/pull pattern would publish marks for every collaborator --
do that only as part of an explicitly designed sync feature."
  :group 'git-appraise
  :type 'string)

(defconst git-appraise-metadata-version 1
  "The record envelope version this reader understands.
Higher versions are skipped safely, never misparsed.")

(defconst git-appraise-metadata--fingerprint-regexp
  (concat "^sha256:[0-9a-f]\\{" (number-to-string (* 2 32)) "\\}$")
  "A well-formed sha256 content fingerprint.")

(defun git-appraise-metadata--git (repo args)
  "Run git ARGS in REPO without a shell.
Return (CODE STDOUT STDERR), all strings stripped of text
properties so the values are safe to store later.  stderr goes to
a temp file: in a DESTINATION list, `call-process' accepts only
the first element as a live buffer."
  (let ((out (generate-new-buffer " *git-appraise-metadata out*"))
        (err-file (make-temp-file "git-appraise-metadata-err")))
    (unwind-protect
        (let ((code (apply #'call-process "git" nil (list out err-file) nil
                           (append (list "-C" (expand-file-name repo)) args))))
          (list code
                (with-current-buffer out
                  (buffer-substring-no-properties (point-min) (point-max)))
                (with-temp-buffer
                  (insert-file-contents err-file)
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (ignore-errors (kill-buffer out))
      (ignore-errors (delete-file err-file)))))

(defun git-appraise-metadata-validate (obj)
  "Validate one metadata record OBJ (an alist from `json-read').
Return (t . nil) when the record is valid, (nil . REASON)
otherwise.  Unknown types and future versions report a reason too,
so the reader can skip them safely instead of misparsing them."
  (cond
   ((not (and (listp obj) (a-get obj 'type)))
    (cons nil "not a JSON object"))
   ((not (and (numberp (a-get obj 'v))
              (= (a-get obj 'v) git-appraise-metadata-version)))
    (cons nil (format "unsupported version %S" (a-get obj 'v))))
   ((not (equal (a-get obj 'type) "viewed"))
    (cons nil (format "unknown type %S" (a-get obj 'type))))
   ((not (member (a-get obj 'item) '("hunk" "file")))
    (cons nil (format "unknown item %S" (a-get obj 'item))))
   ((not (and (stringp (a-get obj 'path))
              (not (string-empty-p (a-get obj 'path)))))
    (cons nil "missing path"))
   ((not (and (stringp (a-get obj 'fingerprint))
              (string-match-p git-appraise-metadata--fingerprint-regexp
                              (a-get obj 'fingerprint))))
    (cons nil "bad fingerprint"))
   ((not (memq (a-get obj 'viewed) '(t :json-false)))
    (cons nil "bad viewed flag"))
   ((not (and (stringp (a-get obj 'at))
              (not (string-empty-p (a-get obj 'at)))))
    (cons nil "missing timestamp"))
   ((not (and (stringp (a-get obj 'id))
              (not (string-empty-p (a-get obj 'id)))))
    (cons nil "missing id"))
   ((and (equal (a-get obj 'item) "hunk")
         (a-get obj 'range)
         (not (stringp (a-get obj 'range))))
    (cons nil "bad range"))
   (t (cons t nil))))

(defun git-appraise-metadata-encode (event)
  "Encode one validated EVENT alist as a single JSON line.
Events must stay one line each: `git notes' merges by
cat_sort_uniq over lines, so an embedded newline would corrupt a
shared merge.  `json-encode' escapes string newlines; the check
below is a safety net for hand-built events."
  (let ((encoded (json-encode event)))
    (when (string-match-p "\n" encoded)
      (error "metadata record would span multiple lines"))
    encoded))

(defun git-appraise-metadata--parse-note (text)
  "Parse the note TEXT into (EVENTS . BAD).
EVENTS are the validated record alists in note order; BAD counts
every skipped line (malformed JSON or a failed validation).  One
bad line never prevents the other lines from loading."
  (let ((events nil)
        (bad 0))
    ;; `git notes append' joins batches with a blank line; OMIT-NULLS
    ;; turns those separators into nothing instead of empty records.
    (dolist (line (split-string text "\n" t))
      (condition-case _
          (let ((obj (with-temp-buffer
                       (insert line)
                       (let ((json-object-type 'alist)
                             (json-array-type 'list))
                         (goto-char (point-min))
                         (json-read)))))
            (pcase (git-appraise-metadata-validate obj)
              (`(t . ,_) (push obj events))
              (`(nil . ,_) (setq bad (1+ bad)))))
        (error (setq bad (1+ bad)))))
    (cons (nreverse events) bad)))

(defun git-appraise-metadata-read (repo ref revision)
  "Read the note for REVISION under notes REF from REPO.
Return (EVENTS . BAD) per `git-appraise-metadata--parse-note'.  A
missing note is (nil . 0), git's normal exit 1 report; any other
failure signals an error carrying git's stderr."
  (pcase-let* ((`(,code ,stdout ,stderr)
                (git-appraise-metadata--git
                 repo (list "notes" (concat "--ref=" ref) "show" revision))))
    (cond ((zerop code)
           (git-appraise-metadata--parse-note stdout))
          ((and (= code 1) (string-match-p "no note found" stderr))
           (cons nil 0))
          (t (error "git notes show %s failed: %s"
                    revision (string-trim stderr))))))

(defun git-appraise-metadata-append (repo ref revision events)
  "Append EVENTS to the note for REVISION under notes REF in REPO.
One git invocation for the whole batch (a file-level viewed action
writes one commit, not one per hunk).  Events are appended, never
rewritten: older lines survive, an unview action is a new
`viewed:false' line.  Signal an error carrying git's stderr when
the write fails."
  (when events
    (let ((file (make-temp-file "git-appraise-metadata")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert (mapconcat #'git-appraise-metadata-encode events "\n"))
              (insert ?\n))
            (pcase-let ((`(,code ,_ ,stderr)
                         (git-appraise-metadata--git
                          repo (list "notes" (concat "--ref=" ref)
                                     "append" "-F" file revision))))
              (unless (zerop code)
                (error "git notes append %s failed: %s"
                       revision (string-trim stderr)))))
        (delete-file file))))
  t)

(provide 'git-appraise-metadata)
;;; git-appraise-metadata.el ends here
