;;; git-appraise-metadata-test.el --- note transport tests -*- lexical-binding: t; -*-

;;; req-2026-11-24-001:
;; covers: git-appraise-metadata-read, git-appraise-metadata-append
;; covers-fn: git-appraise-metadata-encode
;; enforcement: test

(require 'ert)
(require 'a)
(require 'git-appraise-metadata)

(defun git-appraise-metadata-test--event (id item viewed &optional path)
  "Build a valid viewed event alist with ID, ITEM and VIEWED."
  `((v . 1)
    (type . "viewed")
    (id . ,id)
    (actor . "reviewer@example.com")
    (item . ,item)
    (path . ,(or path "src/a.el"))
    (fingerprint . ,(concat "sha256:" (make-string 64 ?a)))
    ,@(when (equal item "hunk") '((range . "-10,3 +10,4")))
    (viewed . ,(if viewed t :json-false))
    (at . "2026-01-01T00:00:00Z")))

;;; req-2026-11-24-001: note roundtrip
(ert-deftest metadata/req-2026-11-24-001-note-roundtrip ()
  "WHEN events are appended under the viewed notes ref THE SYSTEM SHALL return exactly those events on a later read."
  (git-appraise-test--with-git-repo
    (let* ((ref "refs/notes/git-appraise-el/viewed")
           (rev (git-appraise-test--git-out "rev-parse" "HEAD"))
           (events (list (git-appraise-metadata-test--event "h1" "hunk" t)
                         (git-appraise-metadata-test--event "f1" "file" nil))))
      (git-appraise-metadata-append git-appraise-test--repo-dir ref rev events)
      (pcase-let ((`(,read . ,bad)
                   (git-appraise-metadata-read git-appraise-test--repo-dir ref rev)))
        (should (= bad 0))
        (should (= (length read) 2))
        (should (equal (a-get (nth 0 read) 'id) "h1"))
        (should (eq (a-get (nth 0 read) 'viewed) t))
        (should (equal (a-get (nth 0 read) 'range) "-10,3 +10,4"))
        (should (equal (a-get (nth 1 read) 'id) "f1"))
        (should (eq (a-get (nth 1 read) 'viewed) :json-false))))))

;;; req-2026-11-24-002: missing note is empty, not an error
;; covers: git-appraise-metadata-read
;; enforcement: test
(ert-deftest metadata/req-2026-11-24-002-missing-note-empty ()
  "WHEN no note exists for the revision THE SYSTEM SHALL return an empty event list without signalling."
  (git-appraise-test--with-git-repo
    (pcase-let ((`(,read . ,bad)
                 (git-appraise-metadata-read
                  git-appraise-test--repo-dir
                  "refs/notes/git-appraise-el/viewed"
                  (git-appraise-test--git-out "rev-parse" "HEAD"))))
      (should (null read))
      (should (= bad 0)))))

;;; req-2026-11-24-003: append preserves prior lines
;; covers: git-appraise-metadata-append
;; enforcement: test
(ert-deftest metadata/req-2026-11-24-003-append-preserves-prior ()
  "WHEN a second batch is appended THE SYSTEM SHALL keep every line of the first batch."
  (git-appraise-test--with-git-repo
    (let* ((ref "refs/notes/git-appraise-el/viewed")
           (rev (git-appraise-test--git-out "rev-parse" "HEAD")))
      (git-appraise-metadata-append
       git-appraise-test--repo-dir ref rev
       (list (git-appraise-metadata-test--event "h1" "hunk" t)))
      (git-appraise-metadata-append
       git-appraise-test--repo-dir ref rev
       (list (git-appraise-metadata-test--event "h2" "hunk" nil)))
      (pcase-let ((`(,read . ,bad)
                   (git-appraise-metadata-read git-appraise-test--repo-dir ref rev)))
        (should (= bad 0))
        (should (equal (mapcar (lambda (e) (a-get e 'id)) read)
                       '("h1" "h2")))))))

;;; req-2026-11-24-004: malformed line isolated
;; covers: git-appraise-metadata--parse-note, git-appraise-metadata-validate
;; enforcement: test
(ert-deftest metadata/req-2026-11-24-004-malformed-line-isolated ()
  "WHEN the note holds malformed and future-version lines THE SYSTEM SHALL load the valid events and count the skipped ones."
  (git-appraise-test--with-git-repo
    (let* ((ref "refs/notes/git-appraise-el/viewed")
           (rev (git-appraise-test--git-out "rev-parse" "HEAD"))
           (future (cons '(v . 2)
                         (cdr (git-appraise-metadata-test--event "f2" "file" t)))))
      (git-appraise-metadata-append
       git-appraise-test--repo-dir ref rev
       (list (git-appraise-metadata-test--event "h1" "hunk" t)))
      (with-temp-file (expand-file-name "note.txt" default-directory)
        (insert "{\"v\":1,\"type\":\"viewed\",BROKEN\n"))
      (git-appraise-test--git-out
       "notes" (concat "--ref=" ref) "add" "-f" "-F" "note.txt" rev)
      ;; a valid event and an unknown future version appended after it:
      ;; the valid one must survive, the future one is skipped safely
      (with-temp-file (expand-file-name "future.txt" default-directory)
        (insert (git-appraise-metadata-encode
                 (git-appraise-metadata-test--event "f2" "file" t))
                "\n"
                (git-appraise-metadata-encode future) "\n"))
      (git-appraise-test--git-out
       "notes" (concat "--ref=" ref) "append" "-F" "future.txt" rev)
      (pcase-let ((`(,read . ,bad)
                   (git-appraise-metadata-read git-appraise-test--repo-dir ref rev)))
        (should (= bad 2))
        (should (equal (mapcar (lambda (e) (a-get e 'id)) read) '("f2")))))))

;;; req-2026-11-24-005: write failure reported with stderr
;; covers: git-appraise-metadata-append
;; enforcement: test
(ert-deftest metadata/req-2026-11-24-005-write-failure-reported ()
  "WHEN the note write fails THE SYSTEM SHALL signal an error carrying git's stderr."
  (git-appraise-test--with-git-repo
    (let ((err (should-error
                (git-appraise-metadata-append
                 git-appraise-test--repo-dir
                 "refs/notes/git-appraise-el/viewed" "deadbeef"
                 (list (git-appraise-metadata-test--event "h1" "hunk" t))))))
      (should (string-match-p "failed" (error-message-string err)))
      (should (string-match-p "deadbeef\\|resolve" (error-message-string err))))))

;;; req-2026-11-24-006: ref isolation
;; covers: git-appraise-metadata-append
;; enforcement: test
(ert-deftest metadata/req-2026-11-24-006-ref-isolation ()
  "WHEN metadata is written THE SYSTEM SHALL create only the configured notes ref, never any refs/notes/devtools ref."
  (git-appraise-test--with-git-repo
    (let* ((ref "refs/notes/git-appraise-el/viewed")
           (rev (git-appraise-test--git-out "rev-parse" "HEAD")))
      (git-appraise-metadata-append
       git-appraise-test--repo-dir ref rev
       (list (git-appraise-metadata-test--event "h1" "hunk" t)))
      (let ((refs (git-appraise-test--git-out "for-each-ref" "refs/notes")))
        (should (string-match-p (regexp-quote ref) refs))
        (should-not (string-match-p "refs/notes/devtools" refs))))))
