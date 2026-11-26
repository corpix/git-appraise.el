;;; git-appraise-store-import-test.el --- git notes import tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'a)
(require 'json)
(require 'git-appraise-test-helpers)
(require 'git-appraise-backend)
(require 'git-appraise-pr-state)
(require 'git-appraise-store)
(require 'git-appraise-store-import)

(defun git-appraise-store-import-test--json-lines (objs)
  "Encode OBJS as one JSON line each."
  (let ((json-false :json-false))
    (mapconcat #'json-encode objs "\n")))

(defun git-appraise-store-import-test--event (id viewed &optional range at)
  "A valid legacy viewed event."
  `((v . 1) (type . "viewed") (id . ,id)
    (actor . "tester@example.com")
    (item . ,(if range "hunk" "file"))
    (path . "src/a.el")
    (fingerprint . ,(concat "sha256:" (make-string 64 ?a)))
    ,@(when range `((range . ,range)))
    (viewed . ,(if viewed t :json-false))
    (at . ,(or at "2026-01-01T00:00:00Z"))))

(defun git-appraise-store-import-test--fold (id folded &optional range)
  "A valid legacy fold event."
  `((v . 1) (type . "fold") (id . ,id)
    (actor . "tester@example.com")
    (item . ,(if range "hunk" "file"))
    (path . "src/a.el")
    (fingerprint . ,(concat "sha256:" (make-string 64 ?a)))
    ,@(when range `((range . ,range)))
    (folded . ,(if folded t :json-false))
    (at . "2026-01-01T00:00:00Z")))

(defun git-appraise-store-import-test--pr (id feedback v pr &optional actor)
  "A valid legacy pr-state snapshot."
  `((v . ,v) (type . "pr-state") (id . ,id)
    (actor . ,(or actor "tester@example.com"))
    (pr . ,pr)
    (feedback . ,(or feedback :json-false))
    (local_comments . [])
    (at . "2026-01-01T00:00:00Z")))

(defun git-appraise-store-import-test--write-note (repo ref rev text)
  "Write TEXT as REPO's note for REV under notes REF."
  (let ((file (make-temp-file "git-appraise-import-test")))
    (with-temp-file file (insert text "\n"))
    (git-appraise-test--git-out
     "-C" repo "notes" (concat "--ref=" ref) "add" "-f" "-F" file rev)
    (delete-file file)))

(defun git-appraise-store-import-test--legacy-anchor (repo identity)
  "The synthetic v1 notes anchor for IDENTITY, a blob in REPO."
  (let ((file (make-temp-file "git-appraise-import-test-anchor")))
    (with-temp-file file
      (insert (format "git-appraise-pr-state:v1:%s" identity)))
    (prog1 (git-appraise-test--git-out "-C" repo "hash-object" "-w" file)
      (delete-file file))))

(defun git-appraise-store-import-test--refs (repo pattern)
  "REPO's refs matching PATTERN, newline separated."
  (git-appraise-test--git-out "-C" repo "for-each-ref" pattern
                              "--format=%(refname)"))

(defun git-appraise-store-import-test--plan-refs (plan)
  "The refs planned in PLAN."
  (--map (car it) plan))

;;; req-2026-11-26-020: move then delete
;; covers: git-appraise-store-import-read git-appraise-store-import-apply
;; covers-fn: git-appraise-store-import--notes git-appraise-store-import--ref-plan
;; enforcement: test
(ert-deftest import/req-2026-11-26-020-move-records-then-delete-refs ()
  "WHEN the import runs THE SYSTEM SHALL move every personal record into the store, delete exactly those notes refs, and record each ref's pre-delete tip."
  (git-appraise-test--with-git-repo
    (git-appraise-test--with-store
      (let* ((repo git-appraise-test--repo-dir)
             (rev (git-appraise-test--git-out "-C" repo "rev-parse" "HEAD"))
             (identity (format "/tmp/where/repo#%s" rev))
             (legacy-anchor
              (git-appraise-store-import-test--legacy-anchor repo identity))
             (legacy-only "/tmp/where/repo#legacy-only-rev")
             (legacy-only-anchor
              (git-appraise-store-import-test--legacy-anchor repo legacy-only))
             (viewed-ref (nth 0 git-appraise-store-import--refs))
             (fold-ref (nth 1 git-appraise-store-import--refs))
             (pr-ref (nth 2 git-appraise-store-import--refs)))
        ;; a foreign ref the import must never touch
        (git-appraise-test--git-out
         "-C" repo "notes" "--ref=refs/notes/devtools/reviews"
         "add" "-f" "-m" "keep" rev)
        (git-appraise-store-import-test--write-note
         repo viewed-ref rev
         (git-appraise-store-import-test--json-lines
          (list (git-appraise-store-import-test--event "h1" t "-7,6 +8,7")
                (git-appraise-store-import-test--event
                 "f1" nil nil "2026-01-01T00:00:01Z"))))
        (git-appraise-store-import-test--write-note
         repo fold-ref rev
         (git-appraise-store-import-test--json-lines
          (list (git-appraise-store-import-test--fold "fd1" t))))
        ;; v2 snapshot at the review revision, v1 snapshots at the
        ;; legacy synthetic anchors
        (git-appraise-store-import-test--write-note
         repo pr-ref rev
         (git-appraise-store-import-test--json-lines
          (list (git-appraise-store-import-test--pr
                 "s1" "Current" 2 identity))))
        (git-appraise-store-import-test--write-note
         repo pr-ref legacy-anchor
         (git-appraise-store-import-test--json-lines
          (list (git-appraise-store-import-test--pr "s0" "Old" 1 identity)
                (git-appraise-store-import-test--pr
                 "s2" "Legacy only" 1 legacy-only))))
        (let ((plan (git-appraise-store-import-read repo)))
          ;; all three refs planned, with their current tips
          (should (equal (git-appraise-store-import-test--plan-refs plan)
                         git-appraise-store-import--refs))
          (dolist (entry plan)
            (should (equal (cadr entry)
                           (git-appraise-test--git-out
                            "-C" repo "rev-parse" (car entry)))))
          ;; 2 viewed + 1 fold + 3 draft snapshots (two anchors)
          (should (equal (--map (length (nth 3 it)) plan) '(2 1 3)))
          (pcase-let ((`(,stored . ,deleted)
                       (git-appraise-store-import-apply repo plan)))
            (should (= stored 6))
            (should (= deleted 3)))
          ;; only the personal refs are gone; devtools survives
          (should (equal (git-appraise-store-import-test--refs
                          repo "refs/notes/git-appraise-el")
                         ""))
          (should (string-match-p
                   "refs/notes/devtools/reviews"
                   (git-appraise-store-import-test--refs repo "refs/notes")))
          ;; the store serves the moved records
          (should (equal (--map (a-get it 'id)
                                (git-appraise-store-events repo rev 'viewed))
                         '("h1" "f1")))
          (should (eq (a-get (car (git-appraise-store-events repo rev 'viewed))
                              'viewed)
                      t))
          (should (equal (a-get (car (git-appraise-store-events repo rev 'fold))
                                'id)
                         "fd1"))
          ;; the review-revision snapshot (v2) wins over the legacy one
          (should (equal (a-get (git-appraise-store-pr-state
                                 repo rev "tester@example.com")
                                'feedback)
                         "Current"))
          ;; a pr that only ever had a legacy v1 record still restores
          (should (equal (a-get (git-appraise-store-pr-state
                                 repo "legacy-only-rev" "tester@example.com")
                                'feedback)
                         "Legacy only"))
          ;; each ref's pre-delete tip is recorded for recovery
          (dolist (entry plan)
            (should (equal (git-appraise-store-import--recorded-tip
                            (git-appraise-store-db)
                            (git-appraise-store-repo-key repo) (car entry))
                           (cadr entry)))))))))


;;; req-2026-11-26-021: idempotent import
;; covers: git-appraise-store-import-read git-appraise-store-import--recorded-tip
;; enforcement: test
(ert-deftest import/req-2026-11-26-021-imported-refs-not-reimported ()
  "WHEN a notes ref was already imported THE SYSTEM SHALL neither import nor delete it again, even after the ref reappears."
  (git-appraise-test--with-git-repo
    (git-appraise-test--with-store
      (let* ((repo git-appraise-test--repo-dir)
             (rev (git-appraise-test--git-out "-C" repo "rev-parse" "HEAD"))
             (ref (nth 0 git-appraise-store-import--refs)))
        (git-appraise-store-import-test--write-note
         repo ref rev
         (git-appraise-store-import-test--json-lines
          (list (git-appraise-store-import-test--event "h1" t))))
        (pcase-let ((`(,stored . ,deleted)
                     (git-appraise-store-import-apply
                      repo (git-appraise-store-import-read repo))))
          (should (= stored 1))
          (should (= deleted 1)))
        ;; refs are gone: nothing left to plan
        (should (null (git-appraise-store-import-read repo)))
        ;; a restored ref is recognized as already imported
        (let ((tip (caar (sqlite-select
                          (git-appraise-store-db)
                          (car (git-appraise-sql-select
                                :from 'legacy_imports :columns '(tip)
                                :limit 1))))))
          (git-appraise-test--git-out "-C" repo "update-ref" ref tip)
          (should (null (git-appraise-store-import-read repo)))
          (should (string-match-p
                   (regexp-quote ref)
                   (git-appraise-store-import-test--refs repo "refs/notes"))))))))

;;; req-2026-11-26-022: failed import keeps notes
;; covers: git-appraise-store-import-apply git-appraise-store--call-in-write
;; enforcement: test
(ert-deftest import/req-2026-11-26-022-failed-import-keeps-notes ()
  "WHEN storing the records fails THE SYSTEM SHALL leave the notes refs untouched and the import recorded nowhere."
  (git-appraise-test--with-git-repo
    (git-appraise-test--with-store
      (let* ((repo git-appraise-test--repo-dir)
             (rev (git-appraise-test--git-out "-C" repo "rev-parse" "HEAD"))
             (ref (nth 0 git-appraise-store-import--refs)))
        (git-appraise-store-import-test--write-note
         repo ref rev
         (git-appraise-store-import-test--json-lines
          (list (git-appraise-store-import-test--event "h1" t))))
        (let ((plan (git-appraise-store-import-read repo)))
          (cl-letf (((symbol-function 'git-appraise-store--insert-chunked)
                     (lambda (&rest _) (error "write failed"))))
            (should-error (git-appraise-store-import-apply repo plan)
                          :type 'error))
          ;; the note survives and nothing was recorded
          (should (string-match-p
                   (regexp-quote ref)
                   (git-appraise-store-import-test--refs repo "refs/notes")))
          (should (null (git-appraise-store-import--recorded-tip
                         (git-appraise-store-db)
                         (git-appraise-store-repo-key repo) ref)))
          ;; the retry succeeds
          (pcase-let ((`(,stored . ,deleted)
                       (git-appraise-store-import-apply repo plan)))
            (should (= stored 1))
            (should (= deleted 1))))))))

;;; req-2026-11-24-004: malformed line isolated
;; covers: git-appraise-store-import--parse git-appraise-store-import--viewed-p
;; enforcement: test
(ert-deftest import/req-2026-11-24-004-malformed-line-isolated ()
  "WHEN a note holds malformed and future-version lines THE SYSTEM SHALL import the valid records and count the skipped ones."
  (git-appraise-test--with-git-repo
    (git-appraise-test--with-store
      (let* ((repo git-appraise-test--repo-dir)
             (rev (git-appraise-test--git-out "-C" repo "rev-parse" "HEAD"))
             (ref (nth 0 git-appraise-store-import--refs)))
        (git-appraise-store-import-test--write-note
         repo ref rev
         (concat
          (git-appraise-store-import-test--json-lines
           (list (git-appraise-store-import-test--event "h1" t)))
          "\n{\"v\":1,\"type\":\"viewed\",BROKEN\n"
          (git-appraise-store-import-test--json-lines
           (list `((v . 2) ,@(cdr (git-appraise-store-import-test--event
                                   "f2" t)))))
          "\n"))
        (let ((plan (git-appraise-store-import-read repo)))
          (should (= (length plan) 1))
          (pcase-let ((`(,_ref ,_tip ,_kind ,rows ,skipped) (car plan)))
            (should (= (length rows) 1))
            (should (= skipped 2)))
          (pcase-let ((`(,stored . ,deleted)
                       (git-appraise-store-import-apply repo plan)))
            (should (= stored 1))
            (should (= deleted 1)))
          (should (equal (--map (a-get it 'id)
                                (git-appraise-store-events repo rev 'viewed))
                         '("h1"))))))))

;;; req-2026-11-24-006: ref isolation
;; covers: git-appraise-store-append-events git-appraise-store-save-pr-state
;; enforcement: test
(ert-deftest import/req-2026-11-24-006-storing-creates-no-refs ()
  "WHEN personal review state is stored THE SYSTEM SHALL not create or modify any git ref."
  (git-appraise-test--with-git-repo
    (git-appraise-test--with-store
      (let* ((repo git-appraise-test--repo-dir)
             (rev (git-appraise-test--git-out "-C" repo "rev-parse" "HEAD")))
        (git-appraise-store-append-events
         repo rev 'viewed
         (list `((id . "h1") (actor . "tester@example.com")
                 (item . "file") (path . "src/a.el")
                 (fingerprint . ,(concat "sha256:" (make-string 64 ?a)))
                 (range . nil) (viewed . t) (at . "2026-01-01T00:00:00Z"))))
        (git-appraise-store-save-pr-state
         repo rev "tester@example.com"
         `((v . 2) (id . "s1") (feedback . "later") (local_comments . nil)
           (at . "2026-01-02T00:00:00Z")))
        (should (equal (git-appraise-store-import-test--refs repo "refs/notes")
                       ""))))))

;;; req-2026-11-26-023: legacy drafts recover through hydration
;; covers: git-appraise-pr-state-hydrate git-appraise-pr-state-persist
;; enforcement: test
(ert-deftest import/req-2026-11-26-023-legacy-feedback-recovers-and-is-superseded ()
  "WHEN unsent feedback exists only as a legacy v1 record THE SYSTEM SHALL restore it privately on reopen, and new saves supersede it."
  (git-appraise-test--with-git-repo
    (git-appraise-test--with-store
      (let* ((directory git-appraise-test--repo-dir)
             (rev (git-appraise-test--git-out "-C" directory "rev-parse" "HEAD"))
             (identity (format "%s#%s" (directory-file-name directory) rev))
             (pr-ref (nth 2 git-appraise-store-import--refs))
             (anchor (git-appraise-store-import-test--legacy-anchor
                      directory identity)))
        (git-appraise-store-import-test--write-note
         directory pr-ref anchor
         (git-appraise-store-import-test--json-lines
          (list (git-appraise-store-import-test--pr "s0" "Old feedback" 1
                                                    identity
                                                    "reviewer@example.com"))))
        (git-appraise-store-import-apply
         directory (git-appraise-store-import-read directory))
        (let ((pr (git-appraise-db--pullreq-create
                   (git-appraise-backend-repo
                    :id "import-pr" :owner directory :repo "repo"
                    :number rev :url "appraise://rev"))))
          (should (equal (oref pr feedback) "Old feedback"))
          ;; a new save supersedes the legacy record
          (oset pr feedback "Updated feedback")
          (git-appraise-pr-state-persist pr t)
          (should (equal (a-get (git-appraise-store-pr-state
                                 directory rev "reviewer@example.com")
                                'feedback)
                         "Updated feedback")))))))

(provide 'git-appraise-store-import-test)
;;; git-appraise-store-import-test.el ends here
