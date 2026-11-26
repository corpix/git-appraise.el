;;; git-appraise-test-helpers.el --- Shared helpers for git-appraise ERT tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'uuidgen)
(require 'a)
(require 'magit-section)
(require 'ert)
(require 'git-appraise-db)

(setq-default git-appraise-analysis-deferred? nil)

(defmacro git-appraise-test--with-store (&rest body)
  "Run BODY against a fresh temporary store database file."
  (declare (indent 0))
  `(let ((git-appraise-store-file
          (make-temp-file "git-appraise-test-store-" nil ".sqlite")))
     (unwind-protect
         (progn
           (git-appraise-store-close)
           ,@body)
       (git-appraise-store-close)
       (ignore-errors (delete-file git-appraise-store-file)))))

(defun git-appraise-test--reset-db (&optional _file)
  "Reset the review stores.
Clears the in-memory objects and closes the persistent store
connection, so a reopen reads the database file from scratch."
  (clrhash git-appraise-db--objects)
  (setq git-appraise-db--pullreq-id nil)
  (when (fboundp 'git-appraise-store-close)
    (git-appraise-store-close)))

(defmacro git-appraise-test--with-db (&rest body)
  "Run BODY with a fresh in-memory review store and state database."
  (declare (indent 0))
  `(git-appraise-test--with-store
     (git-appraise-test--reset-db)
     (unwind-protect
         (progn ,@body)
       (git-appraise-test--reset-db))))

(defvar git-appraise-test--repo-dir)

(defun git-appraise-test--git-out (&rest args)
  "Run git ARGS in `default-directory'; return trimmed stdout."
  (with-temp-buffer
    (unless (zerop (apply #'call-process "git" nil t nil args))
      (ert-fail (format "git %s failed:\n%s"
                        (string-join args " ")
                        (buffer-substring-no-properties (point-min) (point-max)))))
    (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defmacro git-appraise-test--with-git-repo (&rest body)
  "Run BODY in a fresh disposable git repository.
Binds `git-appraise-test--repo-dir' and `default-directory' to the
repo, which has user.name/email configured and one deterministic
commit on master.  The directory is deleted afterwards; the user's
repositories and notes are never touched."
  (declare (indent 0))
  `(let ((git-appraise-test--repo-dir (make-temp-file "git-appraise-test-repo" t)))
     (unwind-protect
         (let ((default-directory
                 (file-name-as-directory git-appraise-test--repo-dir)))
           (git-appraise-test--git-out "init" "-q" "-b" "master")
           (git-appraise-test--git-out "config" "user.email" "reviewer@example.com")
           (git-appraise-test--git-out "config" "user.name" "Reviewer")
           (write-region "one\n" nil (expand-file-name "f.txt" default-directory))
           (let ((process-environment
                  (cons "GIT_AUTHOR_DATE=2026-01-01T00:00:00Z"
                        (cons "GIT_COMMITTER_DATE=2026-01-01T00:00:00Z"
                              process-environment))))
             (git-appraise-test--git-out "add" ".")
             (git-appraise-test--git-out "commit" "-q" "-m" "init"))
           ,@body)
       (delete-directory git-appraise-test--repo-dir 'recursive))))

(defun git-appraise-test--drain-lazy (buffer)
  "Pump deferred timers until BUFFER's lazy wash and chunk loader finish."
  (with-current-buffer buffer
    (let ((deadline (+ (float-time) 30)))
      (while (or git-appraise-lazy--wash-pos git-appraise-lazy--queue
                 deferred:queue)
        (when (> (float-time) deadline)
          (ert-fail "lazy render never drained"))
        (sit-for 0.05)))))

(defun git-appraise-test--sections-match (insert-fn expected &optional buffer-empty?)
  "Run INSERT-FN in a temp buffer, match sections against EXPECTED."
  (with-temp-buffer
    (funcall insert-fn)
    (let ((count 0))
      (magit-wash-sequence
       (lambda ()
         (when-let (section (magit-current-section))
           (with-slots (type value) section
             (let ((rule (nth count expected)))
               (should (eq (a-get rule 'type) type))
               (should (equal (a-get rule 'value) value)))
             (setq count (1+ count))))
         (magit-section-forward-sibling)))
      (if buffer-empty?
          (should (string-empty-p (buffer-string)))
        (should (string-match-p "[[:word:]]" (buffer-string)))))))

(defun git-appraise-test--run (directory program &rest args)
  "Run PROGRAM with ARGS in DIRECTORY and return trimmed stdout."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((status (apply #'process-file program nil t nil args)))
        (unless (and (integerp status) (zerop status))
          (error "%s %S failed: %s" program args (buffer-string)))
        (string-trim (buffer-string))))))

(defvar git-appraise-test--before-request-hook nil
  "Optional hook run before creating a review in `git-appraise-test--with-review-repo'.
Called with DIRECTORY when non-nil.")

(defmacro git-appraise-test--with-review-repo (prefix &rest body)
  "Run BODY in a disposable git-appraise repository with one review.
Binds `directory', `default-directory' and `revision'.  PREFIX is the
temp-directory name prefix."
  (declare (indent 1))
  (let ((dir (make-symbol "directory")))
    `(let* ((,dir (make-temp-file ,prefix t))
            (default-directory (file-name-as-directory ,dir))
            (directory ,dir)
            revision)
       (git-appraise-test--reset-db)
       (unwind-protect
           (progn
             (git-appraise-test--run ,dir "git" "init" "-q" "-b" "master")
             (git-appraise-test--run ,dir "git" "config" "user.name" "Tester")
             (git-appraise-test--run ,dir "git" "config" "user.email" "tester@example.com")
             (with-temp-file (expand-file-name "example.txt" ,dir)
               (insert "first\nsecond\n"))
             (git-appraise-test--run ,dir "git" "add" ".")
             (git-appraise-test--run ,dir "git" "commit" "-qm" "base")
             (git-appraise-test--run ,dir "git" "checkout" "-qb" "feature")
             (with-temp-file (expand-file-name "example.txt" ,dir)
               (insert "first\nchanged\nthird\n"))
             (git-appraise-test--run ,dir "git" "commit" "-qam" "change")
             (when git-appraise-test--before-request-hook
               (funcall git-appraise-test--before-request-hook ,dir))
             (setq revision (git-appraise-test--run ,dir "git" "rev-parse" "HEAD"))
             (git-appraise-test--run
              ,dir "git-appraise" "request" "-quiet" "-m" "Example review"
              "-target" "refs/heads/master")
             ,@body)
         (let ((review-buf (ignore-errors (get-buffer (git-appraise-pr-buffer-name)))))
           (when (and review-buf (buffer-live-p review-buf))
             (kill-buffer review-buf)))
         (let ((desc-buf (get-buffer git-appraise-pr-description-buffer-name)))
           (when (and desc-buf (buffer-live-p desc-buf))
             (kill-buffer desc-buf)))
         (git-appraise-test--reset-db)
         (delete-directory ,dir t)))))

;;; Failure artifacts

(defcustom git-appraise-test-artifact-dir "/tmp/git-appraise-test"
  "Directory for test failure artifacts."
  :type 'string
  :group 'git-appraise)

(defun git-appraise-test--artifact-file (name)
  "Return artifact path for NAME, creating directory as needed."
  (let* ((dir (file-name-as-directory git-appraise-test-artifact-dir))
         (test (ert-running-test))
         (slug (if test
                   (replace-regexp-in-string "[^[:alnum:]._/-]" "_"
                                             (format "%s" (ert-test-name test)))
                 "unknown-test"))
         (path (expand-file-name name (expand-file-name slug dir))))
    (make-directory (file-name-directory path) t)
    path))

(defun git-appraise-test--write-artifact (name text)
  "Write TEXT to artifact NAME and return its path."
  (let ((path (git-appraise-test--artifact-file name)))
    (with-temp-file path
      (insert (if (stringp text) text (format "%S" text))))
    path))

(defun git-appraise-test--should-equal-text (actual expected &optional label)
  "Assert ACTUAL equals EXPECTED as plain text.
On failure, write artifacts and include a unified diff in the message."
  (let ((a (substring-no-properties (or actual "")))
        (e (substring-no-properties (or expected ""))))
    (unless (equal a e)
      (let* ((label (or label "text mismatch"))
             (af (git-appraise-test--write-artifact "actual.txt" a))
             (ef (git-appraise-test--write-artifact "expected.txt" e))
             (df (git-appraise-test--write-artifact "diff.patch" ""))
             (diff (with-temp-buffer
                     (call-process "diff" nil t nil "-u" ef af)
                     (buffer-string))))
        (with-temp-file df (insert diff))
        (ert-fail
         (format "%s\nactual:   %s\nexpected: %s\ndiff:     %s\n\n%s"
                 label af ef df
                 (if (> (length diff) 6000)
                     (concat (substring diff 0 6000) "\n...diff truncated...")
                   diff)))))))

(defun git-appraise-test--assert-fixture (actual fixture &optional normalize)
  "Assert ACTUAL matches FIXTURE file contents.
NORMALIZE, when non-nil, is called on ACTUAL before comparison.
When `GIT_APPRAISE_TEST_ACCEPT' is set, overwrite FIXTURE with ACTUAL."
  (let* ((actual (substring-no-properties (or actual "")))
         (actual (if normalize (funcall normalize actual) actual))
         (expected (with-temp-buffer
                     (insert-file-contents fixture)
                     (buffer-string))))
    (if (getenv "GIT_APPRAISE_TEST_ACCEPT")
        (unless (equal actual expected)
          (with-temp-file fixture (insert actual))
          (message "Accepted fixture: %s" fixture))
      (git-appraise-test--should-equal-text
       actual expected (file-name-nondirectory fixture)))))

(provide 'git-appraise-test-helpers)
;;; git-appraise-test-helpers.el ends here
