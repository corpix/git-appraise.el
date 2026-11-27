;;; git-appraise-test-helpers.el --- Shared helpers for git-appraise ERT tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'uuidgen)
(require 'a)
(require 'magit-section)
(require 'ert)
(require 'git-appraise-db)

(setq-default git-appraise-analysis-deferred? nil)

(defconst git-appraise-test--fixture-directory
  (expand-file-name "../fixtures"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory holding checked-in test fixtures.")

(defun git-appraise-test--fixture-path (name)
  "Return the absolute path of fixture NAME."
  (expand-file-name name git-appraise-test--fixture-directory))

(defmacro git-appraise-test--with-store (&rest body)
  "Run BODY against a fresh in-memory store database."
  (declare (indent 0))
  `(let ((git-appraise-store-file nil))
     (unwind-protect
         (progn
           (git-appraise-store-close)
           ,@body)
       (git-appraise-store-close))))

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

(defun git-appraise-test--open-and-render (revision)
  "Open REVISION, finish its review render, and return the buffer."
  (let ((git-appraise-history-enabled nil))
    (git-appraise-backend-open revision)
    (let ((git-appraise-section-full-refresh? t))
      (git-appraise--build-buffer))
    (let ((buffer (get-buffer (git-appraise-pr-buffer-name))))
      (git-appraise-test--drain-lazy buffer)
      buffer)))

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

(defmacro git-appraise-test--with-feature-repo (prefix &rest body)
  "Run BODY in a disposable feature branch without a review.
Bind `directory', `default-directory', and `revision'.  PREFIX names
the temporary directory."
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
             (setq revision (git-appraise-test--run ,dir "git" "rev-parse" "HEAD"))
             ,@body)
         (let ((review-buf (ignore-errors (get-buffer (git-appraise-pr-buffer-name)))))
           (when (and review-buf (buffer-live-p review-buf))
             (kill-buffer review-buf)))
         (let ((desc-buf (get-buffer git-appraise-pr-description-buffer-name)))
           (when (and desc-buf (buffer-live-p desc-buf))
             (kill-buffer desc-buf)))
         (git-appraise-test--reset-db)
         (delete-directory ,dir t)))))

(defmacro git-appraise-test--with-review-repo (prefix &rest body)
  "Run BODY in a disposable feature repository with one review."
  (declare (indent 1))
  `(git-appraise-test--with-feature-repo ,prefix
     (when git-appraise-test--before-request-hook
       (funcall git-appraise-test--before-request-hook directory))
     (setq revision (git-appraise-test--run directory "git" "rev-parse" "HEAD"))
     (git-appraise-test--run
      directory "git-appraise" "request" "-quiet" "-m" "Example review"
      "-target" "refs/heads/master")
     ,@body))

(provide 'git-appraise-test-helpers)
;;; git-appraise-test-helpers.el ends here
