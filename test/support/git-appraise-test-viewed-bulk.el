;;; git-appraise-test-viewed-bulk.el --- Large viewed review fixture -*- lexical-binding: t; -*-

(require 'git-appraise)
(require 'git-appraise-test-helpers)
(require 'git-appraise-viewed)

(defvar git-appraise-test-viewed-bulk--lines-per-file 1
  "Changed lines generated per file in the bulk viewed fixture.")

(defmacro git-appraise-test-viewed-bulk--with-review (count &rest body)
  "Run BODY in a disposable review with COUNT changed files.
Bind DIRECTORY and REVISION; keep personal state in memory."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "git-appraise-viewed-bulk-" t))
          (default-directory (file-name-as-directory directory))
          (git-appraise-store-file nil)
          (git-appraise-lazy-enabled nil)
          (git-appraise-history-enabled nil)
          (git-appraise-analysis-deferred? nil)
          revision)
     (git-appraise-test--reset-db)
     (unwind-protect
         (progn
           (git-appraise-test--run directory "git" "init" "-q" "-b" "master")
           (git-appraise-test--run directory "git" "config" "user.name" "Tester")
           (git-appraise-test--run directory "git" "config" "user.email" "tester@example.com")
           (dotimes (n ,count)
             (with-temp-file (expand-file-name (format "file-%04d.txt" n) directory)
               (dotimes (line git-appraise-test-viewed-bulk--lines-per-file)
                 (insert (format "before-%04d\n" line)))))
           (git-appraise-test--run directory "git" "add" ".")
           (git-appraise-test--run directory "git" "commit" "-qm" "base")
           (git-appraise-test--run directory "git" "checkout" "-qb" "feature")
           (dotimes (n ,count)
             (with-temp-file (expand-file-name (format "file-%04d.txt" n) directory)
               (dotimes (line git-appraise-test-viewed-bulk--lines-per-file)
                 (insert (format "after-%04d\n" line)))))
           (git-appraise-test--run directory "git" "add" ".")
           (git-appraise-test--run directory "git" "commit" "-qm" "review")
           (setq revision (git-appraise-test--run directory "git" "rev-parse" "HEAD"))
           (git-appraise-test--run directory "git-appraise" "request" "-quiet"
                                    "-m" "Bulk viewed review" "-target" "refs/heads/master")
           (git-appraise-backend-open revision)
           (let ((git-appraise-section-full-refresh? t))
             (git-appraise--build-buffer))
           (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
           ,@body)
       (when-let* ((buffer (get-buffer (git-appraise-pr-buffer-name))))
         (kill-buffer buffer))
       (git-appraise-test--reset-db)
       (delete-directory directory t))))

(defun git-appraise-test-viewed-bulk--select-all ()
  "Select every file heading in the current review buffer."
  (let (first last)
    (magit-map-sections
     (lambda (section)
       (when (magit-file-section-p section)
         (unless first (setq first section))
         (setq last section)))
     magit-root-section)
    (unless (and first last)
      (error "No file sections to select"))
    (let ((first-path (oref first value))
          (last-path (oref last value)))
      (goto-char (point-min))
      (re-search-forward (concat "^modified[ \t]+.*"
                                 (regexp-quote first-path) "\\(?:[ \t]\\|$\\)"))
      (let ((beg (line-beginning-position)))
        (goto-char (point-min))
        (re-search-forward (concat "^modified[ \t]+.*"
                                   (regexp-quote last-path) "\\(?:[ \t]\\|$\\)"))
        (let ((end (line-end-position)))
          (transient-mark-mode 1)
          (goto-char end)
          (set-mark beg)
          (goto-char end)
          (activate-mark))))))

(defun git-appraise-test-viewed-bulk--press-v ()
  "Select all file headings and press the review buffer's v binding."
  (git-appraise-test-viewed-bulk--select-all)
  (call-interactively (key-binding (kbd "v"))))

(provide 'git-appraise-test-viewed-bulk)
;;; git-appraise-test-viewed-bulk.el ends here
