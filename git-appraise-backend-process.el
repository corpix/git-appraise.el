;;; git-appraise-backend-process.el --- Local process transport -*- lexical-binding: t; -*-

;;; Commentary:
;; Process and JSON transport shared by the local git-appraise backend.

;;; Code:

(require 'json)
(require 'subr-x)

(defun git-appraise-backend--run (directory &rest args)
  "Run git-appraise with ARGS in DIRECTORY and return its output."
  (unless (executable-find "git-appraise")
    (user-error "git-appraise is not installed or is absent from PATH"))
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((status (apply #'process-file "git-appraise" nil t nil args)))
        (unless (and (integerp status) (zerop status))
          (user-error "git appraise %s failed: %s"
                      (car args) (string-trim (buffer-string))))
        (buffer-string)))))

(defun git-appraise-backend--run-message (directory body &rest args)
  "Run git-appraise with BODY on stdin and ARGS in DIRECTORY."
  (unless (executable-find "git-appraise")
    (user-error "git-appraise is not installed or is absent from PATH"))
  (let ((default-directory (file-name-as-directory directory))
        (output (generate-new-buffer " *git-appraise-backend-output*")))
    (unwind-protect
        (with-temp-buffer
          (insert body)
          (let ((status (apply #'call-process-region
                               (point-min) (point-max) "git-appraise"
                               nil output nil args)))
            (unless (and (integerp status) (zerop status))
              (user-error "git appraise %s failed: %s"
                          (car args)
                          (with-current-buffer output
                            (string-trim (buffer-string)))))))
      (kill-buffer output))))

(defun git-appraise-backend--run-parallel (directory commands)
  "Run COMMANDS in parallel in DIRECTORY.
COMMANDS is a list of (TAG PROGRAM . ARGS).  Return an alist of
(TAG . OUTPUT).  Signal `user-error' with the first failure,
matching the single-command helpers."
  (dolist (cmd commands)
    (let ((program (nth 1 cmd)))
      (when (and (stringp program)
                 (string= (file-name-nondirectory program) "git-appraise")
                 (not (executable-find "git-appraise")))
        (user-error "git-appraise is not installed or is absent from PATH"))))
  (let ((default-directory (file-name-as-directory directory))
        (procs nil))
    (unwind-protect
        (progn
          (dolist (cmd commands)
            (let* ((tag (nth 0 cmd))
                   (program (nth 1 cmd))
                   (args (cddr cmd))
                   (buf (generate-new-buffer (format " *ga-par-%s*" tag)))
                   (err-buf (generate-new-buffer (format " *ga-par-%s-err*" tag)))
                   (proc (make-process
                          :name (format "ga-par-%s" tag)
                          :buffer buf
                          :stderr err-buf
                          :command (cons program args)
                          :noquery t
                          :sentinel #'ignore)))
              (push (list tag program args proc buf err-buf) procs)))
          (let ((pending procs))
            (while pending
              (accept-process-output nil 0.1)
              (let (live)
                (dolist (p pending)
                  (when (process-live-p (nth 3 p))
                    (push p live)))
                (setq pending (nreverse live)))))
          (let (results)
            (dolist (p (nreverse procs))
              (pcase-let ((`(,tag ,program ,args ,proc ,buf ,err-buf) p))
                (let ((status (process-exit-status proc)))
                  (unless (and (integerp status) (zerop status))
                    (let ((err (with-current-buffer err-buf
                                 (string-trim (buffer-string))))
                          (out (with-current-buffer buf
                                 (string-trim (buffer-string)))))
                      (user-error "%s %s failed: %s"
                                  (file-name-nondirectory program)
                                  (car args)
                                  (if (string-empty-p err) out err)))))
                (push (cons tag (with-current-buffer buf (buffer-string)))
                      results)))
            (nreverse results)))
      (dolist (p procs)
        (let ((proc (nth 3 p))
              (buf (nth 4 p))
              (err-buf (nth 5 p)))
          (when (process-live-p proc)
            (delete-process proc))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (buffer-live-p err-buf) (kill-buffer err-buf)))))))

(defun git-appraise-backend--git (directory &rest args)
  "Run Git with ARGS in DIRECTORY and return its trimmed output."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil args)))
        (unless (and (integerp status) (zerop status))
          (user-error "Git %s failed: %s" (car args)
                      (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun git-appraise-backend--root ()
  "Return the Git root of `default-directory'."
  (git-appraise-backend--git default-directory "rev-parse" "--show-toplevel"))

(defun git-appraise-backend--json (string context)
  "Parse STRING as JSON, signaling a useful error for CONTEXT."
  (condition-case err
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol)
            (json-false :false)
            (json-null nil))
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (let ((value (json-read)))
            (skip-chars-forward " \t\r\n")
            (unless (eobp)
              (error "Unexpected text after JSON value"))
            value)))
    (error (user-error "Invalid git-appraise %s JSON: %s"
                       context (error-message-string err)))))

(provide 'git-appraise-backend-process)
;;; git-appraise-backend-process.el ends here
