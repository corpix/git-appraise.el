;;; git-appraise-backend-process.el --- Local process transport -*- lexical-binding: t; -*-

;;; Commentary:
;; Process and JSON transport shared by the local git-appraise backend.

;;; Code:

(require 'dash)
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
  (--each commands
    (let ((program (nth 1 it)))
      (when (and (stringp program)
                 (string= (file-name-nondirectory program) "git-appraise")
                 (not (executable-find "git-appraise")))
        (user-error "git-appraise is not installed or is absent from PATH"))))
  (let ((default-directory (file-name-as-directory directory))
        (procs nil)
        (buffers nil))
    (unwind-protect
        (progn
          (--each commands
            (let* ((tag (nth 0 it))
                   (program (nth 1 it))
                   (args (cddr it))
                   (buf (generate-new-buffer (format " *ga-par-%s*" tag))))
              (push buf buffers)
              (let ((proc (make-process
                           :name (format "ga-par-%s" tag)
                           :buffer buf
                           :command (cons program args)
                           :connection-type 'pipe
                           :noquery t
                           :sentinel #'ignore)))
                (push (list tag program args proc buf) procs))))
          (let ((pending procs))
            (while pending
              (accept-process-output nil 0.1)
              (--each pending
                (let ((p it))
                  (unless (process-live-p (nth 3 p))
                    (let ((status (process-exit-status (nth 3 p))))
                      (unless (and (integerp status) (zerop status))
                        (let ((out (with-current-buffer (nth 4 p)
                                     (string-trim (buffer-string)))))
                          (user-error "%s %s failed: %s"
                                      (file-name-nondirectory (nth 1 p))
                                      (car (nth 2 p))
                                      out)))))))
              (setq pending (--filter (process-live-p (nth 3 it)) pending))))
          (--map (cons (car it)
                       (with-current-buffer (nth 4 it) (buffer-string)))
                 (nreverse procs)))
      (--each procs
        (let ((proc (nth 3 it)))
          (when (process-live-p proc)
            (delete-process proc))))
      (--each buffers
        (when (buffer-live-p it)
          (kill-buffer it))))))

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
