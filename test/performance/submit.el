;;; submit.el --- Profile large review submission in batch Emacs -*- lexical-binding: t; -*-

(setq native-comp-jit-compilation nil)
(add-to-list 'load-path default-directory)
(add-to-list 'load-path (expand-file-name "test/support" default-directory))
(require 'git-appraise)
(require 'git-appraise-test-submit-fixture)
(require 'profiler)

(setq git-appraise-store-file nil)
(setq-default git-appraise-analysis-deferred? nil)

(let* ((profile? (equal (getenv "GIT_APPRAISE_PROFILE") "1"))
       (git-appraise-test-viewed-bulk--lines-per-file
        (string-to-number (or (getenv "GIT_APPRAISE_SUBMIT_LINES") "100")))
       (git-appraise-history-enabled nil)
       (output-dir (expand-file-name "tmp/performance" default-directory))
       (phases (make-hash-table :test #'eq))
       probes)
  (when profile? (make-directory output-dir t))
  (dolist (function '(git-appraise--build-buffer
                      git-appraise-backend-refresh-comments
                      git-appraise-backend--run-parallel
                      git-appraise-comment--refresh-conversation-in-place
                      git-appraise-section--update-feedback-in-place))
    (let* ((name function)
           (probe (lambda (original &rest args)
                    (let ((start (current-time)))
                      (unwind-protect
                          (apply original args)
                        (let* ((prior (gethash name phases '(0 . 0.0)))
                               (elapsed (float-time
                                         (time-subtract (current-time) start))))
                          (puthash name (cons (1+ (car prior))
                                              (+ (cdr prior) elapsed)) phases)))))))
      (advice-add name :around probe)
      (push (cons name probe) probes)))
  (unwind-protect
      (git-appraise-test-viewed-bulk--with-review 200
        (git-appraise-test-submit--make-historical-finding directory revision)
        (with-current-buffer (git-appraise-pr-buffer-name)
          (git-appraise-reload))
        (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
        (clrhash phases)
        (with-current-buffer (git-appraise-pr-buffer-name)
          (when profile? (profiler-start 'cpu+mem))
          (let ((start (current-time)))
            (unwind-protect
                (git-appraise--submit "NEEDS_WORK" "Needs another pass")
              (when profile? (profiler-stop)))
            (princ (format "submit: %.3f seconds\n"
                           (float-time (time-subtract (current-time) start)))))
          (maphash (lambda (name value)
                     (princ (format "  %s: %d calls, %.3f seconds\n"
                                    name (car value) (cdr value))))
                   phases)
          (when profile?
            (dolist (spec `((,(profiler-cpu-profile) "submit-cpu")
                            (,(profiler-memory-profile) "submit-memory")))
              (let ((data (car spec))
                    (base (cadr spec)))
                (profiler-write-profile
                 data (expand-file-name (concat base ".prof") output-dir))
                (with-current-buffer (profiler-report-profile data)
                  (goto-char (point-min))
                  (profiler-report-expand-entry t)
                  (let ((make-backup-files nil))
                    (write-region (point-min) (point-max)
                                  (expand-file-name (concat base ".txt")
                                                    output-dir))))))
            (princ (format "Profiles: %s\n" output-dir)))))
    (dolist (entry probes)
      (advice-remove (car entry) (cdr entry)))))
