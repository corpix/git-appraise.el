;;; viewed.el --- Profile bulk viewed actions in batch Emacs -*- lexical-binding: t; -*-

(setq native-comp-jit-compilation nil)
(add-to-list 'load-path default-directory)
(add-to-list 'load-path (expand-file-name "test/support" default-directory))
(require 'git-appraise)
(require 'git-appraise-test-viewed-bulk)
(require 'git-appraise-folding)
(require 'profiler)

(setq git-appraise-store-file nil)
(setq-default git-appraise-analysis-deferred? nil)

(let* ((profile? (equal (getenv "GIT_APPRAISE_PROFILE") "1"))
       (git-appraise-test-viewed-bulk--lines-per-file 100)
       (output-dir (expand-file-name "tmp/performance" default-directory))
       (phases (make-hash-table :test #'eq))
       (probes nil))
  (when profile?
    (make-directory output-dir t))
  (dolist (function '(git-appraise-viewed--region-file-sections
                      git-appraise-viewed--completion-snapshot
                      git-appraise-store-append-events
                      git-appraise-viewed--reproject
                      git-appraise-viewed--apply-in-place
                      git-appraise-viewed-changes--prepare
                      git-appraise-viewed-changes--paint))
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
        (with-current-buffer (git-appraise-pr-buffer-name)
          (when profile?
            (profiler-start 'cpu+mem))
          (unwind-protect
              (dotimes (pass 2)
                (clrhash phases)
                (let ((start (current-time)))
                  (git-appraise-test-viewed-bulk--press-v)
                  (princ (format "%s: %.3f seconds\n"
                                 (if (zerop pass) "mark" "unmark")
                                 (float-time (time-subtract (current-time) start)))))
                (maphash (lambda (name value)
                           (princ (format "  %s: %d calls, %.3f seconds\n"
                                          name (car value) (cdr value))))
                         phases))
            (when profile?
              (profiler-stop)
              (dolist (spec `((,(profiler-cpu-profile) "viewed-cpu")
                              (,(profiler-memory-profile) "viewed-memory")))
                (let ((data (car spec))
                      (base (cadr spec)))
                  (profiler-write-profile
                   data (expand-file-name (concat base ".prof") output-dir))
                  (with-current-buffer (profiler-report-profile data)
                    (goto-char (point-min))
                    (profiler-report-expand-entry t)
                    (let ((make-backup-files nil))
                      (write-region (point-min) (point-max)
                                    (expand-file-name (concat base ".txt") output-dir))))))
              (princ (format "Profiles: %s\n" output-dir))))))
    (dolist (entry probes)
      (advice-remove (car entry) (cdr entry)))))
