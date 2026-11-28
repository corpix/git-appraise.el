;;; git-appraise-submit-large-e2e-test.el --- Large review submission -*- lexical-binding: t; -*-

(require 'a)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-submit-fixture)
(require 'git-appraise-test-viewed-bulk)

;;; req-2026-11-28-001: submission retains context and comments
;; covers: git-appraise-actions.el git-appraise-backend-read.el git-appraise-comment-draft.el
;; covers-fn: git-appraise-submit-needs-work, git-appraise-backend-refresh-comments
;; enforcement: test
(ert-deftest submit-large-e2e/req-2026-11-28-001-needs-work-keeps-context ()
  "WHEN a reviewer requests changes on a large review THE SYSTEM SHALL keep the current file and historical findings visible without reloading the diff."
  (let ((git-appraise-test-viewed-bulk--lines-per-file 10))
    (git-appraise-test-viewed-bulk--with-review 200
      (let* ((hash (git-appraise-test-submit--make-historical-finding
                    directory revision))
             (buffer (get-buffer (git-appraise-pr-buffer-name)))
             (original-build (symbol-function 'git-appraise--build-buffer))
             (original-parallel (symbol-function 'git-appraise-backend--run-parallel))
             (builds 0) (diff-reads 0)
             point-anchor)
        (with-current-buffer buffer
          (git-appraise-reload))
        (git-appraise-test--drain-lazy buffer)
        (with-current-buffer buffer
          (should-not (assoc "file-0000.txt"
                             (git-appraise--diff--split-by-files
                              (git-appraise-db--pullreq-raw-diff))))
          (should (string-match-p "Historical finding on reverted file"
                                  (buffer-string)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "Historical finding on reverted file")
            (let ((section (magit-section-at (point))))
              (while section
                (magit-section-show section)
                (setq section (oref section parent))))
            (should-not (invisible-p (point))))
          (goto-char (point-min))
          (re-search-forward "^modified[ \t]+.*file-0100\\.txt")
          (beginning-of-line)
          (setq point-anchor (copy-marker (point)))
          (set-window-buffer (selected-window) buffer))
        (unwind-protect
            (progn
              (fset 'git-appraise--build-buffer
                    (lambda (&rest args)
                      (cl-incf builds)
                      (apply original-build args)))
              (fset 'git-appraise-backend--run-parallel
                    (lambda (root commands)
                      (when (--any? (member "-diff" it) commands)
                        (cl-incf diff-reads))
                      (funcall original-parallel root commands)))
              (with-current-buffer buffer
                (call-interactively #'git-appraise-submit-needs-work))
              (with-current-buffer git-appraise-comment-buffer-name
                (erase-buffer)
                (insert "Needs another pass")
                (execute-kbd-macro (kbd "C-c C-c")))
              (git-appraise-test--drain-lazy buffer)
              (should (= builds 0))
              (should (= diff-reads 0))
              (with-current-buffer buffer
                (should (= (point) (marker-position point-anchor)))
                (should (equal "file-0100.txt"
                               (get-text-property (point) 'git-appraise-path)))
                (should (string-match-p "Historical finding on reverted file"
                                        (buffer-string)))
                (goto-char (point-min))
                (search-forward "Historical finding on reverted file")
                (should-not (invisible-p (point))))
              (let* ((review (git-appraise-backend--read-review
                              directory revision))
                     (finding (car (a-get review 'comments))))
                (should (eq :false (a-get review 'resolved)))
                (should (--any? (equal (a-get it 'hash) hash)
                                (a-get review 'comments)))
                (should finding)))
          (fset 'git-appraise--build-buffer original-build)
          (fset 'git-appraise-backend--run-parallel original-parallel)
          (when point-anchor (set-marker point-anchor nil)))))))

;;; req-2026-11-28-002: resolution retains the finding and review position
;; covers: git-appraise-actions.el git-appraise-comment-draft.el
;; covers-fn: git-appraise-resolve-comment-at-point, git-appraise-submit-approve
;; enforcement: test
(ert-deftest submit-large-e2e/req-2026-11-28-002-resolve-and-approve ()
  "WHEN a finding is resolved on a large review THE SYSTEM SHALL keep the reviewer at that finding through resolution and approval without reloading the diff."
  (let ((git-appraise-test-viewed-bulk--lines-per-file 10))
    (git-appraise-test-viewed-bulk--with-review 200
      (let* ((hash (git-appraise-test-submit--make-historical-finding
                    directory revision))
             (buffer (get-buffer (git-appraise-pr-buffer-name)))
             (original-build (symbol-function 'git-appraise--build-buffer))
             (builds 0)
             finding-point feedback-anchor-id)
        (with-current-buffer buffer
          (git-appraise-reload)
          (call-interactively #'git-appraise-submit-needs-work))
        (with-current-buffer git-appraise-comment-buffer-name
          (erase-buffer)
          (insert "Please fix the finding")
          (execute-kbd-macro (kbd "C-c C-c")))
        (git-appraise-test--drain-lazy buffer)
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "Historical finding on reverted file")
          (let ((section (magit-section-at (match-beginning 0))))
            (should section)
            (goto-char (oref section start))
            (while section
              (magit-section-show section)
              (setq section (oref section parent))))
          (setq finding-point (point)))
        (unwind-protect
            (progn
              (fset 'git-appraise--build-buffer
                    (lambda (&rest args)
                      (cl-incf builds)
                      (apply original-build args)))
              (with-current-buffer buffer
                (cl-letf (((symbol-function 'read-string)
                           (lambda (&rest _) "Fixed in follow-up")))
                  (call-interactively #'git-appraise-resolve-comment-at-point))
                (should (<= (abs (- (point) finding-point)) 200))
                (should (string-match-p "Historical finding on reverted file"
                                        (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (min (point-max) (+ (point) 250)))))
                (let* ((review (git-appraise-backend--read-review
                                directory revision))
                       (feedback
                        (--first (equal (a-get-in it '(comment description))
                                        "Please fix the finding")
                                 (a-get review 'comments)))
                       (feedback-hash (and feedback (a-get feedback 'hash)))
                       (section
                        (git-appraise-section--find-section
                         buffer
                         (lambda (candidate)
                           (and feedback-hash
                                (equal (git-appraise-actions--section-comment-id
                                        candidate) feedback-hash))))))
                  (should section)
                  (goto-char (oref section start))
                  (cl-letf (((symbol-function 'read-string)
                             (lambda (&rest _) "Feedback addressed")))
                    (call-interactively
                     #'git-appraise-resolve-comment-at-point)))
                (setq feedback-anchor-id
                      (git-appraise-actions--section-comment-id
                       (magit-current-section)))
                (should feedback-anchor-id)
                (call-interactively #'git-appraise-submit-approve)
                (should (equal feedback-anchor-id
                               (git-appraise-actions--section-comment-id
                                (magit-current-section)))))
              (should (= builds 0))
              (let* ((review (git-appraise-backend--read-review
                              directory revision))
                     (finding (--first (equal (a-get it 'hash) hash)
                                       (a-get review 'comments))))
                (should finding)
                (should (eq t (a-get-in (car (a-get finding 'children))
                                          '(comment resolved))))
                (should (eq t (a-get review 'resolved)))
                (should (equal "Fixed in follow-up"
                               (a-get-in (car (a-get finding 'children))
                                         '(comment description))))))
          (fset 'git-appraise--build-buffer original-build))))))

;;; git-appraise-submit-large-e2e-test.el ends here
