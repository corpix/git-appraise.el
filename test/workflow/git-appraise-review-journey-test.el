;;; git-appraise-review-journey-test.el --- End-to-end review journeys -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'a)
(require 'git-appraise)
(require 'git-appraise-test-helpers)
(require 'git-appraise-test-viewed-fixture)

(defun git-appraise-review-journey-test--show (directory revision)
  "Return the native review for REVISION in DIRECTORY."
  (git-appraise-backend--json
   (git-appraise-test--run directory "git-appraise" "show" "-json" revision)
   "test show"))

(defun git-appraise-review-journey-test--text ()
  "Return plain text from the current review buffer."
  (with-current-buffer (git-appraise-pr-buffer-name)
    (substring-no-properties (buffer-string))))

(defun git-appraise-review-journey-test--composer-for (review-buffer)
  "Return the comment composer associated with REVIEW-BUFFER."
  (cl-find-if
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (and (derived-mode-p 'git-appraise-comment-mode)
                 (eq git-appraise-comment-review-buffer review-buffer)))))
   (buffer-list)))

;;; req-2026-11-27-012: create and reopen a review through selection
;; covers: git-appraise.el git-appraise-backend-read.el
;; covers-fn: git-appraise-start, git-appraise-backend-select-review
;; enforcement: test
(ert-deftest review-journey/req-2026-11-27-012-create-and-reopen ()
  "WHEN a reviewer starts in a repository without reviews THE SYSTEM SHALL create and reopen the selected review."
  (git-appraise-test--with-feature-repo "git-appraise-journey-start-"
    (require 'magit)
    (should (eq (lookup-key magit-status-mode-map (kbd "R"))
                'magit-file-rename))
    (should (transient-get-suffix 'magit-dispatch "R"))
    (let ((answers '("feature" "master" "Journey review" "")))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'read-string)
                 (lambda (&rest _) (pop answers))))
        (with-temp-buffer
          (setq default-directory (file-name-as-directory directory))
          (call-interactively #'git-appraise-start)))
      (should-not answers))
    (let ((buffer (get-buffer (git-appraise-pr-buffer-name))))
      (should buffer)
      (git-appraise-test--drain-lazy buffer)
      (should (string-match-p "Journey review"
                              (git-appraise-review-journey-test--text)))
      (kill-buffer buffer))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _) (caar choices))))
      (with-temp-buffer
        (setq default-directory (file-name-as-directory directory))
        (call-interactively #'git-appraise-start)))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (should (string-match-p "Journey review"
                            (git-appraise-review-journey-test--text)))
    (should (equal revision
                   (a-get (git-appraise-review-journey-test--show
                           directory revision) 'revision)))))

;;; req-2026-11-27-013: draft stays private until publication
;; covers: git-appraise-comment.el git-appraise-comment-draft.el git-appraise-actions.el
;; covers-fn: git-appraise-comment-add-or-edit, git-appraise-comment-commit, git-appraise-submit-needs-work
;; enforcement: test
(ert-deftest review-journey/req-2026-11-27-013-draft-edit-publish ()
  "WHEN a reviewer drafts, revises, and publishes an inline finding THE SYSTEM SHALL keep it private until publication."
  (git-appraise-test--with-review-repo "git-appraise-journey-comment-"
    (git-appraise-test--open-and-render revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (search-forward "+changed")
      (magit-section-show
       (git-appraise-eldoc--file-section (magit-current-section)))
      (beginning-of-line)
      (execute-kbd-macro (kbd "RET")))
    (with-current-buffer git-appraise-comment-buffer-name
      (erase-buffer)
      (insert "First finding")
      (execute-kbd-macro (kbd "C-c C-c")))
    (should-not (a-get (git-appraise-review-journey-test--show
                        directory revision) 'comments))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (search-forward "First finding")
      (magit-section-show
       (git-appraise-eldoc--file-section (magit-current-section)))
      (backward-char)
      (should-not (get-char-property (point) 'cursor-intangible))
      (execute-kbd-macro (kbd "RET")))
    (with-current-buffer git-appraise-comment-buffer-name
      (erase-buffer)
      (insert "Discarded edit")
      (execute-kbd-macro (kbd "C-c C-k")))
    (should (string-match-p "First finding"
                            (git-appraise-review-journey-test--text)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (search-forward "First finding")
      (magit-section-show
       (git-appraise-eldoc--file-section (magit-current-section)))
      (backward-char)
      (execute-kbd-macro (kbd "RET")))
    (with-current-buffer git-appraise-comment-buffer-name
      (erase-buffer)
      (insert "Revised finding")
      (execute-kbd-macro (kbd "C-c C-c")))
    (should (string-match-p "Revised finding"
                            (git-appraise-review-journey-test--text)))
    (should-not (string-match-p "First finding"
                                (git-appraise-review-journey-test--text)))
    (should-not (a-get (git-appraise-review-journey-test--show
                        directory revision) 'comments))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (execute-kbd-macro (kbd "g")))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (kill-buffer (git-appraise-pr-buffer-name))
    (git-appraise-test--open-and-render revision)
    (should (string-match-p "Revised finding"
                            (git-appraise-review-journey-test--text)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (call-interactively #'git-appraise-submit-needs-work))
    (let ((comments (a-get (git-appraise-review-journey-test--show
                            directory revision) 'comments)))
      (should (= (length comments) 1))
      (should (equal (a-get-in (car comments) '(comment description))
                     "Revised finding")))))

;;; req-2026-11-27-014: a review round reaches an accepted landing
;; covers: git-appraise-actions.el git-appraise-comment.el git-appraise-backend.el
;; covers-fn: git-appraise-submit-needs-work, git-appraise-resolve-comment-at-point, git-appraise-submit-approve, git-appraise-land-current-review
;; enforcement: test
(ert-deftest review-journey/req-2026-11-27-014-review-round ()
  "WHEN a finding is fixed and resolved THE SYSTEM SHALL allow the review to be approved and landed."
  (git-appraise-test--with-review-repo "git-appraise-journey-round-"
    (let ((review-buffer (git-appraise-test--open-and-render revision)))
    (with-current-buffer review-buffer
      (call-interactively #'git-appraise-submit-needs-work))
    (with-current-buffer git-appraise-comment-buffer-name
      (erase-buffer)
      (insert "Please fix this change")
      (execute-kbd-macro (kbd "C-c C-c")))
    (let ((thread (car (a-get (git-appraise-review-journey-test--show
                              directory revision) 'comments))))
      (should (equal (a-get-in thread '(comment description))
                     "Please fix this change"))
      (should (eq :false (a-get thread 'resolved))))
    (with-temp-file (expand-file-name "example.txt" directory)
      (insert "first\nchanged\nthird\nfixed\n"))
    (git-appraise-test--run directory "git" "commit" "-qam" "fix finding")
    (with-current-buffer review-buffer
      (should (eq (key-binding (kbd "g")) 'revert-buffer))
      (call-interactively (key-binding (kbd "g"))))
    (git-appraise-test--drain-lazy review-buffer)
    (with-current-buffer review-buffer
      (should (string-match-p "+fixed" (buffer-string)))
      (goto-char (point-min))
      (re-search-forward "Please[[:space:]\n]+fix[[:space:]\n]+this[[:space:]\n]+change")
      (should (eq (key-binding (kbd "C-c C-r"))
                  'git-appraise-resolve-comment-at-point))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "Fixed in the follow-up commit")))
        (call-interactively (key-binding (kbd "C-c C-r")))))
    (let ((thread (car (a-get (git-appraise-review-journey-test--show
                              directory revision) 'comments))))
      (should-not (eq :false (a-get thread 'resolved)))
      (should (equal (a-get-in (car (a-get thread 'children))
                               '(comment description))
                     "Fixed in the follow-up commit")))
    (with-current-buffer review-buffer
      (call-interactively #'git-appraise-submit-approve))
    (should (eq t (a-get (git-appraise-review-journey-test--show
                          directory revision) 'resolved)))
    (with-current-buffer review-buffer
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "fast-forward"))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) t)))
        (call-interactively #'git-appraise-land-current-review)))
    (should (equal (git-appraise-test--run directory "git" "rev-parse" "master")
                   (git-appraise-test--run directory "git" "rev-parse" "feature")))
    (let* ((reviews (git-appraise-backend-list directory))
           (review (cl-find-if (lambda (it) (equal (a-get it 'revision) revision))
                               reviews)))
      (should (eq t (a-get review 'submitted)))))))

;;; req-2026-11-27-015: navigation and progress survive lazy refresh
;; covers: git-appraise-viewed.el git-appraise-folding.el git-appraise-lazy.el
;; covers-fn: git-appraise-viewed-mark-file-and-fold, git-appraise-next-unviewed-file, git-appraise-reload
;; enforcement: test
(ert-deftest review-journey/req-2026-11-27-015-progress-across-files ()
  "WHEN a reviewer marks files viewed while navigating THE SYSTEM SHALL retain progress through lazy refresh and reopen."
  (git-appraise-viewed-e2e-test--with-review
    (let ((git-appraise-lazy-enabled t))
      (git-appraise-viewed-e2e-test--build directory revision)
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (search-forward "big.txt")
        (beginning-of-line)
        (call-interactively (key-binding (kbd "TAB")))
        (should (search-forward "L2" nil t))
        (goto-char (point-min))
        (search-forward "big.txt")
        (beginning-of-line)
        (should (eq (key-binding (kbd "V"))
                    'git-appraise-viewed-mark-file-and-fold))
        (call-interactively (key-binding (kbd "V")))
        (goto-char (point-min))
        (call-interactively (key-binding (kbd "N")))
        (let ((path (git-appraise-viewed--file-path (magit-current-section))))
          (should path)
          (should-not (equal path "big.txt"))
          (call-interactively (key-binding (kbd "v")))))
      (should (string-match-p "3/4 viewed"
                              (git-appraise-review-journey-test--text)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (call-interactively (key-binding (kbd "g"))))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (should (string-match-p "3/4 viewed"
                              (git-appraise-review-journey-test--text)))
      (kill-buffer (git-appraise-pr-buffer-name))
      (git-appraise-viewed-e2e-test--build directory revision)
      (should (string-match-p "3/4 viewed"
                              (git-appraise-review-journey-test--text))))))

;;; req-2026-11-27-017: concurrent composers keep their review context
;; covers: git-appraise-comment.el git-appraise-pr-state.el
;; covers-fn: git-appraise-comment-add-or-edit, git-appraise-comment-commit
;; enforcement: test
(ert-deftest review-journey/req-2026-11-27-017-two-live-reviews ()
  "WHEN two reviews have open comment composers THE SYSTEM SHALL save each draft to its originating review."
  (git-appraise-test--with-review-repo "git-appraise-journey-two-"
    (let ((first (git-appraise-test--open-and-render revision))
          second first-composer second-composer second-revision)
      (unwind-protect
          (progn
            (with-current-buffer first
              (goto-char (point-min))
              (search-forward "+changed")
              (beginning-of-line)
              (call-interactively (key-binding (kbd "RET"))))
            (setq first-composer
                  (git-appraise-review-journey-test--composer-for first))
            (should first-composer)
            (git-appraise-test--run directory "git" "checkout" "-qb" "other" "master")
            (with-temp-file (expand-file-name "example.txt" directory)
              (insert "first\nsecond\nother change\n"))
            (git-appraise-test--run directory "git" "commit" "-qam" "other change")
            (setq second-revision
                  (git-appraise-test--run directory "git" "rev-parse" "HEAD"))
            (git-appraise-test--run
             directory "git-appraise" "request" "-quiet" "-m" "Other review"
             "-target" "refs/heads/master")
            (setq second (git-appraise-test--open-and-render second-revision))
            (should-not (eq first second))
            (with-current-buffer second
              (goto-char (point-min))
              (search-forward "+other change")
              (beginning-of-line)
              (call-interactively (key-binding (kbd "RET"))))
            (setq second-composer
                  (git-appraise-review-journey-test--composer-for second))
            (should second-composer)
            (should-not (eq first-composer second-composer))
            (with-current-buffer first-composer
              (erase-buffer)
              (insert "First review draft")
              (call-interactively (key-binding (kbd "C-c C-c"))))
            (with-current-buffer second-composer
              (erase-buffer)
              (insert "Second review draft")
              (call-interactively (key-binding (kbd "C-c C-c"))))
            (with-current-buffer first
              (should (string-match-p "First[[:space:]\n]+review[[:space:]\n]+draft"
                                      (substring-no-properties (buffer-string))))
              (should-not (string-match-p "Second review draft" (buffer-string))))
            (with-current-buffer second
              (should (string-match-p "Second[[:space:]\n]+review[[:space:]\n]+draft"
                                      (substring-no-properties (buffer-string))))
              (should-not (string-match-p "First review draft" (buffer-string)))))
        (dolist (buffer (list first-composer second-composer first second))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

;;; git-appraise-review-journey-test.el ends here
