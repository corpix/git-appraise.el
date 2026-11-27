;;; git-appraise-backend-request-test.el --- ERT tests for creating git-appraise reviews -*- lexical-binding: t; -*-

;;; Commentary:
;; Creating a review is a local git-appraise operation, so these tests
;; use the real CLI in disposable repositories.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

(defmacro git-appraise-backend-request-test-with-repo (&rest body)
  "Run BODY in a disposable repository with a feature commit but no review."
  (declare (indent 0))
  `(git-appraise-test--with-feature-repo "git-appraise-backend-request-test-"
     ,@body))

(defun git-appraise-backend-request-test--description (directory)
  "Return the request description of the single review in DIRECTORY."
  (a-get-in (car (git-appraise-backend-list directory))
            '(request description)))

(ert-deftest git-appraise-backend-request/creates-a-review ()
  "Creating a review anchors it on the first reviewed commit."
  (git-appraise-backend-request-test-with-repo
    (should (equal (git-appraise-backend--create
                    directory "Created review" "feature" "master" nil)
                   revision))
    (let ((review (car (git-appraise-backend-list directory))))
      (should (equal (a-get review 'revision) revision))
      (should (equal (a-get-in review '(request description)) "Created review"))
      (should (equal (a-get-in review '(request reviewRef)) "refs/heads/feature"))
      (should (equal (a-get-in review '(request targetRef)) "refs/heads/master")))))

(ert-deftest git-appraise-backend-request/uses-the-commit-message-when-empty ()
  "An empty message falls back to the reviewed commit's message."
  (git-appraise-backend-request-test-with-repo
    (git-appraise-backend--create directory "" "feature" "master" nil)
    (should (equal (git-appraise-backend-request-test--description directory)
                   "change"))))

(ert-deftest git-appraise-backend-request/records-reviewers ()
  "Reviewers are forwarded to git-appraise as a comma-separated list."
  (git-appraise-backend-request-test-with-repo
    (git-appraise-backend--create directory "With reviewers" "feature" "master"
                                  '("alice" "bob"))
    (should (equal (a-get-in (car (git-appraise-backend-list directory))
                             '(request reviewers))
                   '("alice" "bob")))))

(ert-deftest git-appraise-backend-request/refuses-a-second-open-review ()
  "Creating a review twice for the same commit signals `user-error'."
  (git-appraise-backend-request-test-with-repo
    (git-appraise-backend--create directory "First" "feature" "master" nil)
    (should-error (git-appraise-backend--create directory "Second" "feature"
                                                "master" nil)
                  :type 'user-error)))

(ert-deftest git-appraise-backend-request/default-target-prefers-master ()
  "Without a remote, the target defaults to an existing local branch."
  (git-appraise-backend-request-test-with-repo
    (should (equal (git-appraise-backend--default-target directory) "master"))))

(ert-deftest git-appraise-backend-request/default-target-prefers-remote-head ()
  "The remote's default branch wins when it also exists locally."
  (git-appraise-backend-request-test-with-repo
    (git-appraise-test--run directory "git" "branch" "main" "master")
    (git-appraise-test--run
     directory "git" "symbolic-ref" "refs/remotes/origin/HEAD"
     "refs/remotes/origin/main")
    (should (equal (git-appraise-backend--default-target directory) "main"))))

(ert-deftest git-appraise-backend-request/empty-list-offers-creation ()
  "Selecting a review in an empty repository offers to create one."
  (git-appraise-backend-request-test-with-repo
    (let ((answers '("feature" "master" "Created from start" "")))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'read-string) (lambda (&rest _) (pop answers))))
        (should (equal (git-appraise-backend-select-review) revision))))
    (should (equal (git-appraise-backend-request-test--description directory)
                   "Created from start"))))

(ert-deftest git-appraise-backend-request/declining-creation-keeps-the-error ()
  "Declining the creation prompt still reports that no reviews exist."
  (git-appraise-backend-request-test-with-repo
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should-error (git-appraise-backend-select-review) :type 'user-error))))

;;; req-2026-11-23-005: local-review-creation
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend-request git-appraise-backend-select-review
;; enforcement: test
(ert-deftest git-appraise-backend-request/req-2026-11-23-005-creates-and-opens-a-review ()
  "WHEN a Git repository has no local review yet THE SYSTEM SHALL
create one from the available commits and open it for review.
The creation flow must run even when the global backend is `url'."
  (git-appraise-backend-request-test-with-repo
    (git-appraise-test--with-db
      (let ((git-appraise-start-backend 'url)
            (answers '("feature" "master" "From command" ""))
            buffer)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) (pop answers))))
                (should (equal (git-appraise-backend-request) revision)))
              (setq buffer (get-buffer (git-appraise-pr-buffer-name)))
              (should buffer)
              (git-appraise-test--drain-lazy buffer)
              (should (equal (git-appraise-backend-request-test--description
                              directory)
                             "From command")))
          (when (and buffer (buffer-live-p buffer))
            (kill-buffer buffer)))))))

;;; req-2026-11-24-126: review picker shows refs and times
;; covers: git-appraise-backend.el
;; covers-fn: git-appraise-backend-select-review, git-appraise-backend--review-choice, git-appraise-backend--short-ref, git-appraise-backend--format-unix-time, git-appraise-backend--review-last-comment-ts
;; enforcement: test
(ert-deftest git-appraise-backend-request/req-2026-11-24-126-picker-format ()
  "WHEN a reviewer selects a local review THE SYSTEM SHALL show refs, creation time, and last comment time."
  (should (equal (git-appraise-backend--short-ref "refs/heads/feature") "feature"))
  (should (equal (git-appraise-backend--short-ref "refs/tags/v1") "refs/tags/v1"))
  (should (equal (git-appraise-backend--short-ref nil) "?"))
  (should (equal (git-appraise-backend--format-unix-time "0") "-"))
  (should (equal (git-appraise-backend--format-unix-time nil) "-"))
  (let* ((review '((revision . "e54430688a4c31947bd179ed5c9570190b293a61")
                   (submitted . :false)
                   (request . ((timestamp . "1795542276")
                               (reviewRef . "refs/heads/feature")
                               (targetRef . "refs/heads/master")
                               (description . "My review")))
                   (comments . (((comment . ((timestamp . "1795542282")))
                                 (children . (((comment . ((timestamp . "1795542299")))))))))))
         (choice (git-appraise-backend--review-choice review))
         (display (plist-get choice :display)))
    (should (equal (plist-get choice :refs) "master..feature"))
    (should (equal (plist-get choice :state) "PENDING"))
    (should (equal (plist-get choice :rev) "e54430688a4c"))
    (should (equal (plist-get choice :last)
                   (git-appraise-backend--format-unix-time "1795542299")))
    (should (string-match-p "master\\.\\.feature" display))
    (should (string-match-p "created " display))
    (should (string-match-p "last " display))
    (should (string-match-p "My review" display))))

(ert-deftest git-appraise-backend-request/req-2026-11-24-126-picker-selects-revision ()
  "WHEN a reviewer picks a formatted review candidate THE SYSTEM SHALL return its revision."
  (git-appraise-backend-request-test-with-repo
    (git-appraise-backend--create directory "Created review" "feature" "master" nil)
    (let (captured)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (setq captured choices)
                   (caar choices))))
        (should (equal (git-appraise-backend-select-review) revision))
        (should (cl-some (lambda (c) (string-match-p "master\\.\\.feature" (car c)))
                         captured))
        (should (cl-some (lambda (c) (string-match-p "created " (car c)))
                         captured))
        (should (cl-some (lambda (c) (string-match-p "last -" (car c)))
                         captured))))))


;;; req-2026-11-24-107: review commits are visible
;; covers: git-appraise-backend.el git-appraise-section-header.el
;; covers-fn: git-appraise-backend-refresh, git-appraise-section-insert-commits
;; enforcement: test
(ert-deftest git-appraise-backend-request/req-2026-11-24-107-commits-visible ()
  "WHEN a reviewer opens a local review THE SYSTEM SHALL show its commits and retain them when conversation data refreshes."
  (git-appraise-backend-request-test-with-repo
    (git-appraise-test--with-db
      (let ((git-appraise-history-enabled nil)
            (buffer nil))
        (unwind-protect
            (progn
              (git-appraise-backend--create directory "Review" "feature" "master" nil)
              (git-appraise-backend-open revision)
              (let ((git-appraise-section-full-refresh? t))
                (git-appraise--build-buffer))
              (setq buffer (get-buffer (git-appraise-pr-buffer-name)))
              (git-appraise-test--drain-lazy buffer)
              (with-current-buffer buffer
                (should (string-match-p "Commits (1)[[:space:]\n]+.*change"
                                        (buffer-string))))
              (should (eq 'ok (git-appraise-backend-refresh-comments
                               (git-appraise-db-get-pullreq))))
              (should (= 1 (length (a-get-in
                                     (git-appraise-db--pullreq-raw-infos)
                                     '(commits nodes))))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

;;; req-2026-11-24-109: commit rows open their revision
;; covers: git-appraise-backend.el git-appraise-section-header.el
;; covers-fn: git-appraise-backend--commits, git-appraise-section-visit-commit
;; enforcement: test
(ert-deftest git-appraise-backend-request/req-2026-11-24-109-visit-commit ()
  "WHEN a reviewer opens a listed commit THE SYSTEM SHALL show that exact revision in Magit."
  (git-appraise-backend-request-test-with-repo
    (git-appraise-test--with-db
      (let ((git-appraise-history-enabled nil)
            (buffer nil))
        (unwind-protect
            (progn
              (git-appraise-backend--create directory "Review" "feature" "master" nil)
              (git-appraise-backend-open revision)
              (let ((git-appraise-section-full-refresh? t))
                (git-appraise--build-buffer))
              (setq buffer (get-buffer (git-appraise-pr-buffer-name)))
              (git-appraise-test--drain-lazy buffer)
              (with-current-buffer buffer
                (let (commit shown)
                  (magit-map-sections
                   (lambda (section)
                     (when (eq (oref section type) 'git-appraise-commit-section)
                       (setq commit section)))
                   magit-root-section)
                  (should commit)
                  (goto-char (oref commit start))
                  (search-forward (oref (oref commit value) sha)
                                  (oref commit end) t)
                  (beginning-of-line)
                  (should (eq (key-binding (kbd "RET"))
                              'git-appraise-section-visit-commit))
                  (cl-letf (((symbol-function 'magit-show-commit)
                             (lambda (oid &rest _)
                               (setq shown (cons oid default-directory)))))
                    (call-interactively (key-binding (kbd "RET"))))
                  (should (equal (car shown) revision))
                  (should (equal (directory-file-name (file-truename (cdr shown)))
                                 (file-truename directory))))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

;;; req-2026-11-24-123: folding from within commits returns to heading
;; covers: git-appraise-folding.el
;; covers-fn: git-appraise-fold-toggle-at-point
;; enforcement: test
(ert-deftest git-appraise-backend-request/req-2026-11-24-123-fold-commits-from-row ()
  "WHEN a reviewer folds commits from a listed revision THE SYSTEM SHALL fold the group and place the cursor on its heading."
  (git-appraise-backend-request-test-with-repo
    (git-appraise-test--with-db
      (let ((git-appraise-history-enabled nil)
            buffer)
        (unwind-protect
            (progn
              (git-appraise-backend--create directory "Review" "feature" "master" nil)
              (git-appraise-backend-open revision)
              (let ((git-appraise-section-full-refresh? t))
                (git-appraise--build-buffer))
              (setq buffer (get-buffer (git-appraise-pr-buffer-name)))
              (git-appraise-test--drain-lazy buffer)
              (with-current-buffer buffer
                (let (group commit)
                  (magit-map-sections
                   (lambda (section)
                     (pcase (oref section type)
                       ('git-appraise-commits-header-section
                        (setq group section))
                       ('git-appraise-commit-section
                        (setq commit section))))
                   magit-root-section)
                  (should (and group commit))
                  (magit-section-show group)
                  (goto-char (oref commit start))
                  (should (eq (key-binding (kbd "TAB"))
                              'git-appraise-fold-toggle-at-point))
                  (call-interactively (key-binding (kbd "TAB")))
                  (should (oref group hidden))
                  (should (= (point) (oref group start)))
                  (should (invisible-p (oref commit start)))
                  (call-interactively (key-binding (kbd "TAB")))
                  (should-not (oref group hidden))
                  (should (= (point) (oref group start))))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

;;; git-appraise-backend-request-test.el ends here
