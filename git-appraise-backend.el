;;; git-appraise-backend.el --- Local git-appraise backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dmitry Moskowski

;;; Commentary:
;; Review data is read and written by git-appraise.  The git-appraise database
;; only holds the same UI cache and unfinished drafts used by other backends.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'xml)
(require 'a)
(require 'dash)
(require 'git-appraise-db)
(require 'git-appraise-interfaces)
(require 'git-appraise-parse-hunk)
(require 'git-appraise-time)
(require 'git-appraise-utils)

(declare-function git-appraise-viewed--refresh "git-appraise-viewed"
                  (repo revision))

;; Declared valueless: `git-appraise.el' defines the defcustom, and a
;; value here would pre-bind it and kill that default.
(defvar git-appraise-start-backend)

(defclass git-appraise-backend-repo (git-appraise-db-pullreq)
  ())

(defclass git-appraise-submit-appraise-review ()
  ((state :initform nil)
   (pr :initform nil)
   (local-comments :initform nil)
   (feedback :initform nil)))

(defclass git-appraise-submit-appraise-replies ()
  ((pr :initform nil)
   (replies :initform nil)))

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

(defun git-appraise-backend-list (&optional directory)
  "Return all git-appraise review summaries in DIRECTORY."
  (let* ((root (or directory (git-appraise-backend--root)))
         (output (git-appraise-backend--run root "list" "-a" "-json"))
         (reviews (progn
                    (unless (string-match-p
                             "\\`[ \t\n]*\\(?:\\[\\|null\\)" output)
                      (user-error "Invalid git-appraise review list"))
                    (git-appraise-backend--json output "list"))))
    (unless (or (null reviews)
                (and (listp reviews)
                     (cl-every (lambda (review)
                                 (and (stringp (a-get review 'revision))
                                      (listp (a-get review 'request))))
                               reviews)))
      (user-error "Invalid git-appraise review list"))
    reviews))

(defun git-appraise-backend--branch-exists-p (root branch)
  "Return non-nil when BRANCH is a local branch in ROOT."
  (ignore-errors
    (git-appraise-backend--git root "rev-parse" "--verify" "--quiet"
                               (concat "refs/heads/" branch))
    t))

(defun git-appraise-backend--default-target (root)
  "Return the branch to propose as the target of a new review in ROOT.
Prefer the remote's default branch, then main, then master, but only
offer branches that exist locally: git-appraise verifies the target
ref in the local repository."
  (let* ((remote-head (ignore-errors
                        (git-appraise-backend--git
                         root "symbolic-ref" "--short" "refs/remotes/origin/HEAD")))
         (candidate (and (stringp remote-head)
                         (not (string-empty-p remote-head))
                         (replace-regexp-in-string "\\`[^/]+/" "" remote-head))))
    (or (and candidate
             (git-appraise-backend--branch-exists-p root candidate)
             candidate)
        (cl-find-if (lambda (branch)
                      (git-appraise-backend--branch-exists-p root branch))
                    '("main" "master"))
        "master")))

(defun git-appraise-backend--review-commit (root source target-ref)
  "Return the commit git-appraise anchors a review of SOURCE on TARGET-REF."
  (let* ((source-commit (git-appraise-backend--git
                         root "rev-parse" "--verify" (concat source "^{commit}")))
         (base (git-appraise-backend--git root "merge-base" target-ref source-commit))
         (commits (split-string
                   (git-appraise-backend--git root "rev-list" "--reverse"
                                              (concat base ".." source-commit))
                   "\n" t)))
    (unless commits
      (user-error "No commits to review between %s and %s" source target-ref))
    (car commits)))

(defun git-appraise-backend--create (root message source target reviewers)
  "Create a review in ROOT and return its revision.
MESSAGE may be empty, in which case git-appraise uses the commit
message.  SOURCE is the revision to review and TARGET the branch to
review against.  REVIEWERS is a list of names.  Signal `user-error'
when an open review already covers the same commit."
  (let* ((source-ref (let ((full (ignore-errors
                                   (git-appraise-backend--git
                                    root "rev-parse" "--symbolic-full-name"
                                    source))))
                       (if (and full (not (string-empty-p full))) full source)))
         (target-ref (if (string-prefix-p "refs/" target)
                         target
                       (concat "refs/heads/" target)))
         (review-commit (git-appraise-backend--review-commit
                         root source-ref target-ref))
         (open (cl-find-if
                (lambda (review)
                  (and (equal (a-get review 'revision) review-commit)
                       (not (eq (a-get review 'submitted) t))))
                (git-appraise-backend-list root))))
    (unless (string-prefix-p "refs/" source-ref)
      (user-error "Review source must be a branch or ref, not %s" source))
    (when open
      (user-error "An open review already exists for %s; open it instead"
                  (substring review-commit 0 (min 12 (length review-commit)))))
    (apply #'git-appraise-backend--run
           root
           (append (list "request" "-quiet")
                   (unless (string-empty-p (string-trim (or message "")))
                     (list "-m" message))
                   (list "-source" source-ref "-target" target-ref)
                   (when reviewers
                     (list "-r" (string-join reviewers ",")))))
    (unless (cl-find-if (lambda (review)
                          (equal (a-get review 'revision) review-commit))
                        (git-appraise-backend-list root))
      (user-error "git-appraise did not report the new review"))
    review-commit))

(defun git-appraise-backend--read-request (root)
  "Prompt for a new review in ROOT.
Return a list (MESSAGE SOURCE TARGET REVIEWERS)."
  (let ((branch (git-appraise-backend--git root "rev-parse" "--abbrev-ref" "HEAD")))
    (when (equal branch "HEAD")
      (user-error "Check out a branch before creating a review"))
    (let* ((source (read-string "Source branch: " branch))
           (target (read-string "Target branch: "
                                (git-appraise-backend--default-target root)))
           (message (read-string "Review message (empty uses the commit message): "))
           (reviewers (split-string
                       (read-string "Reviewers (comma separated, optional): ")
                       "," t "[ \t\n]+")))
      (list message source target reviewers))))

(declare-function git-appraise-backend--state "git-appraise-backend" (review))

(defun git-appraise-backend--short-ref (ref)
  "Return REF as a short branch name for display."
  (if (and (stringp ref) (not (string-empty-p ref)))
      (replace-regexp-in-string "\\`refs/heads/" "" ref)
    "?"))

(defun git-appraise-backend--format-unix-time (stamp)
  "Format git-appraise unix STAMP as a short local time, or \"-\"."
  (git-appraise-time-format-unix stamp "%Y-%m-%d %H:%M" nil "-"))

(defun git-appraise-backend--review-last-comment-ts (review)
  "Return the latest comment unix timestamp string in REVIEW, or nil."
  (let ((latest nil))
    (cl-labels ((num (s)
                  (and (stringp s) (string-match-p "\\`[0-9]+\\'" s)
                       (string-to-number s)))
                (walk (thread)
                  (when-let* ((ts (num (a-get-in thread '(comment timestamp)))))
                    (when (or (null latest) (> ts latest))
                      (setq latest ts)))
                  (dolist (child (a-get thread 'children))
                    (walk child))))
      (dolist (thread (a-get review 'comments))
        (walk thread)))
    (and latest (number-to-string latest))))

(defun git-appraise-backend--review-choice (review)
  "Return a (DISPLAY . REVISION) choice for git-appraise REVIEW."
  (let* ((revision (a-get review 'revision))
         (request (a-get review 'request))
         (target (git-appraise-backend--short-ref (a-get request 'targetRef)))
         (source (git-appraise-backend--short-ref (a-get request 'reviewRef)))
         (created (git-appraise-backend--format-unix-time (a-get request 'timestamp)))
         (last (git-appraise-backend--format-unix-time
                (git-appraise-backend--review-last-comment-ts review)))
         (state (git-appraise-backend--state review))
         (desc (or (car (split-string (or (a-get request 'description) "") "\n" t))
                   ""))
         (desc (if (> (length desc) 60)
                   (concat (substring desc 0 57) "...")
                 desc))
         (rev12 (substring revision 0 (min 12 (length revision)))))
    (list :display (format "%s..%s  created %s  last %s  %-9s  %s  %s"
                           target source created last state rev12 desc)
          :revision revision
          :refs (format "%s..%s" target source)
          :created created
          :last last
          :state state
          :rev rev12
          :desc desc)))

(defun git-appraise-backend-select-review ()
  "Choose a local git-appraise review and return its revision.
When the repository has no reviews, offer to create one."
  (let* ((root (git-appraise-backend--root))
         (reviews (git-appraise-backend-list root)))
    (if (null reviews)
        (if (y-or-n-p (format "No git-appraise reviews in %s; create one? " root))
            (apply #'git-appraise-backend--create root
                   (git-appraise-backend--read-request root))
          (user-error "No git-appraise reviews found in this repository"))
      (let* ((rows (--map (git-appraise-backend--review-choice it) reviews))
             (ref-w (apply #'max 0 (--map (length (plist-get it :refs)) rows)))
             (fmt (format "%%-%ds  created %%s  last %%s  %%-%ds  %%s  %%s" ref-w 9))
             (choices (--map (cons (format fmt
                                           (plist-get it :refs)
                                           (plist-get it :created)
                                           (plist-get it :last)
                                           (plist-get it :state)
                                           (plist-get it :rev)
                                           (plist-get it :desc))
                                   (plist-get it :revision))
                             rows)))
        (cdr (assoc (completing-read "Git-appraise review: " choices nil t)
                    choices))))))

(defun git-appraise-backend-open (revision)
  "Create a git-appraise object for git-appraise REVISION in the current Git repo."
  (let ((root (git-appraise-backend--root)))
    (unless (and (stringp revision) (not (string-empty-p revision)))
      (user-error "A git-appraise revision is required"))
    (git-appraise-db--pullreq-create
     (git-appraise-backend-repo
      :owner root :repo (file-name-nondirectory root)
      :number (git-appraise-backend--git
               root "rev-parse" "--verify" (concat revision "^{commit}"))))))

(declare-function git-appraise-start "git-appraise" (url))

;;;###autoload
(defun git-appraise-backend-request ()
  "Create a git-appraise review for the current repository and open it.
The source defaults to the current branch, the target to the remote's
default branch, and an empty message to the commit message.  Reviews
are local: git-appraise notes are not pushed."
  (interactive)
  (require 'git-appraise)
  (let* ((root (git-appraise-backend--root))
         (revision (apply #'git-appraise-backend--create root
                          (git-appraise-backend--read-request root))))
    (let ((git-appraise-start-backend 'appraise))
      (git-appraise-start revision))
    revision))

(defun git-appraise-backend--diff-tables (diff)
  "Return an alist of file paths and parsed hunk tables from DIFF."
  (let (result path lines in-hunk)
    (cl-labels ((finish ()
                  (when (and path lines)
                    (push (cons path
                                (git-appraise-parse-hunk-table
                                 (string-join (nreverse lines) "\n")))
                          result))))
      (dolist (line (split-string diff "\n" t))
        (cond
         ((string-prefix-p "diff --git " line)
          (finish)
          (setq path nil lines nil in-hunk nil))
         ((and (not in-hunk) (string-prefix-p "+++ " line))
          (let ((name (substring line 4)))
            (setq path (unless (string-equal name "/dev/null")
                         (string-remove-prefix "b/" name)))))
         ((string-prefix-p "@@ " line)
          (setq in-hunk t)
          (push line lines))
         (in-hunk (push line lines))))
      (finish))
    (nreverse result)))

(defun git-appraise-backend--row (tables path position)
  "Find the diff row at POSITION in PATH within TABLES."
  (cl-find-if (lambda (row) (equal (a-get row 'relative) position))
              (alist-get path tables nil nil #'equal)))

(defun git-appraise-backend--position (tables path line)
  "Find the exact new-side diff position of PATH at LINE in TABLES."
  (let ((row (cl-find-if
              (lambda (item)
                (and (member (a-get item 'type) '("add" "normal"))
                     (equal (or (a-get item 'ln2) (a-get item 'ln)) line)))
              (alist-get path tables nil nil #'equal))))
    (a-get row 'relative)))

(defun git-appraise-backend--timestamp (stamp)
  "Convert git-appraise Unix STAMP to the timestamp used by sections."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (or (git-appraise-time-parse-unix stamp)
                          (seconds-to-time 0))
                      t))

(defun git-appraise-backend--html (body)
  "Convert plain text BODY to safe HTML for existing comment sections."
  (replace-regexp-in-string "\n" "<br />"
                            (xml-escape-string (or body "")) t t))

(defun git-appraise-backend--head (directory review)
  "Return a safe head commit for inline mapping of REVIEW in DIRECTORY."
  (let* ((revision (a-get review 'revision))
         (ref (a-get-in review '(request reviewRef)))
         (candidate (when (and (stringp ref) (not (string-empty-p ref)))
                      (condition-case nil
                          (git-appraise-backend--git directory
                                                      "rev-parse" "--verify"
                                                      (concat ref "^{commit}"))
                        (error nil)))))
    (if (and candidate
             (eq 0 (let ((default-directory (file-name-as-directory directory)))
                     (process-file "git" nil nil nil "merge-base"
                                   "--is-ancestor" revision candidate))))
        candidate
      revision)))

(defun git-appraise-backend--commits (directory review head)
  "Return review commits in the shape consumed by the commit renderer."
  (let* ((target (a-get-in review '(request targetRef)))
         (base (and target (git-appraise-backend--git
                            directory "merge-base" target head)))
         (output (git-appraise-backend--git
                  directory "log" "--reverse" "--format=%H%x1f%h%x1f%s%x1f%cI"
                  (concat base ".." head))))
    (mapcar (lambda (line)
              (pcase-let ((`(,oid ,sha ,msg ,date) (split-string line "\x1f")))
                `((commit (abbreviatedOid . ,sha)
                          (oid . ,oid)
                          (message . ,msg)
                          (committedDate . ,date)))))
            (split-string output "\n" t))))

(defun git-appraise-backend--state (review)
  "Return a git-appraise state string for git-appraise REVIEW."
  (let ((resolved (a-get review 'resolved))
         (submitted (eq (a-get review 'submitted) t)))
    (cond
     ((eq resolved :false) "REJECTED")
     (submitted "SUBMITTED")
     (resolved "APPROVED")
     (t "PENDING"))))

(defun git-appraise-backend--normalize (review diff head)
  "Normalize git-appraise REVIEW and DIFF for the existing UI at HEAD.
Return a pair of normalized infos and inline comments."
  (let* ((request (a-get review 'request))
         (revision (a-get review 'revision))
         (description (or (a-get request 'description) ""))
         (title (or (car (split-string description "\n" t))
                    (substring revision 0 (min 12 (length revision)))))
         (tables (and (a-get review 'comments)
                      (git-appraise-backend--diff-tables diff)))
         (inline nil)
         (historical nil)
         (conversation nil))
    (cl-labels
        ((visit (thread inherited-location)
           (let* ((comment (a-get thread 'comment))
                  (location (a-get comment 'location))
                  (location (if (a-get location 'path) location inherited-location))
                  (path (a-get location 'path))
                  (range (a-get location 'range))
                  (line (a-get range 'startLine))
                  (commit (a-get location 'commit))
                  (position (and (stringp path) (numberp line) (> line 0)
                                 (equal commit head)
                                 (git-appraise-backend--position tables path line)))
                  (hash (a-get thread 'hash))
                  (author (or (a-get comment 'author) "unknown"))
                  (body (git-appraise-backend--html
                         (a-get comment 'description)))
                  (timestamp (git-appraise-backend--timestamp
                              (a-get comment 'timestamp)))
                  (state (cond ((eq (a-get comment 'resolved) :false)
                                "CHANGES_REQUESTED")
                               ((a-get comment 'resolved) "APPROVED")
                               (t "COMMENTED"))))
             (cond
              (position
               (push `((author (login . ,author))
                       (state . ,state)
                       (comments (nodes ((bodyHTML . ,body)
                                         (bodyText . ,(a-get comment 'description))
                                         (path . ,path)
                                         (position . ,position)
                                         (databaseId . ,hash)
                                         (createdAt . ,timestamp)
                                         (updatedAt . ,timestamp)))))
                     inline))
              ((and (stringp path) (numberp line) (> line 0))
               (push `((author (login . ,author))
                       (bodyText . ,(a-get comment 'description))
                       (path . ,path) (line . ,line) (commit . ,commit)
                       (state . ,state) (databaseId . ,hash)
                       (createdAt . ,timestamp))
                     historical))
              (t
               (let ((where (if path
                                (format "%s @ %s" path (or commit "?"))
                              (format "Commit %s" (or commit revision)))))
                 (push `((author (login . ,author))
                         (bodyHTML . ,(concat "<strong>" (xml-escape-string where)
                                              "</strong><br />" body))
                         (databaseId . ,hash)
                         (createdAt . ,timestamp))
                       conversation))))
             (dolist (child (a-get thread 'children))
               (visit child location)))))
      (dolist (thread (a-get review 'comments))
        (visit thread nil)))
    (cons
     `((title . ,title)
       (state . ,(git-appraise-backend--state review))
       (bodyText . ,description)
       (headRefOid . ,head)
       (baseRefName . ,(or (a-get request 'targetRef) ""))
       (headRefName . ,(or (a-get request 'reviewRef) revision))
       (author (login . ,(or (a-get request 'requester) "unknown")))
       (reviewRequests
        (nodes . ,(mapcar (lambda (reviewer)
                           `((requestedReviewer (login . ,reviewer))))
                         (a-get request 'reviewers))))
       (historicalComments (nodes . ,(nreverse historical)))
       (comments (nodes . ,(nreverse conversation))))
     (nreverse inline))))

(defun git-appraise-backend--validate-review (review revision)
  "Validate parsed REVIEW for REVISION.  Return REVIEW or signal `user-error'."
  (unless (and (listp review)
               (equal (a-get review 'revision) revision)
               (listp (a-get review 'request)))
    (user-error "Invalid git-appraise review for %s" revision))
  review)

(defun git-appraise-backend--read-review (directory revision)
  "Read and validate REVIEW's show -json for REVISION in DIRECTORY.
Signal `user-error' when the review data is unusable."
  (git-appraise-backend--validate-review
   (git-appraise-backend--json
    (git-appraise-backend--run directory "show" "-json" revision)
    "show")
   revision))

(defun git-appraise-backend-refresh (obj buffer-name &optional message)
  "Reload OBJ from Git and render BUFFER-NAME, optionally showing MESSAGE."
  (let* ((directory (oref obj owner))
         (revision (oref obj number))
         (old-head (oref obj sha))
         (old-comments (oref obj raw-comments))
         (fetched (git-appraise-backend--run-parallel
                   directory
                   `((json "git-appraise" "show" "-json" ,revision)
                     (diff "git-appraise" "show" "-diff" ,revision))))
         (review (git-appraise-backend--validate-review
                  (git-appraise-backend--json (alist-get 'json fetched) "show")
                  revision))
         (diff (git-appraise-utils--clean-diff-prefixes
                (alist-get 'diff fetched)))
         (head (git-appraise-backend--head directory review))
         (normalized
          (condition-case err
              (git-appraise-backend--normalize review diff head)
            (error (user-error "Invalid git-appraise review data: %s"
                               (error-message-string err))))))
    (git-appraise-db--pullreq-raw-diff-update diff)
    (setf (alist-get 'commits (car normalized))
          `((nodes . ,(git-appraise-backend--commits directory review head))))
    (git-appraise-db--pullreq-raw-infos-update (car normalized))
    (let ((current (git-appraise-db-get-pullreq)))
      (oset current raw-comments
            (git-appraise-db--reconcile-raw-comments
             (cdr normalized) old-comments))
      (git-appraise-db-update current)
      (when (fboundp 'git-appraise-pr-state-persist)
        (git-appraise-pr-state-persist current)))
    ;; personal viewed state lives in a git notes ref anchored on
    ;; the review revision: re-read it so the render projects the
    ;; freshest note.  Read failures are logged inside and never
    ;; fatal; this is not a provider error.
    (git-appraise-viewed--refresh directory revision)
    (git-appraise--trigger-hooks buffer-name nil message)))

(defun git-appraise-backend-refresh-comments (obj)
  "Refresh only the conversation metadata of the review OBJ.
One `show -json' read, no diff reload and no buffer rebuild: the
stored conversation is re-normalized against the cached diff and
the current head, keeping local drafts through the usual
reconciliation.  Return `ok' when the stored set was updated,
`head-changed' when the review head moved since the cached diff
(the caller should fall back to `git-appraise-backend-refresh'),
or `failed' when the metadata could not be read.  Never signals:
a metadata failure must not turn an already committed send into a
reported send failure."
  (condition-case err
      (let* ((directory (oref obj owner))
             (revision (oref obj number))
             (review (git-appraise-backend--read-review directory revision))
             (head (git-appraise-backend--head directory review))
             (cached-head (and (slot-boundp obj 'sha) (oref obj sha))))
        (if (and cached-head (not (equal cached-head head)))
            'head-changed
          (pcase-let ((normalized
                       (git-appraise-backend--normalize
                        review (git-appraise-db--pullreq-raw-diff) head))
                      (current (git-appraise-db-get-pullreq)))
            (setf (alist-get 'commits (car normalized))
                  (a-get (oref current raw-infos) 'commits))
            (oset current raw-infos (car normalized))
            (oset current raw-comments
                  (git-appraise-db--reconcile-raw-comments
                   (cdr normalized) (oref current raw-comments)))
            (git-appraise-db-update current)
            'ok)))
    (error
     (git-appraise-utils--log
      "git-appraise-backend-refresh-comments" (prin1-to-string err))
     'failed)))

(defun git-appraise-backend--send-comment (pr body &optional path line parent)
  "Write BODY to PR, optionally on PATH, LINE, or as a reply to PARENT."
  (when (string-empty-p (string-trim (or body "")))
    (user-error "git-appraise comments require a message"))
  (let ((args (list "comment" "-F" "-")))
    (when parent (setq args (append args (list "-p" parent))))
    (when path (setq args (append args (list "-f" path))))
    (when line (setq args (append args (list "-l" (number-to-string line)))))
    ;; Go's flag parser stops at the first positional argument.
    (apply #'git-appraise-backend--run-message
           (oref pr owner) body
           (append args (list (oref pr number))))))

(defun git-appraise-backend--line-for-comment (pr comment)
  "Return the head-file line for COMMENT in PR, or signal a useful error.
When the review head moved, refresh the review and remap the comment
against the new diff instead of blocking the send."
  (when (oref pr sha)
    (let* ((review (git-appraise-backend--json
                    (git-appraise-backend--run
                     (oref pr owner) "show" "-json" (oref pr number))
                    "show"))
           (head (git-appraise-backend--head (oref pr owner) review)))
      (unless (equal (oref pr sha) head)
        ;; Head moved: refresh so the diff/snapshot is current, then remap.
        (git-appraise-backend-refresh
         pr (git-appraise-pr-buffer-name pr)
         "Review head changed; refreshed")
        (setq pr (or (git-appraise-db-get-pullreq) pr)))))
  (let* ((path (oref comment path))
         (side (and (slot-boundp comment 'side) (oref comment side)))
         (tables (git-appraise-backend--diff-tables (oref pr raw-diff)))
         (position (or (oref comment position)
                       (and (equal side "RIGHT")
                            (numberp (oref comment line))
                            (git-appraise-backend--position
                             tables path (oref comment line)))))
         (row (git-appraise-backend--row tables path position))
         (type (a-get row 'type)))
    (when (or (equal side "LEFT")
              (string-equal (oref comment line-type) "REMOVED"))
      (user-error "git-appraise cannot comment on a removed line"))
    (unless row
      (user-error "No current diff line at %s:%s; refresh the review and retry"
                  path (or (oref comment line) position)))
    (when (string-equal type "del")
      (user-error "git-appraise cannot comment on a removed line"))
    (or (a-get row 'ln2) (a-get row 'ln))))

(cl-defmethod git-appraise-new-code-comment
  ((pr git-appraise-backend-repo) local-comment callback)
  "Send LOCAL-COMMENT in PR and call CALLBACK on success."
  (let ((path (oref local-comment path)))
    (if (equal (oref local-comment line-type) "FILE")
        (git-appraise-backend--send-comment pr (oref local-comment msg) path)
      (git-appraise-backend--send-comment
       pr (oref local-comment msg) path
       (git-appraise-backend--line-for-comment pr local-comment))))
  (funcall callback))

(cl-defmethod git-appraise-new-issue-comment
  ((pr git-appraise-backend-repo) comment-msg callback)
  "Send a general COMMENT-MSG for PR and call CALLBACK."
  (git-appraise-backend--send-comment pr comment-msg)
  (funcall callback))

(defun git-appraise-backend-send-reply (pr parent body)
  "Send BODY as a reply to PARENT in PR."
  (unless (and (stringp parent) (not (string-empty-p parent)))
    (user-error "Cannot reply: git-appraise comment hash is missing"))
  (git-appraise-backend--send-comment pr body nil nil parent))

(cl-defmethod git-appraise-send-review
  ((review git-appraise-submit-appraise-review) callback)
  "Send REVIEW comments and verdict, then call CALLBACK."
  (let* ((pr (oref review pr))
         (state (oref review state))
         (feedback (oref review feedback)))
    (condition-case err
        (progn
          (when (and (equal state "COMMENT")
                     (not (oref review local-comments))
                     (string-empty-p (string-trim (or feedback ""))))
            (user-error "Add a comment before submitting"))
          (when (and (equal state "REQUEST_CHANGES")
                     (string-empty-p (string-trim (or feedback ""))))
            (user-error "A message is required to request changes"))
          (dolist (comment (nreverse (oref review local-comments)))
            (git-appraise-backend--send-comment
             pr (oref comment body) (oref comment path)
             (git-appraise-backend--line-for-comment pr comment))
            (git-appraise-db-delete-raw-comment (oref comment internal-id)))
          (pcase state
            ("APPROVE"
             (git-appraise-backend--run-message
              (oref pr owner) (or feedback "")
              "accept" "-F" "-" (oref pr number)))
            ("REQUEST_CHANGES"
             (git-appraise-backend--run-message
              (oref pr owner) feedback
              "reject" "-F" "-" (oref pr number)))
            ("COMMENT"
             (when (and feedback (not (string-empty-p (string-trim feedback))))
               (git-appraise-backend--send-comment pr feedback)))
            (_ (user-error "Unsupported git-appraise review action: %s" state)))
          (funcall callback))
      (error
       ;; Successful comments were removed from the draft cache one by one.
       ;; Redraw the remaining drafts before a retry can send them again.
       (let ((git-appraise-section-full-refresh? nil))
         (ignore-errors (git-appraise--build-buffer)))
       (signal (car err) (cdr err))))))

(cl-defmethod git-appraise-send-replies
  ((replies git-appraise-submit-appraise-replies) callback)
  "Send REPLIES to git-appraise and call CALLBACK."
  (dolist (reply (oref replies replies))
    (git-appraise-backend-send-reply
     (oref replies pr) (oref reply reply-to-id) (oref reply body))
    (git-appraise-db-delete-raw-comment (oref reply internal-id)))
  (funcall callback))

(defun git-appraise-backend-abandon (directory revision message)
  "Abandon REVIEW REVISION in DIRECTORY with MESSAGE."
  (let ((msg (or message "")))
    (apply #'git-appraise-backend--run directory
           (append (if (string-empty-p (string-trim msg))
                       '("abandon")
                     (list "abandon" "-m" msg))
                   (list revision)))))

;;;###autoload
(defun git-appraise-abandon-review (revision)
  "Abandon git-appraise REVISION in the current repository."
  (interactive
   (list (git-appraise-backend-select-review)))
  (let* ((root (git-appraise-backend--root))
         (message (read-string "Abandon message (optional): ")))
    (git-appraise-backend-abandon root revision message)
    (message "Review abandoned: %s" revision)))

(defun git-appraise-backend-push (directory &optional remote)
  "Push git-appraise notes from DIRECTORY to REMOTE.
REMOTE defaults to \"origin\"."
  (git-appraise-backend--run directory "push" (or remote "origin")))

(defun git-appraise-backend-pull (directory &optional remote)
  "Pull git-appraise notes into DIRECTORY from REMOTE.
REMOTE defaults to \"origin\"."
  (git-appraise-backend--run directory "pull" (or remote "origin")))

;;;###autoload
(defun git-appraise-push-notes (&optional remote)
  "Push git-appraise notes to REMOTE."
  (interactive
   (list (read-string "Remote: " "origin")))
  (let ((root (git-appraise-backend--root)))
    (message "Pushing git-appraise notes to %s..." remote)
    (git-appraise-backend-push root remote)
    (message "Pushed git-appraise notes to %s" remote)))

;;;###autoload
(defun git-appraise-pull-notes (&optional remote)
  "Pull git-appraise notes from REMOTE."
  (interactive
   (list (read-string "Remote: " "origin")))
  (let ((root (git-appraise-backend--root)))
    (message "Pulling git-appraise notes from %s..." remote)
    (git-appraise-backend-pull root remote)
    (when (fboundp 'git-appraise--build-buffer)
      (ignore-errors (git-appraise--build-buffer)))
    (message "Pulled git-appraise notes from %s" remote)))

(defun git-appraise-backend-rebase (directory &optional revision)
  "Rebase git-appraise REVIEW REVISION in DIRECTORY onto its target."
  (let ((process-environment
         (append '("GIT_SEQUENCE_EDITOR=true" "GIT_EDITOR=true")
                 process-environment)))
    (apply #'git-appraise-backend--run directory
           (append '("rebase") (when revision (list revision))))))

;;;###autoload
(defun git-appraise-rebase-review (revision)
  "Rebase git-appraise REVISION in the current repository."
  (interactive
   (list (git-appraise-backend-select-review)))
  (unless (yes-or-no-p (format "Rebase review %s onto its target? " revision))
    (user-error "Rebase cancelled"))
  (let ((root (git-appraise-backend--root)))
    (message "Rebasing review %s..." revision)
    (git-appraise-backend-rebase root revision)
    (message "Review rebased: %s" revision)))

(defun git-appraise-backend-land-review (directory &optional revision strategy)
  "Land git-appraise REVIEW REVISION in DIRECTORY.
STRATEGY may be nil, \"merge\", \"rebase\", or \"fast-forward\"."
  (let ((process-environment
         (append '("GIT_SEQUENCE_EDITOR=true" "GIT_EDITOR=true")
                 process-environment))
        (args (append '("submit")
                      (pcase strategy
                        ("merge" '("--merge"))
                        ("rebase" '("--rebase"))
                        ("fast-forward" '("--fast-forward"))
                        ((or 'nil "") nil)
                        (_ (user-error "Unknown submit strategy: %s" strategy)))
                      (when revision (list revision)))))
    (apply #'git-appraise-backend--run directory args)))

;;;###autoload
(defun git-appraise-land-review (revision strategy)
  "Land git-appraise REVISION in the current repository using STRATEGY."
  (interactive
   (let ((rev (git-appraise-backend-select-review))
         (strategy (completing-read "Submit strategy: "
                                    '("default" "merge" "rebase" "fast-forward")
                                    nil t nil nil "default")))
     (list rev (unless (equal strategy "default") strategy))))
  (unless (yes-or-no-p (format "Land review %s%s? "
                               revision
                               (if strategy (format " (%s)" strategy) "")))
    (user-error "Land cancelled"))
  (let ((root (git-appraise-backend--root)))
    (message "Landing review %s..." revision)
    (git-appraise-backend-land-review root revision strategy)
    (message "Review landed: %s" revision)))

(provide 'git-appraise-backend)
;;; git-appraise-backend.el ends here
