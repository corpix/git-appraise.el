;;; git-appraise-backend-read.el --- Native review reads and render projection -*- lexical-binding: t; -*-

;;; Commentary:
;; Read git-appraise review events and project them into the section renderer.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'xml)
(require 'a)
(require 'dash)
(require 'git-appraise-db)
(require 'git-appraise-backend-process)
(require 'git-appraise-parse-hunk)
(require 'git-appraise-time)
(require 'git-appraise-utils)

;; Declared valueless: `git-appraise.el' defines the defcustom, and a
;; value here would pre-bind it and kill that default.
(defvar git-appraise-start-backend)

(defclass git-appraise-native-review ()
  ((revision :initarg :revision)
   (request :initarg :request)
   (head :initarg :head)
   (base :initarg :base)
   (threads :initarg :threads)
   (resolved :initarg :resolved)
   (submitted :initarg :submitted))
  "Native git-appraise review identity and discussion state.")

(defclass git-appraise-backend-repo (git-appraise-db-review)
  ((native-review :initform nil))
  "A native review plus the in-memory section renderer projection.")

(defun git-appraise-backend--native-review (directory review head)
  "Build the canonical native record for REVIEW at HEAD in DIRECTORY."
  (git-appraise-native-review
   :revision (a-get review 'revision)
   :request (a-get review 'request)
   :head head
   :base (git-appraise-backend--base directory review head)
   :threads (a-get review 'comments)
   :resolved (a-get review 'resolved)
   :submitted (eq (a-get review 'submitted) t)))

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

(defun git-appraise-backend--create (root message source target reviewers
                                        &optional allow-uncommitted)
  "Create a review in ROOT and return its revision.
MESSAGE may be empty, in which case git-appraise uses the commit
message.  SOURCE is the revision to review and TARGET the branch to
review against.  REVIEWERS is a list of names.  ALLOW-UNCOMMITTED
passes the CLI override after the interactive warning for untracked
files.  Signal `user-error' when an open review already covers the
same commit."
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
                   (when allow-uncommitted
                     (list "-allow-uncommitted"))
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

(defun git-appraise-backend--request-untracked-choice (root)
  "Check ROOT before review prompts; return non-nil to allow untracked files."
  (let* ((lines (split-string
                 (git-appraise-backend--git root "status" "--porcelain=v1"
                                            "--untracked-files=normal")
                 "\n" t))
         (tracked (--any? (not (string-prefix-p "?? " it)) lines)))
    (cond
     (tracked
      (user-error "Commit or stash tracked changes before creating a git-appraise review"))
     ((null lines) nil)
     ((y-or-n-p
       "Untracked files will not be included in the review; continue anyway? ")
      t)
     (t
      (user-error "Review creation cancelled; untracked files remain")))))

(defun git-appraise-backend--read-request (root)
  "Prompt for a new review in ROOT.
Return a list (MESSAGE SOURCE TARGET REVIEWERS ALLOW-UNCOMMITTED)."
  (let ((branch (git-appraise-backend--git root "rev-parse" "--abbrev-ref" "HEAD")))
    (when (equal branch "HEAD")
      (user-error "Check out a branch before creating a review"))
    (let* ((allow-uncommitted
            (git-appraise-backend--request-untracked-choice root))
           (source (read-string "Source branch: " branch))
           (target (read-string "Target branch: "
                                (git-appraise-backend--default-target root)))
           (message (read-string "Review message (empty uses the commit message): "))
           (reviewers (split-string
                       (read-string "Reviewers (comma separated, optional): ")
                       "," t "[ \t\n]+")))
      (list message source target reviewers allow-uncommitted))))

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
  (git-appraise-utils--timed
   "comment:diff-parse"
   (lambda ()
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
      (nreverse result)))))

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
  (let* ((revision (or (a-get-in review '(request alias))
                       (a-get review 'revision)))
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
  (let* ((base (git-appraise-backend--base directory review head))
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

(defun git-appraise-backend--base (directory review head)
  "Return the native base commit for REVIEW at HEAD in DIRECTORY."
  (let* ((request (a-get review 'request))
         (target (a-get request 'targetRef))
         (saved (a-get request 'baseCommit)))
    (cond
     ((and (eq (a-get review 'submitted) t)
           (stringp saved) (not (string-empty-p saved)))
      saved)
     ((and (stringp target) (not (string-empty-p target)))
      (git-appraise-backend--git directory "merge-base" target head))
     (t (git-appraise-backend--git directory "rev-parse"
                                   (concat (a-get review 'revision) "^"))))))

(defun git-appraise-backend--state (review)
  "Return a git-appraise state string for git-appraise REVIEW."
  (let ((resolved (a-get review 'resolved))
         (submitted (eq (a-get review 'submitted) t)))
    (cond
     ((eq resolved :false) "REJECTED")
     (submitted "SUBMITTED")
     (resolved "APPROVED")
     (t "PENDING"))))

(defun git-appraise-backend--normalize (review diff head &optional cached-tables)
  "Normalize git-appraise REVIEW and DIFF for the existing UI at HEAD.
Return a pair of normalized infos and inline comments."
  (let* ((request (a-get review 'request))
         (revision (a-get review 'revision))
         (description (or (a-get request 'description) ""))
         (title (or (car (split-string description "\n" t))
                    (substring revision 0 (min 12 (length revision)))))
         (tables (and (a-get review 'comments)
                      (or cached-tables
                          (git-appraise-backend--diff-tables diff))))
         (inline nil)
         (historical nil)
         (conversation nil))
    (cl-labels
        ((visit (thread inherited-location root-hash parent-hash)
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
                  (root-hash (or root-hash hash))
                  (thread-resolved (a-get thread 'resolved))
                  (author (or (a-get comment 'author) "unknown"))
                  (body (git-appraise-backend--html
                         (a-get comment 'description)))
                  (timestamp (git-appraise-backend--timestamp
                              (a-get comment 'timestamp)))
                  (state (cond ((eq (a-get comment 'resolved) :false)
                                (if (eq thread-resolved :false)
                                    "CHANGES_REQUESTED" "ADDRESSED"))
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
                                         (state . ,state)
                                         (parentHash . ,parent-hash)
                                         (threadId . ,root-hash)
                                         (isResolved . ,(eq thread-resolved t))
                                         (createdAt . ,timestamp)
                                         (updatedAt . ,timestamp)))))
                     inline))
              ((and (stringp path) (numberp line) (> line 0))
               (push `((author (login . ,author))
                       (bodyText . ,(a-get comment 'description))
                       (path . ,path) (line . ,line) (commit . ,commit)
                       (state . ,state) (databaseId . ,hash)
                       (parentHash . ,parent-hash)
                       (threadId . ,root-hash)
                       (isResolved . ,(eq thread-resolved t))
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
                         (state . ,state)
                         (parentHash . ,parent-hash)
                         (threadId . ,root-hash)
                         (isResolved . ,(eq thread-resolved t))
                         (createdAt . ,timestamp))
                       conversation))))
             (dolist (child (a-get thread 'children))
               (visit child location root-hash hash)))))
      (dolist (thread (a-get review 'comments))
        (visit thread nil nil nil)))
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
  (git-appraise-utils--timed
   "comment:read-review"
   (lambda ()
     (git-appraise-backend--validate-review
      (git-appraise-backend--json
       (git-appraise-backend--run directory "show" "-json" revision)
       "show")
      revision))))

(defun git-appraise-backend-refresh (obj buffer-name &optional message)
  "Reload OBJ from Git and render BUFFER-NAME, optionally showing MESSAGE."
  (let* ((directory (oref obj owner))
         (revision (oref obj number))
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
         (tables (and (a-get review 'comments)
                      (git-appraise-backend--diff-tables diff)))
         (normalized
          (condition-case err
              (git-appraise-backend--normalize review diff head tables)
            (error (user-error "Invalid git-appraise review data: %s"
                               (error-message-string err))))))
    (oset obj native-review (git-appraise-backend--native-review directory review head))
    (git-appraise-db--pullreq-raw-diff-update diff)
    (oset obj diff-tables tables)
    (setf (alist-get 'commits (car normalized))
          `((nodes . ,(git-appraise-backend--commits directory review head))))
    (git-appraise-db--pullreq-raw-infos-update (car normalized))
    (let ((current (git-appraise-db-get-pullreq)))
      (oset current raw-comments
            (git-appraise-db--reconcile-raw-comments
             (cdr normalized) old-comments))
      (git-appraise-db-update current))
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
                        review (git-appraise-db--pullreq-raw-diff) head
                        (or (oref obj diff-tables)
                            (let ((parsed (git-appraise-backend--diff-tables
                                           (oref obj raw-diff))))
                              (oset obj diff-tables parsed)
                              parsed))))
                      (current (git-appraise-db-get-pullreq)))
            (setf (alist-get 'commits (car normalized))
                  (a-get (oref current raw-infos) 'commits))
            (oset current raw-infos (car normalized))
            (oset current native-review
                  (git-appraise-backend--native-review directory review head))
            (oset current raw-comments
                  (git-appraise-db--reconcile-raw-comments
                   (cdr normalized) (oref current raw-comments)))
            (git-appraise-db-update current)
            'ok)))
    (error
     (git-appraise-utils--log
      "git-appraise-backend-refresh-comments" (prin1-to-string err))
     'failed)))


(provide 'git-appraise-backend-read)
;;; git-appraise-backend-read.el ends here
