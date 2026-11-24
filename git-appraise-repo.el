;;; git-appraise-repo.el --- Local repository context for git-appraise -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal repository context for git-appraise reviews.

;;; Code:

(require 'magit-git)
(require 'git-appraise-db)

(defgroup git-appraise-repo nil
  "Local repository integration for git-appraise."
  :group 'git-appraise)

(defcustom git-appraise-repo-enable t
  "When non-nil, bind local repository context for reviews."
  :type 'boolean
  :group 'git-appraise-repo)

(defvar-local git-appraise-repo-worktree nil
  "Worktree directory for the reviewed repository, or nil.")
(put 'git-appraise-repo-worktree 'permanent-local t)

(defvar-local git-appraise-repo-dir nil
  "Local git repository directory for the reviewed PR, or nil.")
(put 'git-appraise-repo-dir 'permanent-local t)

(defun git-appraise-repo-setup (_pr)
  "Set up local repo context.  For appraise, use the repository root."
  (when git-appraise-repo-enable
    (let ((root (or (ignore-errors (oref (git-appraise-db-get-pullreq) owner))
                    (magit-toplevel))))
      (when root
        (setq git-appraise-repo-dir (file-name-as-directory root)
              git-appraise-repo-worktree (file-name-as-directory root))
        git-appraise-repo-worktree))))

(defun git-appraise-repo-insert-header ()
  "Insert a Worktree header line when local repo context is set up."
  (when git-appraise-repo-worktree
    (insert (format "%-17s" "Worktree: "))
    (insert-button git-appraise-repo-worktree
                   'face 'git-appraise-url-header-face
                   'help-echo "Open the PR worktree in Dired"
                   'action (lambda (&rest _)
                             (dired git-appraise-repo-worktree)))
    (insert ?\n)))

(defun git-appraise-repo-open-worktree ()
  "Open the PR worktree for this review in Dired."
  (interactive)
  (if git-appraise-repo-worktree
      (dired git-appraise-repo-worktree)
    (user-error "No local worktree for this review")))

(provide 'git-appraise-repo)
;;; git-appraise-repo.el ends here
