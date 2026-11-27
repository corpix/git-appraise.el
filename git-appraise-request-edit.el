;;; git-appraise-request-edit.el --- Edit native review requests -*- lexical-binding: t; -*-

(require 'dash)
(require 'subr-x)
(require 'transient)
(require 'git-appraise-backend)
(require 'git-appraise-comment)
(require 'git-appraise-db)
(declare-function git-appraise-reload "git-appraise-actions")
(declare-function git-appraise-backend-update-request-fields
                  "git-appraise-backend" (pr changes))
(defvar git-appraise-review-buffer-pr-id)

(defun git-appraise-request-edit--context ()
  "Return (PR . REQUEST) for the current native review."
  (let ((pr (or (and (bound-and-true-p git-appraise-review-buffer-pr-id)
                     (gethash git-appraise-review-buffer-pr-id
                              git-appraise-db--objects))
                (git-appraise-db-get-pullreq))))
    (unless (and pr (git-appraise-backend-repo-p pr))
      (user-error "No git-appraise review is open"))
    (let* ((review (git-appraise-backend--read-review
                    (oref pr owner) (oref pr number)))
           (request (a-get review 'request))
           (actor (git-appraise-backend--git
                   (oref pr owner) "config" "user.email")))
      (unless (equal actor (a-get request 'requester))
        (user-error "Only the request author can edit review metadata"))
      (when (or (eq (a-get review 'submitted) t)
                (not (stringp (a-get request 'targetRef)))
                (string-empty-p (a-get request 'targetRef)))
        (user-error "Only an open review request can be edited"))
      (cons pr request))))

(defun git-appraise-request-edit--refresh (pr)
  "Refresh PR after a request change."
  (when-let* ((buffer (get-buffer (git-appraise-pr-buffer-name pr))))
    (with-current-buffer buffer
      (git-appraise-reload))))

;;;###autoload
(defun git-appraise-edit-review-description ()
  "Edit the current review description in the comment editor."
  (interactive)
  (pcase-let* ((`(,pr . ,request) (git-appraise-request-edit--context))
               (description (or (a-get request 'description) "")))
    (with-current-buffer (git-appraise-pr-buffer-name pr)
      (let ((git-appraise-comment-description? t))
        (git-appraise-comment-add description)))))

;;;###autoload
(defun git-appraise-edit-reviewers (reviewers)
  "Replace the current review's REVIEWERS, a comma-separated string."
  (interactive
   (pcase-let* ((`(,_ . ,request) (git-appraise-request-edit--context)))
     (list (read-string
            "Reviewers (comma-separated, empty for none): "
            (string-join (a-get request 'reviewers) ", ")))))
  (pcase-let* ((`(,pr . ,request) (git-appraise-request-edit--context))
               (parsed (delete-dups
                        (--filter (not (string-empty-p it))
                                  (--map (string-trim it)
                                         (split-string reviewers "," t))))))
    (unless (equal parsed (a-get request 'reviewers))
      (git-appraise-backend-update-request-fields
       pr (list :reviewers parsed))
      (git-appraise-request-edit--refresh pr))))

;;;###autoload
(defun git-appraise-retarget-review (branch)
  "Set the current review's target to local BRANCH."
  (interactive
   (pcase-let* ((`(,pr . ,request) (git-appraise-request-edit--context))
                (branches (split-string
                           (git-appraise-backend--git
                            (oref pr owner) "for-each-ref"
                            "--format=%(refname:short)" "refs/heads")
                           "\n" t))
                (current (string-remove-prefix
                          "refs/heads/" (or (a-get request 'targetRef) ""))))
     (list (completing-read "New target branch: " branches nil t
                            nil nil current))))
  (pcase-let* ((`(,pr . ,request) (git-appraise-request-edit--context))
               (target (if (string-prefix-p "refs/heads/" branch)
                           branch (concat "refs/heads/" branch))))
    (unless (equal target (a-get request 'targetRef))
      (git-appraise-backend-update-request-fields
       pr (list :target target))
      (git-appraise-request-edit--refresh pr))))

(transient-define-prefix git-appraise-edit-request ()
  "Edit review request metadata on its existing anchor."
  [["Request"
    ("d" "Description" git-appraise-edit-review-description)
    ("r" "Reviewers" git-appraise-edit-reviewers)
    ("t" "Target branch" git-appraise-retarget-review)]])

(provide 'git-appraise-request-edit)
;;; git-appraise-request-edit.el ends here
