;;; git-appraise-section-header.el --- the PR header section inserters -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski
;;
;; Maintainer: Andrea <andrea-dev@hotmail.com>
;; Keywords: git, tools, vc
;; Homepage: https://github.com/corpix/git-appraise.el
;;
;; Split from git-appraise-section.el (phase 15c):
;; git-appraise-section.el, moved verbatim by the split; the fork and
;; this file are maintained by Andrea.

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; the PR header section inserters (part of the git-appraise section rendering, split from
;; git-appraise-section.el in phase 15c).

;;; Code:

(require 'git-appraise-section-shared)
(require 'git-appraise-time)
(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'dash)

;; the budget estimate line (phase 25b); valueless defvar on purpose
(declare-function git-appraise-diff--stats "git-appraise-diff")
(declare-function git-appraise-diff--stats-string "git-appraise-diff")
(declare-function git-appraise-diff--pr-stats-summary "git-appraise-diff")
(declare-function git-appraise-db--pullreq-raw-diff "git-appraise-db")
(declare-function git-appraise-utils--log "git-appraise-utils")
(require 'a)
(require 'git-appraise-faces)
(require 'git-appraise-db)
(require 'git-appraise-utils)




(defclass git-appraise-url-section (magit-section)
  ((keymap :initform 'git-appraise-url-section-map)
   (url :initarg :url)))
(defvar git-appraise-url-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'browse-url)
    (define-key map [mouse-2] 'browse-url)
    (define-key map [follow-link] 'browse-url)
    map)
  "Keymaps for header url section.")
(defclass git-appraise-author-section (magit-section)
  ((keymap :initform 'git-appraise-author-section-map)
   (login :initarg :login)
   (url :initarg :url)))
(defvar git-appraise-author-section-map
  (make-sparse-keymap)
  "Keymaps for header author section.")
(defun git-appraise-section--insert-field (label value &rest props)
  "Insert a header LABEL and propertized VALUE, then a newline."
  (insert (format "%-17s" label))
  (insert (apply #'propertize value props))
  (insert ?\n))

(defun git-appraise-section-insert-url ()
  "Insert the author of the PR in the buffer."
  (let ((url (ignore-errors (oref (git-appraise-db-get-pullreq) url))))
    (when url
      (let ((obj (git-appraise-url-section
                  :url url)))
        (magit-insert-section (git-appraise-author-section obj)
          (git-appraise-section--insert-field
           "Url: " (format "%s" url)
           'face 'git-appraise-url-header-face
           'mouse-face 'git-appraise-hover-face
           'help-echo "Visit PR url"
           'keymap 'git-appraise-url-section-map))))))
(defclass git-appraise-files-summary-section (magit-section)
  ())
(defun git-appraise-section-insert-files-summary ()
  "Insert the one-line change summary right under the PR url.
The same numbers the \"Files changed\" heading reports, but at
the top of the buffer: the main signal for whether this PR can be
reviewed quickly (the phase 25 review-budget view).  Numbers come
from review infos when available, otherwise from the raw diff text
(including the \"excluding docs\" split)."
  (let ((summary (git-appraise-diff--pr-stats-summary
                  (a-get (git-appraise-db--pullreq-raw-infos) 'files)
                  (ignore-errors (git-appraise-db--pullreq-raw-diff)))))
    (when summary
      (magit-insert-section (git-appraise-files-summary-section)
        (git-appraise-section--insert-field
         "Changes: " summary
         'font-lock-face 'git-appraise-url-header-face)))))
(defun git-appraise-section-insert-author ()
  "Insert the author of the PR in the buffer."
  (let-alist (git-appraise-db--pullreq-raw-infos)
    (when .author.login
      (let ((obj (git-appraise-author-section
                  :login .author.login
                  :url .author.url)))
        (magit-insert-section (git-appraise-author-section obj)
          (git-appraise-section--insert-field
           "Author: " .author.login
           'face 'git-appraise-author-header-face
           'mouse-face 'git-appraise-hover-face
           'help-echo "Visit author's page"
           'keymap 'git-appraise-author-section-map))))))
(defclass git-appraise-title-section (magit-section)
  ((keymap  :initform 'git-appraise-title-section-map)
   (title  :initform nil
           :type (or null string))))
(defvar git-appraise-title-section-map
  (make-sparse-keymap)
  "Keymaps for code-comment sections.")
(defun git-appraise-section-insert-header-title ()
  "Insert the title header line."
  (let ((pr (git-appraise-db-get-pullreq)))
    (setq header-line-format
          (propertize
           (format "#%s: %s" (oref pr number) (oref pr title))
           'font-lock-face
           'magit-section-heading))))
(defun git-appraise-section-insert-title ()
  "Insert the title of the header buffer."
  (when-let (title (git-appraise-db--pullreq-title))
    (magit-insert-section (git-appraise-title-section title)
      (insert (format "%-17s" "Title: ") title)
      (insert ?\n))))
(defclass git-appraise-state-section (magit-section)
  ((state  :initform nil
           :type (or null string))))
(defun git-appraise-section-insert-state ()
  "Insert the state of the header buffer."
  (when-let (state (git-appraise-db--pullreq-state))
    (let ((value (if state state "none")))
      (magit-insert-section (git-appraise-state-section value)
        (insert (format "%-17s" "State: ") value)
        (insert ?\n)))))
(defclass git-appraise-ref-section (magit-section)
  ((base   :initarg :base
           :type (or null string))
   (head   :initarg :head
           :type (or null string))))
(defun git-appraise-section-insert-ref ()
  "Insert the state of the header buffer."
  (let* ((pr (git-appraise-db-get-pullreq))
         (obj (git-appraise-ref-section
               :base (oref pr base-ref-name)
               :head (oref pr head-ref-name))))
    (magit-insert-section (git-appraise-ref-section obj)
      (insert (format "%-17s" "Refs: "))
      (insert (oref pr base-ref-name))
      (insert (propertize " ... " 'font-lock-face 'magit-dimmed))
      (insert (oref pr head-ref-name))
      (insert ?\n))))
(defclass git-appraise-milestone-section (magit-section)
  ((keymap :initform 'git-appraise-milestone-section-map)
   (title  :initarg :title)
   (perc   :initarg :perc)
   (number :initarg :number
           :type number)))
(defvar git-appraise-milestone-section-map
  (make-sparse-keymap)
  "Keymaps for milestone section.")
(defun git-appraise-section-insert-milestone ()
  "Insert the milestone of the header buffer."
  (let ((milestones (git-appraise-db--pullreq-milestones)))
    (let-alist milestones
      (let* ((title (when (not (string-empty-p .title)) .title))
             (obj (git-appraise-milestone-section :title title :perc .perc)))
        (magit-insert-section (git-appraise-milestone-section obj)
          (insert (format "%-17s" "Milestone: "))
          (insert (propertize (git-appraise-pretty-milestone obj) 'font-lock-face 'magit-dimmed))
          (insert ?\n))))))
(defclass git-appraise-labels-section (magit-section)
  ((keymap :initform 'git-appraise-labels-section-map)
   (labels :initarg :labels)))
(defvar git-appraise-labels-section-map
  (make-sparse-keymap)
  "Keymaps for code-comment sections.")
(defun git-appraise-section-insert-labels ()
  "Insert the labels of the header buffer."
  (let* ((infos (git-appraise-db--pullreq-raw-infos))
         (labels (git-appraise--distinct-labels
                  (append (git-appraise-db--pullreq-labels)
                          (a-get-in infos (list 'labels 'nodes)))))
         (obj (git-appraise-labels-section :labels labels)))
    (magit-insert-section (git-appraise-labels-section obj)
      (insert (format "%-17s" "Labels: "))
      (if labels
          (dolist (label labels)
            (insert (a-get label 'name))
            (let* ((raw-color (a-get label 'color))
                   (color (if (string-prefix-p "#" raw-color)
                              raw-color
                            (concat "#" raw-color)))
                   (background (git-appraise-utils--sanitize-color color))
                   (foreground (git-appraise-utils--contrast-color color))
                   (o (make-overlay (- (point) (length (a-get label 'name))) (point))))
              (overlay-put o 'priority 2)
              (overlay-put o 'evaporate t)
              (overlay-put o 'font-lock-face
                           `((:background ,background)
                             (:foreground ,foreground)
                             git-appraise-topic-label-face)))
            (insert " "))
        (git-appraise-section--insert-dimmed "None yet"))
      (insert ?\n))))
(defclass git-appraise-assignees-section (magit-section)
  ((keymap :initform 'git-appraise-assignees-section-map)
   (assignees :initarg :assignees)))
(defvar git-appraise-assignees-section-map
  (make-sparse-keymap)
  "Keymaps for code-comment sections.")
(defclass git-appraise-assignee-section (magit-section)
  ((keymap :initform 'git-appraise-assignee-section-map)
   (name :initarg :name)
   (url :initarg :url)))
(defvar git-appraise-assignee-section-map
  (make-sparse-keymap)
  "Keymaps for assignee section")
(defun git-appraise-section--insert-assignee-action (label)
  "Insert the LABEL action link (the set-assignee actions)."
  (insert (propertize label
                      'font-lock-face 'git-appraise-dimmed
                      'mouse-face 'git-appraise-hover-face
                      'help-echo "Set new assignee"
                      'keymap 'git-appraise-assignees-section-map)))
(defun git-appraise-section--insert-assignee-line (assignee)
  "Insert one ASSIGNEE (a name/url alist) as its own section."
  (let-alist assignee
    (let ((assignee-obj (git-appraise-assignee-section
                         :name .name
                         :url .url)))
      (magit-insert-section (git-appraise-assignee-section assignee-obj)
        (insert (propertize .name
                            'face 'git-appraise-author-header-face
                            'mouse-face 'git-appraise-hover-face
                            'help-echo "Visit author's page"
                            'keymap 'git-appraise-assignee-section-map))))))
(defun git-appraise-section-insert-assignee ()
  "Insert the assignee of the header buffer."
  (let* ((infos (git-appraise-db--pullreq-assignees))
         (assignee-names (-map
                          (lambda (a)
                            (let ((name
                                   (if (a-get a 'name)
                                       (format "%s (@%s)"
                                               (a-get a 'name)
                                               (a-get a 'login))
                                     (format "@%s" (a-get a 'login)))))
                              `((name . ,name)
                                (url . ,(a-get a 'url)))))
                          infos)))
    (magit-insert-section (git-appraise-assignees-section)
      (insert (format "%-17s" "Assignees: "))
      (if (not assignee-names)
          (git-appraise-section--insert-assignee-action
           "No one — Assign yourself")
        (progn
          (git-appraise-section--insert-assignee-action "Set new assignee")
          (insert ?\n)
          (dolist (assignee assignee-names)
            (git-appraise-section--insert-assignee-line assignee))
          (insert ?\n)))
      (insert ?\n))))
(defclass git-appraise-project-section (magit-section)
  ((name :initarg :name)))
(defun git-appraise-section-insert-project ()
  "Insert the project of the header buffer."
  (let-alist (git-appraise-db--pullreq-raw-infos)
    (let* ((project-names (-map
                           (lambda (p)
                             (a-get-in p (list 'project 'name)))
                           .projectCards.nodes))
           (projects (if project-names
                         (string-join project-names ", ")
                       (git-appraise-section--dimmed "None yet"))))
      (magit-insert-section (git-appraise-project-section projects)
        (insert (format "%-17s" "Projects: ") projects)
        (insert ?\n)))))
(defclass git-appraise-is-draft-section (magit-section)
  ((draft? :initform nil
           :type (or null string))))
(defun git-appraise-section-insert-is-draft ()
  "Insert the isDraft value of the header buffer."
  (let-alist (git-appraise-db--pullreq-raw-infos)
    (let* ((draft? (if .isDraft "true" "false")))
      (magit-insert-section (git-appraise-is-draft-section draft?)
        (insert (format "%-17s" "Draft: ") draft?)
        (insert ?\n)))))
(defclass git-appraise-reviewers-section (magit-section)
  (()))
(defclass git-appraise-reviewer-section (magit-section)
  ((keymap :initform 'git-appraise-reviewer-section-map)
   (login :initarg :login)
   (url :initarg :url)))
(defvar git-appraise-reviewer-section-map
  (make-sparse-keymap)
  "Keymaps for reviewer section.")
(defun git-appraise-section-insert-reviewers ()
  "Insert the reviewers section."
  (let* ((infos (git-appraise-db--pullreq-raw-infos))
         (groups (git-appraise-utils--fmt-reviewers infos)))
    (magit-insert-section (git-appraise-reviewers-section)
      (insert "Reviewers:\n")
      (maphash (lambda (status users-objs)
                 (dolist (user-o users-objs)
                   (let-alist user-o
                     (let ((obj (git-appraise-reviewer-section
                                 :login .login
                                 :url .url)))
                       (magit-insert-section (git-appraise-reviewer-section obj)
                         (insert (git-appraise--propertize-keyword status))
                         (insert " - ")
                         (insert (propertize (concat "@" .login)
                                             'face 'git-appraise-author-face
                                             'mouse-face 'git-appraise-hover-face
                                             'help-echo "Visit user profile"
                                             'keymap 'git-appraise-reviewer-section-map))
                         (when .code-owner?
                           (insert " as CODE OWNER"))
                         (when .at
                           (insert " " (propertize (git-appraise-utils--format-timestamp .at) 'face 'git-appraise-timestamp-face))))
                       (insert ?\n)))))
               groups))))

;; headers hook definition

(defun git-appraise-section-insert-headers ()
  "Insert all the headers."
  (magit-insert-headers 'git-appraise-headers-hook))

;; commits

(defclass git-appraise-commits-header-section (magit-section)
  (()))
(defclass git-appraise-commit-section (magit-section)
  ((keymap :initform 'git-appraise-commit-section-map)
   (sha    :initarg :sha)
   (oid    :initarg :oid :initform nil)
   (msg    :initarg :msg)
   (date   :initarg :date)))
(defun git-appraise-section-header--format-commit-date (iso)
  "Format the ISO8601 commit date ISO as a compact local timestamp."
  (git-appraise-time-format-iso iso "%Y-%m-%d %H:%M"))
(defvar git-appraise-commit-section-map
  (make-sparse-keymap)
  "Keymaps for commit section.")
(defun git-appraise-section-visit-commit ()
  "Show the commit at point in a Magit revision buffer."
  (interactive)
  (let* ((section (magit-current-section))
         (commit (and (cl-typep section 'git-appraise-commit-section)
                      (oref section value)))
         (revision (and commit (or (oref commit oid) (oref commit sha))))
         (pr (git-appraise-db-get-pullreq)))
    (unless revision
      (user-error "No commit at point"))
    (let ((default-directory (file-name-as-directory (oref pr owner))))
      (magit-show-commit revision))))

(defvar git-appraise-commit-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'git-appraise-visit-commit-rev-at-point)
    (define-key map [mouse-1] #'git-appraise-visit-commit-rev-at-point)
    (define-key map [mouse-2] #'git-appraise-visit-commit-rev-at-point)
    map)
  "Keymap for inline commit hash links.")

(defun git-appraise-visit-commit-rev-at-point ()
  "Show the commit revision stored at point."
  (interactive)
  (let ((rev (get-text-property (point) 'git-appraise-commit-rev))
        (pr (git-appraise-db-get-pullreq)))
    (unless rev
      (user-error "No commit at point"))
    (let ((default-directory (file-name-as-directory (oref pr owner))))
      (magit-show-commit rev))))

(defun git-appraise-section--propertize-commit-link (rev display)
  "Return DISPLAY propertized as a link to commit REV."
  (propertize display
              'font-lock-face 'magit-hash
              'mouse-face 'highlight
              'help-echo (format "Visit commit %s" rev)
              'keymap git-appraise-commit-link-map
              'git-appraise-commit-rev rev))
(defclass git-appraise-commit-check-detail-section (magit-section)
  ((keymap :initform 'git-appraise-commit-check-detail-section-map)
   (details :initarg :details)
   (check   :initarg :check)))
(defclass git-appraise-commit-checks-section (magit-section)
  ()
  "Groups the CI check details of one commit behind one heading.")
(defvar git-appraise-commit-check-detail-section-map
  (make-sparse-keymap)
  "Keymaps for commit check section.")
(defun git-appraise-section--insert-commit-check-detail (check)
  "Insert one CI CHECK line: check name, outcome detail, details link."
  (let-alist check
    (let ((obj (git-appraise-commit-check-detail-section
                :check check :details (or .detailsUrl .targetUrl)))
          (success? (string-equal .conclusion "SUCCESS"))
          (check-name
           (if-let ((check-suite-name
                     (or .checkSuite.workflowRun.workflow.name
                         .checkSuite.app.name)))
               (format "%s / %s" check-suite-name .name)
             ;; for StatusContext actions
             .context)))
      (magit-insert-section (git-appraise-commit-check-detail-section obj)
        (insert (propertize (format "%-7s %s" "" check-name)
                            'font-lock-face 'git-appraise-checker-name-face))
        (insert " - ")
        (if success?
            (when .startedAt
              (insert (propertize
                       (format "%s  "
                               (format "Successful in %s."
                                       (git-appraise-utils--elapsed-time
                                        .completedAt .startedAt)))
                       'font-lock-face 'magit-dimmed)))
          (insert (propertize (format "%s  " (or .summary .description))
                              'font-lock-face 'magit-dimmed)))
        (insert (propertize (if success?
                                ":white_check_mark: Details"
                              ":x: Details")
                            'font-lock-face 'git-appraise-checker-detail-face
                            'mouse-face 'git-appraise-hover-face
                            'help-echo "Visit the page for details"
                            'keymap 'git-appraise-commit-check-detail-section-map))
        (insert "\n")))))
(defun git-appraise-section--insert-commit-checks (contexts)
  "Insert the CI Checks section for the commit rollup CONTEXTS."
  (git-appraise-section--hide-if-hidden
   (magit-insert-section (git-appraise-commit-checks-section nil t)
     (insert (propertize
              (format "  CI Checks (%s)" (length contexts))
              'font-lock-face 'git-appraise-checker-name-face))
     (magit-insert-heading)
     (dolist (check contexts)
       (git-appraise-section--insert-commit-check-detail check)))))
(defun git-appraise-section--insert-commit-with-checks (obj state contexts)
  "Insert the commit OBJ heading with its CI checks subtree.
STATE is the rollup state, CONTEXTS the checks list."
  (insert (format "%s%s%s %s "
                  (propertize (format "%-6s " (oref obj sha))
                              'font-lock-face 'magit-hash)
                  (if-let ((date (oref obj date)))
                      (propertize (format "[%s] "
                                          (git-appraise-section-header--format-commit-date date))
                                  'font-lock-face 'magit-dimmed)
                    "")
                  (car (split-string (oref obj msg) "\n"))
                  (if (string-equal state "SUCCESS")
                      ":white_check_mark:"
                    ":x:")))
  (insert (propertize "Expand for Details:"
                      'font-lock-face 'git-appraise-checker-detail-face))
  (magit-insert-heading)
  (when (> (length (split-string (oref obj msg) "\n")) 1)
    (insert (oref obj msg))
    (insert "\n"))
  (git-appraise-section--insert-commit-checks contexts))
(defun git-appraise-section--insert-plain-commit (obj)
  "Insert the commit OBJ (no CI checks): sha, date and message."
  (let ((start (point)))
    (insert (propertize (format "%-6s " (oref obj sha))
                        'font-lock-face 'magit-hash))
    (when-let ((date (oref obj date)))
      (insert (propertize (format "[%s] "
                                  (git-appraise-section-header--format-commit-date date))
                          'font-lock-face 'magit-dimmed)))
    (insert (oref obj msg))
    (magit-insert-heading)
    (add-text-properties start (point)
                         (list 'keymap git-appraise-commit-section-map))))
(defun git-appraise-section--insert-commit (pr c obj)
  "Insert one commit section: the raw alist C rendered as OBJ.
When the commit carries a statusCheckRollup, the section starts
collapsed and carries the CI checks subtree."
  (let-alist c
    (let ((contexts nil))
      (git-appraise-section--hide-if-hidden
       (magit-insert-section (git-appraise-commit-section obj)
                             ;; collapsed by default when expandable
                             contexts
                             (if contexts
                                 (git-appraise-section--insert-commit-with-checks
                                  obj .commit.statusCheckRollup.state contexts)
                               (git-appraise-section--insert-plain-commit obj)))))))
(defun git-appraise-section-insert-commits ()
  "Insert commits from PULL-REQUEST."
  (let ((pr (git-appraise-db-get-pullreq)))
    (let-alist (oref pr raw-infos)
      (git-appraise-section--hide-if-hidden
       (magit-insert-section (git-appraise-commits-header-section
                             nil git-appraise-fold-header-sections)
        (insert (propertize "Commits:" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (dolist (c .commits.nodes)
          (let-alist c
            (let* ((sha (a-get-in c (list 'commit 'abbreviatedOid)))
                   (msg (a-get-in c (list 'commit 'message)))
                   (date (a-get-in c (list 'commit 'committedDate)))
                   (obj (git-appraise-commit-section
                         :sha sha :oid (a-get-in c '(commit oid))
                         :msg msg :date date)))
              (git-appraise-section--insert-commit pr c obj))))
        (insert ?\n))))))

;; description

(defclass git-appraise-description-section (magit-section)
  ((keymap :initform 'git-appraise-description-section-map)
   (id     :initarg :id)
   (msg    :initarg :msg)
   (reactions :initarg :reactions)))
(defvar git-appraise-description-section-map
  (make-sparse-keymap)
  "Keymaps for description section.")
(defun git-appraise-section-insert-pr-description ()
  "Insert PULL-REQUEST description."
  (when-let (infos (git-appraise-db--pullreq-raw-infos))
    (let-alist infos
      (let* ((is-html? (when .bodyHTML t))
             (is-empty? (and (string-empty-p .bodyHTML)
                             (string-empty-p .bodyText)))
             (description-cleaned (if is-empty?
                                      "No description provided."
                                    (or .bodyHTML .bodyText)))
             (reaction-objs (-map
                             (lambda (r)
                               (git-appraise-reaction-section
                                :id (a-get r 'id)
                                :content (a-get r 'content)))
                             .reactions.nodes))
             (obj (git-appraise-description-section :msg description-cleaned
                                                   :id .databaseId
                                                   :reactions reaction-objs)))
        (git-appraise-section--hide-if-hidden
         (magit-insert-section (git-appraise-description-section obj
                                                                git-appraise-fold-header-sections)
          (insert (propertize "Description" 'font-lock-face 'magit-section-heading))
          (magit-insert-heading)
          (insert ?\n)
          (magit-insert-section (git-appraise-description-section obj)
            (if is-empty?
                (insert (propertize description-cleaned 'font-lock-face 'magit-dimmed))
              (if is-html?
                  (git-appraise--insert-html description-cleaned (* 2 git-appraise-section-indent-width))
                (insert description-cleaned)))
            (insert ?\n)
            (insert ?\n))))))))

;; feedback

(defclass git-appraise-feedback-section (magit-section)
  ((keymap :initform 'git-appraise-feedback-section-map)
   (msg    :initarg :msg)))
(defvar git-appraise-feedback-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'git-appraise-set-feedback)
    (define-key map (kbd "C-c C-k") 'git-appraise-delete-feedback)
    map)
  "Keymaps for feedback section.")
(defun git-appraise-section-insert-feedback-heading ()
  "Insert feedback heading.
Local diff reviews are read-only: no feedback section."
  (when (not (git-appraise-db-local-pr-p))
    (let* ((feedback (git-appraise-db--pullreq-feedback))
           (obj (git-appraise-feedback-section :msg feedback)))
      (git-appraise-section--hide-if-hidden
       (magit-insert-section (git-appraise-feedback-section obj
                                                            git-appraise-fold-header-sections)
         (insert (propertize "Your Review Feedback" 'font-lock-face 'magit-section-heading))
         (magit-insert-heading)
        (magit-insert-section (git-appraise-feedback-section obj)
          (if feedback
              (insert feedback)
            (insert (propertize "Leave a comment here." 'font-lock-face 'magit-dimmed))))
        (insert ?\n)
        (insert ?\n))))))
(cl-defmethod git-appraise-pretty-milestone ((obj git-appraise-milestone-section))
  "Get the pretty version of milestone for a given OBJ."
  (cond
   ((and (oref obj title) (oref obj perc))
    (format "%s (%0.2f%%)"
            (oref obj title)
            (oref obj perc)))
   ((oref obj title)
    (oref obj title))
   (t
    "No milestone")))
;; `defvar' does not replace maps in a running Emacs after `load'.
(--each '(git-appraise-author-section-map
          git-appraise-title-section-map
          git-appraise-milestone-section-map
          git-appraise-labels-section-map
          git-appraise-assignees-section-map
          git-appraise-assignee-section-map
          git-appraise-reviewer-section-map
          git-appraise-commit-section-map
          git-appraise-commit-check-detail-section-map
          git-appraise-description-section-map)
  (set it (make-sparse-keymap)))
(define-key git-appraise-commit-section-map (kbd "RET")
  #'git-appraise-section-visit-commit)

(provide 'git-appraise-section-header)
;;; git-appraise-section-header ends here
