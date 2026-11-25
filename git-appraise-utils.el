;;; git-appraise-utils.el --- General helpers -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski

;;; Commentary:
;;  General helper functions.

;;; Code:

(require 'a)
(require 'dash)
(require 'git-appraise-time)
(require 'magit-git)
(require 'subr-x)

(defcustom git-appraise-download-dir "/tmp/git-appraise/"
  "Directory where git-appraise will download binary files."
  :type 'string
  :group 'git-appraise)

(defcustom git-appraise-log-file (expand-file-name
                                 "git-appraise-error.log"
                                 user-emacs-directory)
  "Path to write append only log errors."
  :group 'git-appraise
  :type 'file)

;;; Repository helpers

(defvar git-appraise-repo-worktree)
(defvar git-appraise-stage-timing?)

(defun git-appraise-utils--timed (stage action)
  "Run ACTION and log elapsed time for STAGE when timing is enabled."
  (let ((started (and (bound-and-true-p git-appraise-stage-timing?)
                      (float-time))))
    (unwind-protect
        (funcall action)
      (when started
        (ignore-errors
          (git-appraise-utils--log
           "stage-timing"
           (format "%s: %.3fs" stage (- (float-time) started))))))))

(defun git-appraise-utils--repo-dir ()
  "Return the review worktree or the Git root of `default-directory'."
  (or (bound-and-true-p git-appraise-repo-worktree)
      (let ((root (and (fboundp 'magit-toplevel) (magit-toplevel))))
        (and root (file-directory-p root) root))))

(defun git-appraise-utils--git (repo &rest args)
  "Run git ARGS in REPO and return stdout, or nil on failure."
  (with-temp-buffer
    (let ((code (apply #'call-process "git" nil (list (current-buffer) nil)
                       nil
                       (append (list "-C" (expand-file-name repo)) args))))
      (and (zerop code)
           (string-trim-right
            (buffer-substring-no-properties (point-min) (point-max)))))))

;;; Comments

(defun git-appraise-utils--comment-key (path pos)
  "Define a key using PATH and POS."
  (format "%s:%s" path pos))

(defun git-appraise-utils--comment-key-from-line (path side line)
  "Define a grouping key using PATH, SIDE and LINE."
  (format "%s:%s:%s" path (or side "RIGHT") (or line 0)))

(defun git-appraise-utils--comment-get (grouped-comments path-pos)
  "Get comments from GROUPED-COMMENTS located by PATH-POS key."
  (alist-get path-pos grouped-comments nil nil 'equal))

(defun git-appraise-utils--comment-update-written-count (count-comments path amount-loc-incr)
  "Update how many comment lines was written for a given PATH."
  (if-let (count (alist-get path count-comments nil nil 'equal))
      (a-assoc count-comments path (+ count amount-loc-incr))
    (a-assoc count-comments path amount-loc-incr)))

(defun git-appraise-utils--comment-clean-msg (msg text-to-remove)
  "Remove TEXT-TO-REMOVE from MSG."
  (let ((msg (or msg ""))
        (text-to-remove (or text-to-remove "")))
    (if (string-empty-p text-to-remove)
        (string-trim msg)
      (string-trim
       (string-replace
        text-to-remove ""
        (string-replace (concat text-to-remove "\n") "" msg))))))

(defun git-appraise-utils--wrap-text (text n)
  "Wrap TEXT at every N column."
  (let ((fill-column n))
    (with-temp-buffer
      (insert text)
      (fill-paragraph)
      (buffer-substring (point-min) (point-max)))))

(defun git-appraise-utils--clean-suggestion (suggestion)
  "Clean SUGGESTION comment."
  (let ((res (-reduce-from
              (lambda (acc line)
                (let ((str (string-trim line)))
                  (cond
                   ((string-empty-p str) acc)
                   ((string-match-p "Suggested change" str)
                    (a-update acc 'block (lambda (v) (cons str v))))
                   (t
                    (if (not (a-get acc 'negative))
                        (-> acc
                            (a-update 'block (lambda (v)
                                               (cons (format "-   %s" str) v)))
                            (a-assoc 'negative t))
                      (-> acc
                          (a-update 'block (lambda (v)
                                             (cons (format "+   %s" str) v)))
                          (a-assoc 'negative nil)))))))
              (a-alist 'negative nil 'block (list))
              (split-string suggestion "\n"))))
    (nreverse (a-get res 'block))))

(defun git-appraise-utils--split-comment (comment)
  "Clean and split the COMMENT in lines."
  (let ((safe-comment (or comment "")))
    (if (string-match-p "Suggested change" safe-comment)
        (git-appraise-utils--clean-suggestion safe-comment)
      (split-string safe-comment "\n"))))

(defun git-appraise-utils--missing-outdated-commments? (path-name path-pos-written grouped-comments)
  "Find missing outdated comments for PATH-NAME."
  (let* ((written-in-path (-filter
                           (lambda (ckey)
                             (string-prefix-p path-name ckey))
                           path-pos-written))
         (expected-in-path (-filter
                            (lambda (ckey)
                              (string-prefix-p path-name ckey))
                            (a-keys grouped-comments))))
    (-difference expected-in-path written-in-path)))

(defun git-appraise-utils-make-group (raw-comments)
  "Group RAW-COMMENTS to ease access when building the buffer."
  (-reduce-from
   (lambda (acc node)
     (let ((author (a-get-in node (list 'author 'login)))
           (state (a-get node 'state)))
       (if-let (comments (a-get-in node (list 'comments 'nodes)))
           (-reduce-from
            (lambda (grouped-comments comment)
              (let-alist comment
                (let* ((handled-pos (or .position .originalPosition))
                       (has-line (or .line .startLine))
                       (path-pos (cond
                                  (.outdated
                                   (git-appraise-utils--comment-key .path handled-pos))
                                  (has-line
                                   (git-appraise-utils--comment-key-from-line .path .side .line))
                                  (handled-pos
                                   (git-appraise-utils--comment-key .path handled-pos))
                                  (t
                                   (git-appraise-utils--comment-key-from-line .path .side .line))))
                       (obj (cond
                             (.reply?
                              (git-appraise-reply-comment-section
                               :state state
                               :author author
                               :msg .bodyText
                               :body .bodyText
                               :position handled-pos
                               :reactions nil
                               :path .path
                               :diffHunk .diffHunk
                               :internalId .internal-id
                               :id .databaseId
                               :createdAt .createdAt
                               :updatedAt .updatedAt))
                             (.outdated
                              (git-appraise-outdated-comment-section
                               :state state
                               :author author
                               :msg .bodyHTML
                               :body .body
                               :position handled-pos
                               :reactions nil
                               :internalId .internal-id
                               :path .path
                               :diffHunk .diffHunk
                               :id .databaseId
                               :thread-id .threadId
                               :resolved? .isResolved
                               :createdAt .createdAt
                               :updatedAt .updatedAt))
                             (.local?
                              (git-appraise-local-comment-section
                               :state state
                               :author author
                               :msg .bodyText
                               :body .bodyText
                               :position handled-pos
                               :reactions nil
                               :internalId .internal-id
                               :path .path
                               :createdAt .createdAt
                               :updatedAt .updatedAt
                               :line-type .line-type
                               :side .side
                               :line .line
                               :send? .send?
                               :start-side .startSide
                               :start-line .startLine))
                             (t
                              (git-appraise-code-comment-section
                               :state state
                               :author author
                               :msg .bodyHTML
                               :body .body
                               :position handled-pos
                               :reactions nil
                               :internalId .internal-id
                               :path .path
                               :diffHunk .diffHunk
                               :id .databaseId
                               :thread-id .threadId
                               :resolved? .isResolved
                               :createdAt .createdAt
                               :updatedAt .updatedAt)))))
                  (when (not .path)
                    (throw :git-appraise/comment-missing-path
                           "Every comment requires a path in the diff."))
                  (when (and (not .bodyHTML) (not .bodyText))
                    (git-appraise-utils--log
                     "git-appraise-comment-make-group"
                     (format "Comment with nil body: %S" (prin1-to-string comment))))
                  (if (or (not grouped-comments)
                          (not (git-appraise-utils--comment-get grouped-comments path-pos)))
                      (a-assoc grouped-comments path-pos (list obj))
                    (a-update grouped-comments path-pos (lambda (v) (append v (list obj))))))))
            acc
            comments)
         acc)))
   nil
   raw-comments))

;;; Git

(defun git-appraise-utils--git-get-user ()
  "Get user from git config."
  (magit-get "user.name"))

;;; Diff

(defun git-appraise-utils--clean-diff-prefixes (raw-diff)
  "Remove all prefixes from RAW-DIFF."
  (let ((res raw-diff))
    (setq res
          (replace-regexp-in-string
           (rx line-start "diff --git a/" (group-n 1 (+? not-newline)) " b/" (backref 1) line-end)
           "diff --git \\1 \\1"
           res))
    (setq res (replace-regexp-in-string
               (rx line-start (group-n 1 (or "+++" "---")) " " (or "a/" "b/") (group-n 2 (+? not-newline)) line-end)
               "\\1 \\2"
               res))
    (replace-regexp-in-string "\\`\\s-+" "" res)))

;;; Date

(defun git-appraise-utils--format-timestamp (str)
  "Convert and format timestamp STR from json."
  (or (git-appraise-time-format-iso str "%b %d, %Y, %H:%M") ""))

(defun git-appraise-utils--elapsed-time (t2 t1)
  "Compute the elapsed time between T2 and T1."
  (let ((res (- (time-to-seconds (date-to-time t2))
                (time-to-seconds (date-to-time t1)))))
    (if (> res 100)
        (format "%smin" (/ res 60))
      (format "%ss" res))))

;;; Line-number cache

(defvar git-appraise--line-number-cache nil)
(make-variable-buffer-local 'git-appraise--line-number-cache)

(defun git-appraise--after-change (&rest _args)
  "Flush cache after change."
  (setq git-appraise--line-number-cache nil))

(defun git-appraise--line-number-at-pos ()
  "Like `line-number-at-pos' but sped up with a cache."
  (let ((pos
         (if (and git-appraise--line-number-cache
                  (> (- (point) (point-min))
                     (abs (- (point) (car git-appraise--line-number-cache)))))
             (funcall (if (> (point) (car git-appraise--line-number-cache))
                          #'+ #'-)
                      (cdr git-appraise--line-number-cache)
                      (count-lines (point) (car git-appraise--line-number-cache)))
           (line-number-at-pos))))
    (setq git-appraise--line-number-cache (cons (point) pos))
    pos))

;;; Log

(defun git-appraise-utils--log (origin msg)
  "Append MSG from ORIGIN to the error log without masking its caller."
  (condition-case err
      (with-temp-buffer
        (insert ?\n (current-time-string) " - " origin " - " msg ?\n)
        (write-region (point-min) (point-max)
                      git-appraise-log-file t 'silent))
    (file-error
     (message "git-appraise log unavailable: %s"
              (error-message-string err)))))

;;; Colors (kept for header rendering)

(defun git-appraise-utils--sanitize-color (color)
  "Sanitize COLOR."
  (cond ((x-color-values color) color)
        ((string-match-p "\\`#.\\{4\\}\\'" color) (substring color 0 3))
        ((string-match-p "\\`#.\\{8\\}\\'" color) (substring color 0 6))
        (t "#000000")))

(defun git-appraise-utils--contrast-color (color)
  "Return black or white depending on luminance of COLOR."
  (if (> (git-appraise-utils--x-color-luminance color) 0.5) "black" "white"))

(defun git-appraise-utils--x-color-luminance (color)
  "Calculate luminance of COLOR."
  (let ((values (x-color-values color)))
    (git-appraise-utils--color-luminance (/ (nth 0 values) 256.0)
                                        (/ (nth 1 values) 256.0)
                                        (/ (nth 2 values) 256.0))))

(defun git-appraise-utils--color-luminance (red green blue)
  "Calculate luminance of RED GREEN BLUE."
  (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 256))

(defun git-appraise-utils--fmt-reviewers (infos)
  "Produce group of reviewers and their statuses from INFOS."
  (let-alist infos
    (let ((groups (make-hash-table :test 'equal)))
      (puthash "PENDING" (mapcar
                          (lambda (r)
                            (let-alist r
                              `((code-owner? . ,.asCodeOwner)
                                (login . ,.requestedReviewer.login)
                                (url . ,.requestedReviewer.url)
                                (at))))
                          (-distinct .reviewRequests.nodes))
               groups)
      (mapc (lambda (o)
              (let* ((state (a-get o 'state))
                     (author (a-get o 'author))
                     (login (a-get author 'login))
                     (current-data (gethash state groups)))
                (when (not (-contains-p (-map
                                         (lambda (it)
                                           (a-get it 'login))
                                         current-data)
                                        login))
                  (push `((code-owner?)
                          (login . ,login)
                          (url . ,(a-get author 'url))
                          (at . ,(a-get o 'createdAt)))
                        (gethash state groups)))))
            .latestOpinionatedReviews.nodes)
      groups)))

(provide 'git-appraise-utils)
;;; git-appraise-utils.el ends here
