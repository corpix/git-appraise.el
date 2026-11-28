;;; git-appraise-comment-format.el --- Comment headings and dates -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared display formatting for conversation and diff comments.

;;; Code:

(require 'time-date)
(require 'subr-x)
(require 'git-appraise-faces)
(require 'git-appraise-badges)
(require 'dash)

(defcustom git-appraise-activity-indent 2
  "Spaces before activity timestamps in expanded discussion sections."
  :group 'git-appraise :type 'natnum)

(defun git-appraise-comment-format-time (timestamp &optional now)
  "Format TIMESTAMP for a comment relative to local time NOW.
Show only the clock time for today, day and month for another day
this year, and the year when it differs.  TIMESTAMP may be an ISO
date string or an Emacs time value.  Return nil when it is nil."
  (when timestamp
    (let* ((time (if (stringp timestamp)
                     (date-to-time timestamp)
                   timestamp))
           (date (decode-time time))
           (today (decode-time (or now (current-time))))
           (same-year (= (decoded-time-year date)
                         (decoded-time-year today)))
           (same-day (and same-year
                          (= (decoded-time-month date)
                             (decoded-time-month today))
                          (= (decoded-time-day date)
                             (decoded-time-day today))))
           (format-string (cond (same-day "%H:%M")
                                (same-year "%-d %b %H:%M")
                                (t "%-d %b %Y %H:%M"))))
      (format-time-string format-string time))))

(defun git-appraise-comment-format-heading (kind author timestamp &optional statuses)
  "Build a KIND comment heading for AUTHOR at TIMESTAMP.
TIMESTAMP is displayed in the opened section body.  STATUSES is a
list of short labels such as \"DRAFT\" or \"OUTDATED\"."
  (ignore timestamp)
  (let* ((who (or author "unknown"))
         (statuses (--map (if (and (equal kind "Reply")
                                     (equal it "APPROVED"))
                                "RESOLVED" it)
                          (--filter it (if (listp statuses) statuses
                                         (list statuses)))))
         (promoted (and (equal kind "Comment")
                        (--first (member it '("APPROVED" "LGTM"
                                              "CHANGES_REQUESTED"
                                              "NEEDS_WORK" "DRAFT"))
                                 statuses)))
         (primary (or promoted kind))
         (secondary (if promoted (remove promoted statuses) statuses)))
    (concat
     (git-appraise-badge primary) " "
     (propertize who 'face 'git-appraise-author-face)
     (mapconcat (lambda (status)
                  (concat " " (git-appraise-badge status)))
                secondary ""))))

(defun git-appraise-comment-format-time-detail (label timestamp)
  "Return a highlighted LABEL and TIMESTAMP line for an opened section."
  (when-let* ((time (git-appraise-comment-format-time timestamp)))
    (concat (make-string git-appraise-activity-indent ?\s)
            (propertize label 'font-lock-face 'magit-dimmed) "  "
            (propertize time 'font-lock-face 'git-appraise-timestamp-face)
            "\n")))

(defun git-appraise-comment-format-time-details (created updated)
  "Return posted and, when different, edited times for a comment."
  (concat
   (or (git-appraise-comment-format-time-detail "Posted" created) "")
   (if (and updated (not (equal created updated)))
       (or (git-appraise-comment-format-time-detail "Edited" updated) "")
     "")))

(provide 'git-appraise-comment-format)
;;; git-appraise-comment-format.el ends here
