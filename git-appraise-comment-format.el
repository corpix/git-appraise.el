;;; git-appraise-comment-format.el --- Comment headings and dates -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared display formatting for conversation and diff comments.

;;; Code:

(require 'time-date)
(require 'git-appraise-faces)

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
KIND is \"Comment\" or \"Reply\".  STATUSES is a list of short
labels such as \"DRAFT\" or \"OUTDATED\"."
  (let* ((who (or author "unknown"))
         (statuses (if (listp statuses) statuses (list statuses)))
         (time (git-appraise-comment-format-time timestamp)))
    (concat
     (propertize (format "%s by " kind) 'face 'magit-section-heading)
     (propertize who 'face 'git-appraise-author-face)
     (mapconcat (lambda (status)
                  (concat " · "
                          (propertize (replace-regexp-in-string "_" " " status)
                                      'face 'git-appraise-state-face)))
                (delq nil statuses) "")
     (when time
       (concat " · " (propertize time 'face 'git-appraise-timestamp-face))))))

(provide 'git-appraise-comment-format)
;;; git-appraise-comment-format.el ends here
