;;; git-appraise-time.el --- Time parsing and formatting -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared time helpers.  Display formatters return local time; machine
;; timestamps return UTC with a trailing Z.  "Now" helpers use UTC.

;;; Code:

(require 'time-date)

(defun git-appraise-time-parse-unix (stamp)
  "Parse git-appraise unix STAMP to an Emacs time, or nil.
STAMP may be a string or number.  Nil, empty, non-numeric, and 0
are treated as missing."
  (let ((n (cond
            ((numberp stamp) stamp)
            ((and (stringp stamp)
                  (string-match-p "\\`[0-9]+\\'" stamp))
             (string-to-number stamp))
            (t nil))))
    (when (and n (> n 0))
      (seconds-to-time n))))

(defun git-appraise-time-format-unix (stamp format &optional utc default)
  "Format unix STAMP with FORMAT, or DEFAULT when STAMP is missing.
UTC non-nil formats in UTC.  DEFAULT defaults to \"-\"."
  (if-let* ((time (git-appraise-time-parse-unix stamp)))
      (format-time-string format time utc)
    (or default "-")))

(defun git-appraise-time-format-iso (str format &optional utc default)
  "Format ISO date STR with FORMAT, or DEFAULT when STR is missing.
UTC non-nil formats in UTC.  DEFAULT defaults to nil."
  (if (and (stringp str) (not (string-empty-p str)))
      (condition-case nil
          (format-time-string format (date-to-time str) utc)
        (error (or default nil)))
    (or default nil)))

(defun git-appraise-time-now-utc (&optional microseconds)
  "Return current UTC time as an ISO string.
With MICROSECONDS non-nil, include six fractional digits for
last-write-wins ordering within the same second."
  (format-time-string (if microseconds "%Y-%m-%dT%T.%6NZ" "%FT%TZ") nil t))

(provide 'git-appraise-time)
;;; git-appraise-time.el ends here
