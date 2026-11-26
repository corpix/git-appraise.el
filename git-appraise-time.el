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

(defconst git-appraise-time--iso-regexp
  (concat "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
          "T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
          "\\(?:\\.\\([0-9]+\\)\\)?Z\\'")
  "A UTC ISO-8601 stamp with an optional fractional part.")

(defun git-appraise-time-parse-utc (stamp)
  "Parse UTC ISO STAMP into integer microseconds since the epoch.
Accepts an optional fractional part of any length; nil, garbage
and unparseable values return nil."
  (when (and (stringp stamp)
             (string-match git-appraise-time--iso-regexp stamp))
    (let* ((frac (or (match-string 7 stamp) ""))
           ;; microseconds: truncate or pad the fraction to 6 digits
           (frac-us (cond
                     ((string-empty-p frac) 0)
                     ((>= (length frac) 6)
                      (string-to-number (substring frac 0 6)))
                     (t (* (string-to-number frac)
                           (expt 10 (- 6 (length frac)))))))
           (time (encode-time (string-to-number (match-string 6 stamp))
                              (string-to-number (match-string 5 stamp))
                              (string-to-number (match-string 4 stamp))
                              (string-to-number (match-string 3 stamp))
                              (string-to-number (match-string 2 stamp))
                              (string-to-number (match-string 1 stamp))
                              0)))
      (pcase-let* ((`(,hi ,lo . ,frac-time) time)
                   (us (or (car frac-time) 0)))
        (+ (* (+ (* hi 65536) lo) 1000000) us frac-us)))))

(defun git-appraise-time-from-epoch-us (micros)
  "Format integer MICROSECONDS since the epoch as a canonical UTC
ISO string with six fractional digits, the form the review store
compares with string ordering."
  (let* ((seconds (/ micros 1000000))
         (frac (% micros 1000000))
         (decoded (decode-time seconds t)))
    (format "%04d-%02d-%02dT%02d:%02d:%02d.%06dZ"
            (nth 5 decoded) (nth 4 decoded) (nth 3 decoded)
            (nth 2 decoded) (nth 1 decoded) (nth 0 decoded)
            frac)))

(provide 'git-appraise-time)
;;; git-appraise-time.el ends here
