;;; git-appraise-diff-presentation-test.el --- Diff label tests -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'git-appraise)

;;; req-2026-11-24-110: rename labels are not repeated
;; covers: git-appraise-diff.el git-appraise-section-wash.el
;; covers-fn: git-appraise--diff--classify-diff
;; enforcement: test
(ert-deftest git-appraise-diff-presentation/req-2026-11-24-110-rename-label ()
  "WHEN a file is labeled renamed THE SYSTEM SHALL avoid repeating that status as a moved tag."
  (let* ((diff (concat "diff --git a/old.txt b/new.txt\n"
                       "similarity index 100%\n"
                       "rename from old.txt\n"
                       "rename to new.txt\n"))
         (info (gethash "new.txt" (git-appraise--diff--classify-diff diff))))
    (should info)
    (should-not (equal (plist-get info :tag) "MOVED"))))

;;; git-appraise-diff-presentation-test.el ends here
