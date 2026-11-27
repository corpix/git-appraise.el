;;; git-appraise-gitattributes-test.el --- Tests for gitattributes exclusion -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for excluding generated/vendored files via git attributes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-appraise-diff)

(defconst git-appraise-gitattributes-test--diff
  (concat
   "diff --git a/src/main.go b/src/main.go\n"
   "index 1111111..2222222 100644\n"
   "--- a/src/main.go\n"
   "+++ b/src/main.go\n"
   "@@ -1 +1,2 @@\n"
   " package main\n"
   "+func main() {}\n"
   "diff --git a/vendor/lib.go b/vendor/lib.go\n"
   "index 3333333..4444444 100644\n"
   "--- a/vendor/lib.go\n"
   "+++ b/vendor/lib.go\n"
   "@@ -1 +1,2 @@\n"
   " package lib\n"
   "+// generated\n"
   "diff --git a/api.pb.go b/api.pb.go\n"
   "index 5555555..6666666 100644\n"
   "--- a/api.pb.go\n"
   "+++ b/api.pb.go\n"
   "@@ -1 +1,2 @@\n"
   " package api\n"
   "+// pb\n"))

(defun git-appraise-gitattributes-test--with-repo (fn)
  "Run FN in a temp git repo with .gitattributes."
  (let ((dir (make-temp-file "git-appraise-gitattributes-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (process-file "git" nil nil nil "init" "-q")
          (with-temp-file ".gitattributes"
            (insert "*.pb.go linguist-generated=true\n")
            (insert "vendor/** linguist-vendored=true\n"))
          (funcall fn dir))
      (delete-directory dir t))))

;;; req-2026-11-23-015: gitattributes generated files classified
;; covers: git-appraise-diff.el
;; covers-fn: git-appraise--diff--classify-diff, git-appraise-diff--gitattributes-generated-paths
;; enforcement: test
(ert-deftest git-appraise-gitattributes/req-2026-11-23-015-classify-generated-files ()
  "WHEN a diff contains files marked generated or vendored by git attributes
THE SYSTEM SHALL classify them as generated and collapse them."
  (git-appraise-gitattributes-test--with-repo
   (lambda (dir)
     (let ((default-directory (file-name-as-directory dir))
           (git-appraise-exclude-gitattributes-generated t))
       (let* ((table (git-appraise--diff--classify-diff git-appraise-gitattributes-test--diff))
              (main (gethash "src/main.go" table))
              (vendor (gethash "vendor/lib.go" table))
              (pb (gethash "api.pb.go" table)))
         (should-not (plist-get main :gitattributes-generated))
         (should (plist-get vendor :gitattributes-generated))
         (should (plist-get pb :gitattributes-generated))
         (should (equal "GEN" (plist-get vendor :tag)))
         (should (equal "GEN" (plist-get pb :tag)))
         (should (plist-get vendor :collapse))
         (should (plist-get pb :collapse)))))))

;;; req-2026-11-23-016: gitattributes exclusion hides files from diff
;; covers: git-appraise-diff.el
;; covers-fn: git-appraise--maybe-reorder-diff
;; enforcement: test
(ert-deftest git-appraise-gitattributes/req-2026-11-23-016-exclude-generated-from-diff ()
  "WHEN gitattributes exclusion is enabled
THE SYSTEM SHALL hide generated and vendored files from the reordered diff."
  (git-appraise-gitattributes-test--with-repo
   (lambda (dir)
     (let ((default-directory (file-name-as-directory dir))
           (git-appraise-exclude-gitattributes-generated t))
       (let* ((table (git-appraise--diff--classify-diff git-appraise-gitattributes-test--diff))
              (reordered (git-appraise--maybe-reorder-diff
                          git-appraise-gitattributes-test--diff table)))
         (should (string-match-p "src/main.go" reordered))
         (should-not (string-match-p "vendor/lib.go" reordered))
         (should-not (string-match-p "api.pb.go" reordered)))))))

;;; req-2026-11-23-017: gitattributes exclusion can be disabled
;; covers: git-appraise-diff.el
;; covers-fn: git-appraise--diff--classify-diff, git-appraise--maybe-reorder-diff
;; enforcement: test
(ert-deftest git-appraise-gitattributes/req-2026-11-23-017-disabled-keeps-generated-files ()
  "WHEN gitattributes exclusion is disabled
THE SYSTEM SHALL keep generated and vendored files in the review diff."
  (git-appraise-gitattributes-test--with-repo
   (lambda (dir)
     (let ((default-directory (file-name-as-directory dir))
           (git-appraise-exclude-gitattributes-generated nil))
       (let* ((table (git-appraise--diff--classify-diff git-appraise-gitattributes-test--diff))
              (reordered (git-appraise--maybe-reorder-diff
                          git-appraise-gitattributes-test--diff table)))
         (should-not (plist-get (gethash "vendor/lib.go" table) :gitattributes-generated))
         (should (string-match-p "vendor/lib.go" reordered))
         (should (string-match-p "api.pb.go" reordered)))))))

;;; git-appraise-gitattributes-test.el ends here
