;;; git-appraise-gitattributes-test.el --- Tests for gitattributes exclusion -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for excluding generated/vendored files via git attributes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dash)
(require 'git-appraise-diff)
(require 'git-appraise)
(require 'git-appraise-test-helpers)

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
            (insert "vendor/** linguist-vendored=true\n")
            (insert "docs/** linguist-documentation\n")
            (insert "docs/README.md -linguist-documentation\n")
            (insert "*.js linguist-language=JavaScript\n")
            (insert "*.dat -linguist-detectable\n"))
          (funcall fn dir))
      (delete-directory dir t))))

;;; req-2026-11-23-015: gitattributes generated files classified
;; covers: git-appraise-diff.el
;; covers-fn: git-appraise--diff--classify-diff, git-appraise-file-types--attributes
;; enforcement: test
(ert-deftest git-appraise-gitattributes/req-2026-11-23-015-classify-generated-files ()
  "WHEN a diff contains generated or vendored files THE SYSTEM SHALL identify each type and collapse both."
  (git-appraise-gitattributes-test--with-repo
   (lambda (dir)
     (let ((default-directory (file-name-as-directory dir))
           (git-appraise-exclude-gitattributes-generated t))
       (let* ((table (git-appraise--diff--classify-diff git-appraise-gitattributes-test--diff))
              (main (gethash "src/main.go" table))
              (vendor (gethash "vendor/lib.go" table))
              (pb (gethash "api.pb.go" table)))
         (should-not (plist-get main :linguist-excluded))
         (should (plist-get vendor :linguist-excluded))
         (should (plist-get pb :linguist-excluded))
         (should (equal "VEND" (plist-get vendor :tag)))
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
         (should (plist-get (gethash "vendor/lib.go" table) :linguist-excluded))
         (should (string-match-p "vendor/lib.go" reordered))
         (should (string-match-p "api.pb.go" reordered)))))))

;;; req-2026-11-27-010: explicit Linguist overrides inform review types
;; covers: git-appraise-file-types.el git-appraise-diff.el
;; covers-fn: git-appraise-file-types--attributes, git-appraise--diff--classify-path
;; enforcement: test
(ert-deftest git-appraise-gitattributes/req-2026-11-27-010-five-linguist-attributes ()
  "WHEN a repository sets Linguist attributes THE SYSTEM SHALL honor all five overrides in file classification and details."
  (git-appraise-gitattributes-test--with-repo
   (lambda (dir)
     (let* ((paths '("api.pb.go" "vendor/lib.go" "docs/manual.md"
                     "docs/README.md" "src/a.js" "data file.dat"))
            (attrs (git-appraise-file-types--attributes paths dir)))
       (should (eq 'set (plist-get (gethash "api.pb.go" attrs) :generated)))
       (should (eq 'set (plist-get (gethash "vendor/lib.go" attrs) :vendored)))
       (should (eq 'set (plist-get (gethash "docs/manual.md" attrs)
                                 :documentation)))
       (should (eq 'unset (plist-get (gethash "docs/README.md" attrs)
                                   :documentation)))
       (should (equal "JavaScript"
                      (plist-get (gethash "src/a.js" attrs) :language)))
       (should (eq 'unset (plist-get (gethash "data file.dat" attrs)
                                   :detectable)))
       (should (eq 'documentation
                   (plist-get (git-appraise--diff--classify-path
                               "docs/manual.md"
                               (gethash "docs/manual.md" attrs)) :type)))
       (should-not (plist-get (git-appraise--diff--classify-path
                               "docs/README.md"
                               (gethash "docs/README.md" attrs)) :type))))))

(ert-deftest git-appraise-gitattributes/attribute-reader-is-one-batch ()
  "Two hundred paths are read with one Git attribute process."
  (git-appraise-gitattributes-test--with-repo
   (lambda (dir)
     (let* ((paths (--map (format "file-%03d.js" it)
                          (number-sequence 0 199)))
           (original (symbol-function 'call-process-region))
           (calls 0))
       (unwind-protect
           (progn
             (fset 'call-process-region
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply original args)))
             (let ((attrs (git-appraise-file-types--attributes paths dir)))
               (should (= 200 (hash-table-count attrs)))
               (should (= calls 1))))
         (fset 'call-process-region original))))))

;;; req-2026-11-27-011: explicit language is visible in file details
;; covers: git-appraise-file-types.el git-appraise-section-wash.el
;; covers-fn: git-appraise-file-types--attributes, git-appraise-wash-insert-file-section
;; enforcement: test
(ert-deftest git-appraise-gitattributes/req-2026-11-27-011-language-details ()
  "WHEN a reviewed file has an explicit Linguist language THE SYSTEM SHALL show that language in its expanded file details."
  (git-appraise-test--with-review-repo "git-appraise-language-detail-"
    (let ((git-appraise-history-enabled nil))
      (with-temp-file (expand-file-name ".gitattributes" directory)
        (insert "example.txt linguist-language=JavaScript\n"))
      (git-appraise-test--run directory "git" "add" ".gitattributes")
      (git-appraise-test--run directory "git" "commit" "-qm" "set language")
      (let ((buffer (git-appraise-test--open-and-render revision)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "Language: JavaScript" nil t))
          (should (eq (oref (magit-current-section) type)
                      'git-appraise-file-language)))))))

;;; git-appraise-gitattributes-test.el ends here
