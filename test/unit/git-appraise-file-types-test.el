;;; git-appraise-file-types-test.el --- Configurable file types -*- lexical-binding: t; -*-

(require 'ert)
(require 'git-appraise-diff)
(require 'git-appraise-file-types)

;;; req-2026-11-27-009: file types are user configurable
;; covers: git-appraise-file-types.el git-appraise-diff.el
;; covers-fn: git-appraise-file-types--classify, git-appraise-diff--stats
;; enforcement: test
(ert-deftest file-types/req-2026-11-27-009-custom-type-labels-and-matching ()
  "WHEN a reviewer changes file-type rules or labels THE SYSTEM SHALL classify files accordingly without changing document statistics or focus behavior."
  (let ((git-appraise-file-types (copy-tree git-appraise-file-types)))
    (should (eq 'generated (plist-get
                            (git-appraise-file-types--classify "flake.lock")
                            :type)))
    (should (eq 'vendored (plist-get
                           (git-appraise-file-types--classify "vendor/lib.go")
                           :type)))
    (should (eq 'spec (plist-get
                       (git-appraise-file-types--classify "specs/api.md")
                       :type)))
    (should (eq 'test (plist-get
                       (git-appraise-file-types--classify "tests/app_test.go")
                       :type)))
    (should (eq 'documentation (plist-get
                                (git-appraise-file-types--classify "README.md")
                                :type)))
    (setf (plist-get (cdr (assq 'documentation git-appraise-file-types)) :label)
          "GUIDE")
    (let ((info (git-appraise-file-types--classify "README.md")))
      (should (equal "GUIDE" (plist-get info :tag)))
      (should (eq 'documentation (plist-get info :type)))
      (should (plist-get info :hide))
      (should (plist-get info :collapse)))
    (let* ((diff (concat "diff --git a/README.md b/README.md\n"
                         "--- a/README.md\n+++ b/README.md\n"
                         "@@ -1 +1 @@\n-old\n+new\n"))
           (stats (git-appraise-diff--stats diff)))
      (should (= 1 (plist-get stats :add-doc)))
      (should (= 1 (plist-get stats :del-doc)))
      (let* ((git-appraise-focus-mode t)
             (classes (git-appraise--diff--classify-diff diff))
             (visible (git-appraise--maybe-reorder-diff diff classes)))
        (should-not (string-match-p "README.md" visible))))))

(ert-deftest file-types/explicit-attributes-and-language-match ()
  "Explicit Linguist attributes override paths and preserve unset values."
  (let ((git-appraise-file-types (copy-tree git-appraise-file-types)))
    (should (eq 'documentation
                (plist-get (git-appraise-file-types--classify
                            "specs/api.md" '(:documentation set)) :type)))
    (should (eq 'spec
                (plist-get (git-appraise-file-types--classify
                            "specs/api.md" '(:documentation unset)) :type)))
    (should-not (plist-get (git-appraise-file-types--classify
                            "README.md" '(:documentation unset)) :type))
    (push '(custom :languages ("javascript") :detectable unset
                   :label "JS" :face font-lock-keyword-face
                   :collapse nil :hide nil :order 5)
          git-appraise-file-types)
    (let ((info (git-appraise-file-types--classify
                 "src/odd.txt" '(:language "JavaScript" :detectable unset))))
      (should (eq 'custom (plist-get info :type)))
      (should (equal "JS" (plist-get info :tag)))
      (should (eq 'font-lock-keyword-face (plist-get info :face))))))

(ert-deftest file-types/width-and-sort-band-are-configurable ()
  "Custom labels retain full width and type sort bands order files."
  (let ((git-appraise-file-types (copy-tree git-appraise-file-types)))
    (setf (plist-get (cdr (assq 'documentation git-appraise-file-types)) :label)
          "DOCUMENTATION")
    (should (>= (git-appraise-file-types--tag-width) 13))
    (should (< (git-appraise--diff--file-order-index
                "src/main.go" (git-appraise-file-types--classify "src/main.go"))
               (git-appraise--diff--file-order-index
                "README.md" (git-appraise-file-types--classify "README.md"))))))

;;; git-appraise-file-types-test.el ends here
