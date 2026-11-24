;;; git-appraise-viewed-test.el --- viewed identity and projection tests -*- lexical-binding: t; -*-

;;; req-2026-11-24-007..012, 2026-11-24-024:
;; covers: git-appraise-viewed--inventory, git-appraise-viewed--project
;; covers-fn: git-appraise-viewed--fingerprint, git-appraise-viewed--range-key
;; enforcement: test

(require 'ert)
(require 'a)
(require 'dash)
(require 'git-appraise-viewed)

(defconst git-appraise-viewed-test--two-hunks
  "diff --git src/a.el src/a.el
index 1b2a1c5..7330311 100644
--- src/a.el
+++ src/a.el
@@ -1,5 +1,5 @@
 l1
-l2
+L2
 l3
 l4
 l5
@@ -7,6 +7,6 @@ l6
 l7
 l8
 l9
-l10
+L10
 l11
 l12
"
  "A file with two separate hunks.")

(defconst git-appraise-viewed-test--shifted
  "diff --git src/a.el src/a.el
index b3990fa..5249752 100644
--- src/a.el
+++ src/a.el
@@ -1,6 +1,6 @@
 zero
 l1
-l2
+L2
 l3
 l4
 l5
@@ -8,6 +8,6 @@ l6
 l7
 l8
 l9
-l10
+L10
 l11
 l12
"
  "The same edits one line lower: hunk 2's content is identical,
only its @@ range numbers moved.")

(defconst git-appraise-viewed-test--identical-dup
  "diff --git src/dup.txt src/dup.txt
index bca3c99..6651b12 100644
--- src/dup.txt
+++ src/dup.txt
@@ -2,7 +2,7 @@ h1
 h2
 h3
 ctx-a
-old
+NEW
 ctx-b
 t1
 t2
@@ -11,7 +11,7 @@ h1
 h2
 h3
 ctx-a
-old
+NEW
 ctx-b
 t1
 t2
"
  "Two hunks with byte-identical bodies at different positions.")

(defconst git-appraise-viewed-test--misc
  "diff --git bin.dat bin.dat
index 8352675..2071727 100644
Binary files bin.dat and bin.dat differ
diff --git gone.txt gone.txt
deleted file mode 100644
index 2fa992c..0000000
--- gone.txt
+++ /dev/null
@@ -1 +0,0 @@
-keep
diff --git old-name.txt new-name.txt
similarity index 100%
rename from old-name.txt
rename to new-name.txt
diff --git script.sh script.sh
old mode 100644
new mode 100755
diff --git tail.txt tail.txt
index e34f96c..4771b5c 100644
--- tail.txt
+++ tail.txt
@@ -1 +1 @@
-no newline v1
\\ No newline at end of file
+no newline v2
\\ No newline at end of file
"
  "Binary, deletion, rename, mode-only and no-newline blocks.")

(defun git-appraise-viewed-test--event (id path fp viewed &optional range at)
  "Build a valid viewed event for PATH and FP."
  `((v . 1)
    (type . "viewed")
    (id . ,id)
    (actor . "reviewer@example.com")
    (item . "hunk")
    (path . ,path)
    (fingerprint . ,fp)
    ,@(when range `((range . ,range)))
    (viewed . ,(if viewed t :json-false))
    (at . ,(or at "2026-01-01T00:00:00Z"))))

(defun git-appraise-viewed-test--entry (inventory path)
  "The inventory entry for PATH."
  (gethash path inventory))

;;; req-2026-11-24-007: shift stability
(ert-deftest viewed/req-2026-11-24-007-shift-stability ()
  "WHEN only line numbers before a hunk change THE SYSTEM SHALL keep the hunk's fingerprint identical."
  (let* ((inv1 (git-appraise-viewed--inventory
                git-appraise-viewed-test--two-hunks))
         (inv2 (git-appraise-viewed--inventory
                git-appraise-viewed-test--shifted))
         (h1 (nth 1 (plist-get (git-appraise-viewed-test--entry inv1 "src/a.el")
                               :hunks)))
         (h2 (nth 1 (plist-get (git-appraise-viewed-test--entry inv2 "src/a.el")
                               :hunks))))
    (should (equal (plist-get h1 :fp) (plist-get h2 :fp)))
    (should (equal (plist-get h1 :range) "-7,6 +7,6"))
    (should (equal (plist-get h2 :range) "-8,6 +8,6"))))

;;; req-2026-11-24-008: content invalidation
;; covers: git-appraise-viewed--fingerprint
;; enforcement: test
(ert-deftest viewed/req-2026-11-24-008-content-invalidation ()
  "WHEN one hunk's content changes THE SYSTEM SHALL give it a different fingerprint while an unchanged hunk keeps its fingerprint."
  (let* ((inv1 (git-appraise-viewed--inventory
                git-appraise-viewed-test--two-hunks))
         (inv2 (git-appraise-viewed--inventory
                git-appraise-viewed-test--shifted))
         (fps1 (-map (lambda (h) (plist-get h :fp))
                     (plist-get (git-appraise-viewed-test--entry inv1 "src/a.el")
                                :hunks)))
         (fps2 (-map (lambda (h) (plist-get h :fp))
                     (plist-get (git-appraise-viewed-test--entry inv2 "src/a.el")
                                :hunks))))
    (should-not (equal (nth 0 fps1) (nth 0 fps2)))
    (should (equal (nth 1 fps1) (nth 1 fps2)))))

;;; req-2026-11-24-009: rename keeps the canonical path
;; covers: git-appraise-viewed--inventory
;; enforcement: test
(ert-deftest viewed/req-2026-11-24-009-rename-canonical-path ()
  "WHEN a pure rename block is inventoried THE SYSTEM SHALL key it under the canonical new path as a hunk-less block."
  (let* ((inv (git-appraise-viewed--inventory
               git-appraise-viewed-test--misc))
         (entry (git-appraise-viewed-test--entry inv "new-name.txt")))
    (should entry)
    (should-not (gethash "old-name.txt" inv))
    (should-not (plist-get entry :has-hunks))
    (should (plist-get entry :file-fp))))

;;; req-2026-11-24-010: hunk-less block identity
;; covers: git-appraise-viewed--file-fingerprint, git-appraise-viewed--parse-ranges
;; enforcement: test
(ert-deftest viewed/req-2026-11-24-010-nohunk-block-identity ()
  "WHEN a hunk-less block changes (mode, binary payload, newline marker) THE SYSTEM SHALL change its identity, and keep the marker lines as content."
  (let* ((inv (git-appraise-viewed--inventory
               git-appraise-viewed-test--misc))
         (script (git-appraise-viewed-test--entry inv "script.sh"))
         (bin (git-appraise-viewed-test--entry inv "bin.dat"))
         ;; a trailing newline appears: the `\\ No newline' marker
         ;; line disappears from the block
         (variant (string-replace
                   "+no newline v2\n\\ No newline at end of file\n"
                   "+no newline v2\n"
                   git-appraise-viewed-test--misc))
         (inv2 (git-appraise-viewed--inventory variant))
         ;; mode-only change on the same file
         (variant2 (string-replace "new mode 100755" "new mode 100600"
                                   git-appraise-viewed-test--misc))
         (inv3 (git-appraise-viewed--inventory variant2)))
    ;; binary and mode-only files are hunk-less blocks with own hashes
    (should (and bin script))
    (should-not (plist-get bin :has-hunks))
    (should-not (plist-get script :has-hunks))
    (should-not (equal (plist-get bin :file-fp)
                       (plist-get script :file-fp)))
    ;; a changed block invalidates its mark
    (let* ((bin-variant (string-replace "index 8352675..2071727 100644"
                                        "index 8352675..9999999 100644"
                                        git-appraise-viewed-test--misc))
           (inv-bin (git-appraise-viewed--inventory bin-variant)))
      (should-not (equal (plist-get bin :file-fp)
                         (plist-get (git-appraise-viewed-test--entry inv-bin
                                               "bin.dat")
                                    :file-fp))))
    ;; an untouched file keeps its identity across other blocks changing
    (should (equal (plist-get bin :file-fp)
                   (plist-get (git-appraise-viewed-test--entry inv2
                                         "bin.dat")
                              :file-fp)))
    (should-not (equal (plist-get script :file-fp)
                       (plist-get (git-appraise-viewed-test--entry inv3
                                             "script.sh")
                                  :file-fp)))
    ;; the no-newline markers are content: removing one flips the hunk
    (should-not (equal (plist-get (nth 0 (plist-get
                                          (git-appraise-viewed-test--entry
                                           inv "tail.txt")
                                          :hunks))
                                  :fp)
                       (plist-get (nth 0 (plist-get
                                          (git-appraise-viewed-test--entry
                                           inv2 "tail.txt")
                                          :hunks))
                                  :fp)))
    ;; missing counts canonicalize: "-1 +1" -> "-1,1 +1,1"
    (should (equal (git-appraise-viewed--range-key
                    (git-appraise-viewed--parse-ranges "-1 +1"))
                   "-1,1 +1,1"))))

;;; req-2026-11-24-011: duplicate hunk ambiguity
;; covers: git-appraise-viewed--project
;; enforcement: test
(ert-deftest viewed/req-2026-11-24-011-duplicate-ambiguity ()
  "WHEN two hunks share a fingerprint THE SYSTEM SHALL transfer a prior mark only when its stored range still matches."
  (let* ((inv (git-appraise-viewed--inventory
               git-appraise-viewed-test--identical-dup))
         (entry (git-appraise-viewed-test--entry inv "src/dup.txt"))
         (fp (plist-get (nth 0 (plist-get entry :hunks)) :fp)))
    (should (= (length (plist-get entry :hunks)) 2))
    (should (equal (plist-get (nth 0 (plist-get entry :hunks)) :range)
                   "-2,7 +2,7"))
    (should (equal (plist-get (nth 1 (plist-get entry :hunks)) :range)
                   "-11,7 +11,7"))
    ;; both hunks are flagged ambiguous
    (should (-all? (lambda (h) (plist-get h :dup)) (plist-get entry :hunks)))
    ;; a mark for the first position only marks the first hunk
    (let ((idx (git-appraise-viewed--project
                (list (git-appraise-viewed-test--event
                       "e1" "src/dup.txt" fp t "-2,7 +2,7"))
                "reviewer@example.com" inv)))
      (should (equal (plist-get (gethash "src/dup.txt" idx) :hunk-viewed)
                     '("-2,7 +2,7")))
      (should (= (plist-get (gethash "src/dup.txt" idx) :viewed-ct) 1))
      (should-not (plist-get (gethash "src/dup.txt" idx) :all-hunks)))
    ;; a rangeless mark is ambiguous: neither hunk may claim it
    (let ((idx (git-appraise-viewed--project
                (list (git-appraise-viewed-test--event
                       "e2" "src/dup.txt" fp t))
                "reviewer@example.com" inv)))
      (should (null (plist-get (gethash "src/dup.txt" idx) :hunk-viewed)))
      (should (= (plist-get (gethash "src/dup.txt" idx) :viewed-ct) 0)))
    ;; marks for both positions mark both hunks
    (let ((idx (git-appraise-viewed--project
                (list (git-appraise-viewed-test--event
                       "e3" "src/dup.txt" fp t "-2,7 +2,7")
                      (git-appraise-viewed-test--event
                       "e4" "src/dup.txt" fp t "-11,7 +11,7"))
                "reviewer@example.com" inv)))
      (should (= (plist-get (gethash "src/dup.txt" idx) :viewed-ct) 2))
      (should (plist-get (gethash "src/dup.txt" idx) :all-hunks)))))

;;; req-2026-11-24-012: reduction determinism
;; covers: git-appraise-viewed--project, git-appraise-viewed--later-p
;; enforcement: test
(ert-deftest viewed/req-2026-11-24-012-reduction-lww ()
  "WHEN the note holds duplicate ids, opposing flags and multiple reviewers THE SYSTEM SHALL reduce deterministically by timestamp, id tie-break and reviewer."
  (let* ((inv (git-appraise-viewed--inventory
               git-appraise-viewed-test--two-hunks))
         (fp (plist-get (nth 0 (plist-get
                                (git-appraise-viewed-test--entry inv
                                                           "src/a.el")
                                :hunks))
                        :fp))
         (ev (lambda (id viewed at &optional actor)
               (let ((e (git-appraise-viewed-test--event
                         id "src/a.el" fp viewed "-1,5 +1,5" at)))
                 (if actor
                     (cons `(actor . ,actor) e)
                   e)))))
    ;; duplicate ID: the first occurrence wins, no double counting
    (let ((idx (git-appraise-viewed--project
                (list (funcall ev "dup" t "2026-01-02T00:00:00Z")
                      (funcall ev "dup" nil "2026-01-03T00:00:00Z"))
                "reviewer@example.com" inv)))
      (should (= (plist-get (gethash "src/a.el" idx) :viewed-ct) 1)))
    ;; later timestamp wins over an earlier unview
    (let ((idx (git-appraise-viewed--project
                (list (funcall ev "a" t "2026-01-02T00:00:00Z")
                      (funcall ev "b" nil "2026-01-02T00:00:00Z")
                      (funcall ev "c" t "2026-01-04T00:00:00Z"))
                "reviewer@example.com" inv)))
      (should (= (plist-get (gethash "src/a.el" idx) :viewed-ct) 1)))
    ;; equal timestamps: the greater record ID is the tie-break
    (let ((idx (git-appraise-viewed--project
                (list (funcall ev "aaa" nil "2026-01-02T00:00:00Z")
                      (funcall ev "zzz" t "2026-01-02T00:00:00Z"))
                "reviewer@example.com" inv)))
      (should (= (plist-get (gethash "src/a.el" idx) :viewed-ct) 1)))
    ;; an unview clears a prior view
    (let ((idx (git-appraise-viewed--project
                (list (funcall ev "a" t "2026-01-02T00:00:00Z")
                      (funcall ev "b" nil "2026-01-05T00:00:00Z"))
                "reviewer@example.com" inv)))
      (should (= (plist-get (gethash "src/a.el" idx) :viewed-ct) 0)))
    ;; another reviewer's events do not leak
    (let ((idx (git-appraise-viewed--project
                (list (funcall ev "a" t "2026-01-02T00:00:00Z"
                               "someone-else@example.com"))
                "reviewer@example.com" inv)))
      (should (= (plist-get (gethash "src/a.el" idx) :viewed-ct) 0)))))

;;; req-2026-11-24-024: changed projection
;; covers: git-appraise-viewed--project
;; enforcement: test
(ert-deftest viewed/req-2026-11-24-024-changed-projection ()
  "WHEN viewed content changes THE SYSTEM SHALL flag the file as previously viewed and changed, and flag hunks whose position was viewed under other content."
  (let* ((inv1 (git-appraise-viewed--inventory
                git-appraise-viewed-test--two-hunks))
         (inv2 (git-appraise-viewed--inventory
                git-appraise-viewed-test--shifted))
         (fp1 (plist-get (nth 0 (plist-get
                                 (git-appraise-viewed-test--entry inv1
                                                            "src/a.el")
                                 :hunks))
                         :fp))
         ;; the old hunk 1 was viewed; the shifted diff changed it
         (idx (git-appraise-viewed--project
               (list (git-appraise-viewed-test--event
                      "e1" "src/a.el" fp1 t "-1,5 +1,5"))
               "reviewer@example.com" inv2))
         (entry2 (gethash "src/a.el" idx)))
    ;; file level: viewed before, now incomplete
    (should (plist-get entry2 :changed))
    (should-not (plist-get entry2 :all-hunks))
    ;; the changed hunk moved position, so no positional stale signal
    (should (null (plist-get entry2 :hunk-changed)))
    ;; a mark stored under the CURRENT range but an old fingerprint
    ;; means: this position was viewed, then its content changed
    (let ((idx2 (git-appraise-viewed--project
                 (list (git-appraise-viewed-test--event
                        "e2" "src/a.el" fp1 t "-1,6 +1,6"))
                 "reviewer@example.com" inv2)))
      (should (equal (plist-get (gethash "src/a.el" idx2) :hunk-changed)
                     '("-1,6 +1,6")))
      (should (= (plist-get (gethash "src/a.el" idx2) :viewed-ct) 0)))))
