;;; git-appraise-viewed-e2e-test.el --- viewed state render tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Real git-appraise reviews in disposable repositories (the
;; `git-appraise-backend-test' harness), with viewed events written
;; through the real store.

;;; Code:

(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)
(require 'git-appraise-store)
(require 'git-appraise-viewed)
(require 'git-appraise-viewed-changes)
(require 'git-appraise-backend-test)

(defmacro git-appraise-viewed-e2e-test--with-review (&rest body)
  "Run BODY in a disposable repository with a review worth marking.
The review has: big.txt with two hunks (edits at lines 2 and 10),
other.txt with one hunk, and mode.sh with a mode change only, so
the countable items are 3 hunks + 1 hunk-less file."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "git-appraise-viewed-e2e-" t))
          (default-directory (file-name-as-directory directory))
          revision)
     (git-appraise-test--reset-db)
     (unwind-protect
         (progn
           (git-appraise-test--run directory "git" "init" "-q" "-b" "master")
           (git-appraise-test--run directory "git" "config" "user.name" "Tester")
           (git-appraise-test--run directory "git" "config" "user.email" "tester@example.com")
           (with-temp-file (expand-file-name "big.txt" directory)
             (insert "l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8\nl9\nl10\nl11\nl12\n"))
           (with-temp-file (expand-file-name "other.txt" directory)
             (insert "keep\n"))
           (with-temp-file (expand-file-name "mode.sh" directory)
             (insert "#!/bin/sh\ntrue\n"))
           (git-appraise-test--run directory "git" "add" ".")
           (git-appraise-test--run directory "git" "commit" "-qm" "base")
           (git-appraise-test--run directory "git" "checkout" "-qb" "feature")
           (with-temp-file (expand-file-name "big.txt" directory)
             (insert "l1\nL2\nl3\nl4\nl5\nl6\nl7\nl8\nl9\nL10\nl11\nl12\n"))
           (with-temp-file (expand-file-name "other.txt" directory)
             (insert "changed\n"))
           (chmod (expand-file-name "mode.sh" directory) #o755)
           (git-appraise-test--run directory "git" "add" "-A")
           (git-appraise-test--run directory "git" "commit" "-qm" "review changes")
           (setq revision (git-appraise-test--run directory "git" "rev-parse" "HEAD"))
           (git-appraise-test--run
            directory "git-appraise" "request" "-quiet" "-m" "Viewed review"
            "-target" "refs/heads/master")
           ,@body)
       (let ((review-buf (ignore-errors (get-buffer (git-appraise-pr-buffer-name)))))
         (when (and review-buf (buffer-live-p review-buf))
           (kill-buffer review-buf)))
       (git-appraise-test--reset-db)
       (delete-directory directory t))))

(defun git-appraise-viewed-e2e-test--build (directory revision)
  "Full refresh render of the review in DIRECTORY, async drains included."
  (let ((default-directory (file-name-as-directory directory)))
    (git-appraise-backend-open revision))
  (let ((git-appraise-section-full-refresh? t))
    (git-appraise--build-buffer))
  (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name))))

(defun git-appraise-viewed-e2e-test--rendered ()
  "The plain review buffer text."
  (with-current-buffer (git-appraise-pr-buffer-name)
    (substring-no-properties (buffer-string))))

(defun git-appraise-viewed-e2e-test--face-at (needle)
  "The font-lock-face at NEEDLE in the review buffer, or `not-found'."
  (with-current-buffer (git-appraise-pr-buffer-name)
    (goto-char (point-min))
    (if (search-forward needle nil t)
        (get-text-property (match-beginning 0) 'font-lock-face)
      'not-found)))

(defun git-appraise-viewed-e2e-test--hunk-fp-range (path index)
  "The (FP . RANGE) of PATH's INDEXth hunk in the current raw diff."
  (let* ((inv (git-appraise-viewed--inventory
               (git-appraise-db--pullreq-raw-diff)))
         (h (nth index (plist-get (gethash path inv) :hunks))))
    (cons (plist-get h :fp) (plist-get h :range))))

(defun git-appraise-viewed-e2e-test--write (directory revision events)
  "Store EVENTS for DIRECTORY's REVISION directly."
  (git-appraise-store-append-events directory revision 'viewed events))

(defun git-appraise-viewed-e2e-test--hunk-event (id path index viewed)
  "A valid viewed event for PATH's INDEXth hunk."
  (pcase-let* ((`(,fp . ,range)
                (git-appraise-viewed-e2e-test--hunk-fp-range path index)))
    `((v . 1) (type . "viewed") (id . ,id) (actor . "tester@example.com")
      (item . "hunk") (path . ,path) (fingerprint . ,fp) (range . ,range)
      (viewed . ,(if viewed t :json-false)) (at . "2026-01-01T00:00:00Z"))))

(defun git-appraise-viewed-e2e-test--file-event (id path viewed)
  "A valid viewed event for the hunk-less file PATH."
  (let* ((inv (git-appraise-viewed--inventory
               (git-appraise-db--pullreq-raw-diff)))
         (fp (plist-get (gethash path inv) :file-fp)))
    `((v . 1) (type . "viewed") (id . ,id) (actor . "tester@example.com")
      (item . "file") (path . ,path) (fingerprint . ,fp)
      (viewed . ,(if viewed t :json-false)) (at . "2026-01-01T00:00:00Z"))))

;;; req-2026-11-26-002: changed lines are marked after viewing
;; covers: git-appraise-viewed.el git-appraise-viewed-changes.el git-appraise-store.el
;; covers-fn: git-appraise-viewed-toggle-file, git-appraise-viewed-changes--paint
;; enforcement: test
(ert-deftest viewed-e2e/req-2026-11-26-002-changed-line-margin ()
  "WHEN a viewed file changes THE SYSTEM SHALL mark its newly changed diff lines at the left edge."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (search-forward "big.txt")
      (beginning-of-line)
      (git-appraise-viewed-toggle-file))
    (should (git-appraise-store-viewed-snapshot
             directory revision "tester@example.com" "big.txt"))
    (with-temp-file (expand-file-name "big.txt" directory)
      (insert "l1\nL2-new\nl3\nl4\nl5\nl6\nl7\nl8\nl9\nL10\nl11\nl12\n"))
    (git-appraise-test--run directory "git" "commit" "-qam" "change viewed line")
    (with-current-buffer (git-appraise-pr-buffer-name)
      (git-appraise-reload))
    (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (should (search-forward "+L2-new" nil t))
      (should (cl-some (lambda (ov)
                         (overlay-get ov 'git-appraise-viewed-change-bar))
                       (overlays-at (line-beginning-position))))
      (goto-char (point-min))
      (search-forward "big.txt")
      (beginning-of-line)
      (git-appraise-viewed-toggle-file)
      (should-not (cl-some (lambda (ov)
                             (overlay-get ov 'git-appraise-viewed-change-bar))
                           (overlays-in (point-min) (point-max)))))))

;;; req-2026-11-26-003: navigation skips fully viewed files
;; covers: git-appraise-actions.el git-appraise.el
;; covers-fn: git-appraise-next-unviewed-file
;; enforcement: test
(ert-deftest viewed-e2e/req-2026-11-26-003-next-unviewed-file ()
  "WHEN a reviewer advances to unfinished work THE SYSTEM SHALL skip files already fully viewed."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (search-forward "big.txt")
      (beginning-of-line)
      (git-appraise-viewed-toggle-file)
      (goto-char (point-min))
      (execute-kbd-macro (kbd "N"))
      (let ((path (git-appraise-viewed--file-path (magit-current-section))))
        (should path)
        (should-not (equal path "big.txt"))
        (should-not (plist-get (gethash path git-appraise-section--viewed)
                               :all-hunks))))))

;;; req-2026-11-24-013: render tags and counter
(ert-deftest viewed-e2e/req-2026-11-24-013-render-tags-and-counter ()
  "WHEN some hunks of a file are viewed THE SYSTEM SHALL count them, and tag the file only once ALL its hunks are viewed."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    ;; an untouched review has no marks anywhere
    (should-not (string-match-p "✓" (git-appraise-viewed-e2e-test--rendered)))
    (should (string-match-p "0/4 viewed" (git-appraise-viewed-e2e-test--rendered)))
    ;; one of big.txt's two hunks: counted, but the file is incomplete
    ;; so the rollup does not light the file tag yet
    (git-appraise-viewed-e2e-test--write
     directory revision
     (list (git-appraise-viewed-e2e-test--hunk-event "e1" "big.txt" 0 t)))
    (git-appraise-viewed-e2e-test--build directory revision)
    (let ((rendered (git-appraise-viewed-e2e-test--rendered)))
      (should (string-match-p "1/4 viewed" rendered))
      (should-not (string-match-p "✓" rendered)))
    ;; all of big.txt's hunks: the file is viewed (the rollup), the
    ;; tag appears exactly once, and the no-hunk file stays untagged
    (git-appraise-viewed-e2e-test--write
     directory revision
     (list (git-appraise-viewed-e2e-test--hunk-event "e2" "big.txt" 1 t)))
    (git-appraise-viewed-e2e-test--build directory revision)
    (let ((rendered (git-appraise-viewed-e2e-test--rendered)))
      (should (string-match-p "2/4 viewed" rendered))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (should (re-search-forward "^modified[ \t]+.*big\\.txt" nil t))
        (should (string-match-p
                 "✓"
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
        (should (= (how-many "✓" (point-min) (point-max)) 1))))))

;;; req-2026-11-24-014: hunk dimming
(ert-deftest viewed-e2e/req-2026-11-24-014-hunk-dimming ()
  "WHEN a single hunk is viewed THE SYSTEM SHALL dim exactly that hunk's heading."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (git-appraise-viewed-e2e-test--write
     directory revision
     (list (git-appraise-viewed-e2e-test--hunk-event "e1" "big.txt" 0 t)))
    (git-appraise-viewed-e2e-test--build directory revision)
    ;; hunk 1 of big.txt is dimmed, hunk 2 is not
    (should (eq (git-appraise-viewed-e2e-test--face-at "@@ -1,5 +1,5")
                'git-appraise-dimmed))
    (should-not (eq (git-appraise-viewed-e2e-test--face-at "@@ -7,6 +7,6")
                    'git-appraise-dimmed))))

;;; req-2026-11-24-015: lazy parity
(ert-deftest viewed-e2e/req-2026-11-24-015-lazy-parity ()
  "WHEN the lazy wash is on or off THE SYSTEM SHALL render the same marks and counter."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (git-appraise-viewed-e2e-test--write
     directory revision
     (list (git-appraise-viewed-e2e-test--hunk-event "e1" "big.txt" 0 t)
           (git-appraise-viewed-e2e-test--file-event "e2" "mode.sh" t)))
    (let ((git-appraise-lazy-enabled t))
      (git-appraise-viewed-e2e-test--build directory revision)
      (let ((rendered (git-appraise-viewed-e2e-test--rendered)))
        (should (string-match-p "✓" rendered))
        (should (string-match-p "2/4 viewed" rendered))))
    (let ((git-appraise-lazy-enabled nil))
      (git-appraise-viewed-e2e-test--build directory revision)
      (let ((rendered (git-appraise-viewed-e2e-test--rendered)))
        (should (string-match-p "✓" rendered))
        (should (string-match-p "2/4 viewed" rendered))))))

;;; req-2026-11-24-016: unreadable store safe default
(ert-deftest viewed-e2e/req-2026-11-24-016-unreadable-store-safe-default ()
  "WHEN the stored viewed events cannot be read THE SYSTEM SHALL complete the render with everything unviewed."
  (git-appraise-viewed-e2e-test--with-review
    (cl-letf (((symbol-function 'git-appraise-store-events)
               (lambda (&rest _) (error "store on fire"))))
      (git-appraise-viewed-e2e-test--build directory revision))
    (let ((rendered (git-appraise-viewed-e2e-test--rendered)))
      (should (string-match-p "0/4 viewed" rendered))
      (should-not (string-match-p "✓" rendered)))))

(defun git-appraise-viewed-e2e-test--note-events (directory revision)
  "The stored viewed events for DIRECTORY's REVISION."
  (git-appraise-store-events directory revision 'viewed))

(defun git-appraise-viewed-e2e-test--press (needle command)
  "Move point to NEEDLE in the review buffer and call COMMAND."
  (with-current-buffer (git-appraise-pr-buffer-name)
    (goto-char (point-min))
    (search-forward needle)
    (funcall command))
  (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name))))

(defun git-appraise-viewed-e2e-test--press-file (path command)
  "Call COMMAND on the heading of modified file PATH."
  (with-current-buffer (git-appraise-pr-buffer-name)
    (goto-char (point-min))
    (re-search-forward
     (concat "^modified[ \t]+.*" (regexp-quote path) "\\(?:[ \t]\\|$\\)"))
    (funcall command))
  (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name))))

;;; req-2026-11-24-018: toggle hunk persists
(ert-deftest viewed-e2e/req-2026-11-24-018-toggle-hunk-persists ()
  "WHEN a hunk is toggled viewed THE SYSTEM SHALL persist the mark through rebuilds and unmark it on a second toggle."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (git-appraise-viewed-e2e-test--press "@@ -1,5 +1,5" #'git-appraise-viewed-toggle)
    (should (= (length (git-appraise-viewed-e2e-test--note-events
                        directory revision))
               1))
    (let ((rendered (git-appraise-viewed-e2e-test--rendered)))
      (should (string-match-p "1/4 viewed" rendered))
      (should (eq (git-appraise-viewed-e2e-test--face-at "@@ -1,5 +1,5")
                  'git-appraise-dimmed)))
    ;; the mark survives a fresh full rebuild (the store is re-read)
    (git-appraise-viewed-e2e-test--build directory revision)
    (should (string-match-p "1/4 viewed" (git-appraise-viewed-e2e-test--rendered)))
    ;; toggling again appends an unview event, never rewrites history
    (git-appraise-viewed-e2e-test--press "@@ -1,5 +1,5" #'git-appraise-viewed-toggle)
    (let ((events (git-appraise-viewed-e2e-test--note-events
                   directory revision)))
      (should (= (length events) 2))
      (should (equal (a-get (nth 0 events) 'viewed) t))
      (should (null (a-get (nth 1 events) 'viewed))))
    (should (string-match-p "0/4 viewed" (git-appraise-viewed-e2e-test--rendered)))))

;;; req-2026-11-24-019: file rollup batch
(ert-deftest viewed-e2e/req-2026-11-24-019-file-rollup ()
  "WHEN a file heading is toggled THE SYSTEM SHALL mark all its hunks in one batch, and drop the rollup when a single hunk is unmarked."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    ;; v on the file heading: both hunks in one batch
    (git-appraise-viewed-e2e-test--press-file "big.txt"
                                         #'git-appraise-viewed-toggle)
    (should (= (length (git-appraise-viewed-e2e-test--note-events
                        directory revision))
               2))
    (let ((rendered (git-appraise-viewed-e2e-test--rendered)))
      (should (string-match-p "2/4 viewed" rendered))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (re-search-forward "^modified[ \t]+.*big\\.txt")
        (should (string-match-p
                 "✓"
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))))
    ;; unmarking one hunk drops the file rollup
    (git-appraise-viewed-e2e-test--press "@@ -7,6 +7,6"
                                         #'git-appraise-viewed-toggle)
    (should (= (length (git-appraise-viewed-e2e-test--note-events
                        directory revision))
               3))
    (should (string-match-p "1/4 viewed" (git-appraise-viewed-e2e-test--rendered)))
    (should-not (string-match-p "✓"
                                (git-appraise-viewed-e2e-test--rendered)))
    ;; V from inside the incomplete file completes it: only the
    ;; missing hunk is written
    (git-appraise-viewed-e2e-test--press "@@ -1,5 +1,5"
                                         #'git-appraise-viewed-toggle-file)
    (let ((events (git-appraise-viewed-e2e-test--note-events
                   directory revision)))
      (should (= (length events) 4))
      (should (equal (a-get (nth 3 events) 'viewed) t))
      (should (equal (a-get (nth 3 events) 'range) "-7,6 +7,6")))
    (should (string-match-p "2/4 viewed" (git-appraise-viewed-e2e-test--rendered)))
    ;; V again on the fully viewed file unmarks every current hunk
    (git-appraise-viewed-e2e-test--press "@@ -1,5 +1,5"
                                         #'git-appraise-viewed-toggle-file)
    (let ((events (git-appraise-viewed-e2e-test--note-events
                   directory revision)))
      (should (= (length events) 6))
      (should (seq-every-p (lambda (e) (null (a-get e 'viewed)))
                           (nthcdr 4 events))))
    (should (string-match-p "0/4 viewed" (git-appraise-viewed-e2e-test--rendered)))))

;;; req-2026-11-24-020: hunk-less file toggle
(ert-deftest viewed-e2e/req-2026-11-24-020-nohunk-toggle ()
  "WHEN a metadata-only file heading is toggled THE SYSTEM SHALL write a single file event."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (git-appraise-viewed-e2e-test--press-file "mode.sh"
                                         #'git-appraise-viewed-toggle)
    (let ((events (git-appraise-viewed-e2e-test--note-events
                   directory revision)))
      (should (= (length events) 1))
      (should (equal (a-get (nth 0 events) 'item) "file"))
      (should (null (a-get (nth 0 events) 'range))))
    (should (string-match-p "1/4 viewed" (git-appraise-viewed-e2e-test--rendered)))
    (with-current-buffer (git-appraise-pr-buffer-name)
      (goto-char (point-min))
      (re-search-forward "^modified[ \t]+.*mode\\.sh")
      (should (string-match-p
               "✓"
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))))
    ;; toggling again clears it
    (git-appraise-viewed-e2e-test--press-file "mode.sh"
                                         #'git-appraise-viewed-toggle)
    (should (string-match-p "0/4 viewed" (git-appraise-viewed-e2e-test--rendered)))))

;;; req-2026-11-24-021: failed write leaves state unchanged
;; covers: git-appraise-viewed--write-events
;; enforcement: test
(ert-deftest viewed-e2e/req-2026-11-24-021-failed-write-unchanged ()
  "WHEN the store write fails THE SYSTEM SHALL keep the rendered and stored state unchanged and report the error."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (let ((orig (symbol-function 'git-appraise-store-append-events)))
      (fset 'git-appraise-store-append-events
            (lambda (&rest _) (error "disk on fire")))
      (unwind-protect
          (let ((buf (get-buffer (git-appraise-pr-buffer-name)))
                (err nil))
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "@@ -1,5 +1,5"))
            (setq err (should-error (with-current-buffer buf
                                      (git-appraise-viewed-toggle))
                                    :type 'user-error))
            (should (string-match-p "disk on fire"
                                    (error-message-string err))))
        (fset 'git-appraise-store-append-events orig)))
    (should (null (git-appraise-viewed-e2e-test--note-events
                   directory revision)))
    (should (string-match-p "0/4 viewed" (git-appraise-viewed-e2e-test--rendered)))))

;;; req-2026-11-24-022: marks live beyond Lisp memory
(ert-deftest viewed-e2e/req-2026-11-24-022-cross-process-persistence ()
  "WHEN every in-memory projection is rebuilt from the store THE SYSTEM SHALL still show the marks."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (git-appraise-viewed-e2e-test--press "@@ -1,5 +1,5"
                                         #'git-appraise-viewed-toggle)
    (git-appraise-viewed-e2e-test--build directory revision)
    (should (string-match-p "1/4 viewed" (git-appraise-viewed-e2e-test--rendered)))
    (should (eq (git-appraise-viewed-e2e-test--face-at "@@ -1,5 +1,5")
                'git-appraise-dimmed))))

;;; req-2026-11-24-023: revision and reviewer isolation
(ert-deftest viewed-e2e/req-2026-11-24-023-isolation ()
  "WHEN another reviewer or another review revision holds marks THE SYSTEM SHALL not share any viewed state."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (git-appraise-viewed-e2e-test--press "@@ -1,5 +1,5"
                                         #'git-appraise-viewed-toggle)
    (let ((events (git-appraise-viewed-e2e-test--note-events
                   directory revision)))
      ;; the same note read for another reviewer yields nothing
      (with-current-buffer (git-appraise-pr-buffer-name)
        (let ((idx (git-appraise-viewed--project
                    events "other@example.com"
                    git-appraise-viewed--buffer-inventory)))
          (should (= (plist-get (gethash "big.txt" idx) :viewed-ct) 0)))
        ;; the reviewer's own events project as viewed
        (let ((idx (git-appraise-viewed--project
                    events "tester@example.com"
                    git-appraise-viewed--buffer-inventory)))
          (should (= (plist-get (gethash "big.txt" idx) :viewed-ct) 1)))))
    ;; a mark stored on the PARENT revision's note never affects this
    ;; review: reads are anchored on the review revision
    (let ((parent (git-appraise-test--run
                   directory "git" "rev-parse" "HEAD~1"))
          (event (git-appraise-viewed-e2e-test--hunk-event
                  "e-parent" "big.txt" 0 t)))
      (git-appraise-store-append-events directory parent 'viewed
                                        (list event))
      (git-appraise-viewed-e2e-test--build directory revision)
      (should (string-match-p "1/4 viewed" (git-appraise-viewed-e2e-test--rendered))))))

;;; req-2026-11-24-017: reload keeps marks, flags changed files
;; covers: git-appraise-viewed.el git-appraise-backend.el git-appraise-diff.el
;; covers-fn: git-appraise-backend-refresh, git-appraise-viewed--render-prepare
;; enforcement: test
(ert-deftest viewed-e2e/req-2026-11-24-017-reload-keeps-marks ()
  "WHEN the review gains a commit THE SYSTEM SHALL keep unchanged hunks' marks and flag the changed file as previously viewed."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (git-appraise-viewed-e2e-test--write
     directory revision
     (list (git-appraise-viewed-e2e-test--hunk-event "e1" "big.txt" 0 t)
           (git-appraise-viewed-e2e-test--hunk-event "e2" "other.txt" 0 t)))
    (git-appraise-viewed-e2e-test--build directory revision)
    (should (string-match-p "2/4 viewed" (git-appraise-viewed-e2e-test--rendered)))
    ;; a followup commit changes other.txt's hunk content
    (with-temp-file (expand-file-name "other.txt" directory)
      (insert "changed again\n"))
    (git-appraise-test--run directory "git" "commit" "-qam" "followup")
    (git-appraise-viewed-e2e-test--build directory revision)
    (let ((rendered (git-appraise-viewed-e2e-test--rendered)))
      ;; big.txt keeps its mark; other.txt was viewed before, changed since
      (should (string-match-p "1/4 viewed" rendered))
      (should (string-match-p "\\[CHANGED\\]" rendered))
      (should (eq (git-appraise-viewed-e2e-test--face-at "@@ -1,5 +1,5")
                  'git-appraise-dimmed)))))

(defconst git-appraise-viewed-e2e-test--fixtures-dir
  (expand-file-name "fixtures"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "Directory containing viewed-e2e fixture files.")

(defun git-appraise-viewed-e2e-test--normalize-buffer (text)
  "Normalize dynamic parts of review buffer TEXT for fixture comparison."
  (replace-regexp-in-string
   "^[0-9a-f]\\{7\\} \\[[^]]+\\] review changes$"
   "<HASH> [<DATE>] review changes" text t t))

;;; req-2026-11-24-132: viewed files and fold state survive reload
;; covers: git-appraise-viewed.el git-appraise-fold-state.el git-appraise-folding.el
;; covers-fn: git-appraise-reload, git-appraise-folding--set-file
;; enforcement: test
(ert-deftest viewed-e2e/req-2026-11-24-132-viewed-and-fold-state-survive-reload ()
  "WHEN a reviewer reloads a review with viewed files THE SYSTEM SHALL preserve viewed marks and file fold state."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (git-appraise-viewed-e2e-test--write
     directory revision
     (list (git-appraise-viewed-e2e-test--hunk-event "e1" "big.txt" 0 t)
           (git-appraise-viewed-e2e-test--hunk-event "e2" "big.txt" 1 t)
           (git-appraise-viewed-e2e-test--hunk-event "e3" "other.txt" 0 t)))
    (git-appraise-viewed-e2e-test--build directory revision)
    (let (big other before after)
      (with-current-buffer (git-appraise-pr-buffer-name)
        (magit-map-sections
         (lambda (s)
           (when (eq (oref s type) 'file)
             (pcase (oref s value)
               ("big.txt" (setq big s))
               ("other.txt" (setq other s)))))
         magit-root-section)
        (should big)
        (should other)
        (git-appraise-folding--set-file big nil)
        (git-appraise-folding--set-file other t)
        (setq before (buffer-substring-no-properties (point-min) (point-max)))
        (should-not (oref big hidden))
        (should (oref other hidden))
        (should (= 0 (cl-count-if (lambda (ov) (overlay-get ov 'invisible))
                                  (overlays-at (oref big content)))))
        (should (= 1 (cl-count-if (lambda (ov) (overlay-get ov 'invisible))
                                  (overlays-at (oref other content)))))
        (git-appraise-reload))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (setq after (buffer-substring-no-properties (point-min) (point-max))
              big nil other nil)
        (magit-map-sections
         (lambda (s)
           (when (eq (oref s type) 'file)
             (pcase (oref s value)
               ("big.txt" (setq big s))
               ("other.txt" (setq other s)))))
         magit-root-section)
        (should big)
        (should other)
        (should-not (oref big hidden))
        (should (oref other hidden))
        (should (= 0 (cl-count-if (lambda (ov) (overlay-get ov 'invisible))
                                  (overlays-at (oref big content)))))
        (should (= 1 (cl-count-if (lambda (ov) (overlay-get ov 'invisible))
                                  (overlays-at (oref other content)))))
        (should (invisible-p (oref other content)))
        (should-not (invisible-p (oref big content)))
        (should (equal before after))
        (let* ((fixture (expand-file-name "viewed-reload-buffer.txt"
                                           git-appraise-viewed-e2e-test--fixtures-dir))
               (expected (with-temp-buffer
                           (insert-file-contents fixture)
                           (buffer-string))))
          (should (equal (git-appraise-viewed-e2e-test--normalize-buffer after)
                         expected)))))))

;;; req-2026-11-24-133: viewed state survives buffer close and reopen
;; covers: git-appraise-viewed.el git-appraise-backend.el
;; covers-fn: git-appraise-backend-open, git-appraise--build-buffer
;; enforcement: test
(ert-deftest viewed-e2e/req-2026-11-24-133-viewed-state-survives-reopen ()
  "WHEN a reviewer closes and reopens a review with viewed files THE SYSTEM SHALL preserve viewed marks."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (git-appraise-viewed-e2e-test--write
     directory revision
     (list (git-appraise-viewed-e2e-test--hunk-event "e1" "big.txt" 0 t)
           (git-appraise-viewed-e2e-test--hunk-event "e2" "big.txt" 1 t)
           (git-appraise-viewed-e2e-test--hunk-event "e3" "other.txt" 0 t)))
    (git-appraise-viewed-e2e-test--build directory revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (should (string-match-p "3/4 viewed" (buffer-string)))
      (should (string-match-p "✓.*big\\.txt" (buffer-string)))
      (should (string-match-p "✓.*other\\.txt" (buffer-string))))
    (kill-buffer (git-appraise-pr-buffer-name))
    (git-appraise-viewed-e2e-test--build directory revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (should (string-match-p "3/4 viewed" (buffer-string)))
      (should (string-match-p "✓.*big\\.txt" (buffer-string)))
      (should (string-match-p "✓.*other\\.txt" (buffer-string)))
      (should-not (string-match-p "✓.*mode\\.sh" (buffer-string))))))

(defun git-appraise-viewed-e2e-test--file-fold-states ()
  "Alist of (path . folded?) for file sections in the current review buffer."
  (let (states)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (magit-map-sections
       (lambda (s)
         (when (eq (oref s type) 'file)
           (push (cons (oref s value)
                       (if (oref s hidden) 'folded 'unfolded))
                 states)))
       magit-root-section))
    (nreverse states)))

;;; req-2026-11-24-134: fold state survives close and reload
;; covers: git-appraise-fold-state.el git-appraise-folding.el git-appraise-section.el
;; covers-fn: git-appraise-folding--set-file, git-appraise-backend-open, git-appraise-reload
;; enforcement: test
(ert-deftest viewed-e2e/req-2026-11-24-134-fold-state-survives-close-and-reload ()
  "WHEN a reviewer closes and reopens or reloads a review THE SYSTEM SHALL preserve file fold state."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (with-current-buffer (git-appraise-pr-buffer-name)
      (let (big mode other)
        (magit-map-sections
         (lambda (s)
           (when (eq (oref s type) 'file)
             (pcase (oref s value)
               ("big.txt" (setq big s))
               ("mode.sh" (setq mode s))
               ("other.txt" (setq other s)))))
         magit-root-section)
        (should big) (should mode) (should other)
        (git-appraise-folding--set-file big t)
        (git-appraise-folding--set-file mode nil)
        (git-appraise-folding--set-file other nil)))
    (let* ((fixture (expand-file-name "viewed-reload-fold-state.sexp"
                                      git-appraise-viewed-e2e-test--fixtures-dir))
           (expected (with-temp-buffer
                       (insert-file-contents fixture)
                       (read (current-buffer)))))
      (should (equal (git-appraise-viewed-e2e-test--file-fold-states) expected))
      (kill-buffer (git-appraise-pr-buffer-name))
      (git-appraise-viewed-e2e-test--build directory revision)
      (should (equal (git-appraise-viewed-e2e-test--file-fold-states) expected))
      (with-current-buffer (git-appraise-pr-buffer-name)
        (git-appraise-reload))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (should (equal (git-appraise-viewed-e2e-test--file-fold-states) expected)))))

(defun git-appraise-viewed-e2e-test--file-paths-in-order ()
  "Modified file paths in buffer order."
  (with-current-buffer (git-appraise-pr-buffer-name)
    (let (paths)
      (magit-map-sections
       (lambda (s)
         (when (magit-file-section-p s)
           (push (oref s value) paths)))
       magit-root-section)
      (nreverse paths))))

(defun git-appraise-viewed-e2e-test--set-region-over-paths (paths)
  "Set an active region spanning file headings for PATHS."
  (with-current-buffer (git-appraise-pr-buffer-name)
    (transient-mark-mode 1)
    (let (beg end)
      (dolist (path paths)
        (goto-char (point-min))
        (re-search-forward
         (concat "^modified[ \t]+.*" (regexp-quote path) "\\(?:[ \t]\\|$\\)"))
        (let ((bol (line-beginning-position))
              (eol (line-end-position)))
          (setq beg (if beg (min beg bol) bol)
                end (if end (max end eol) eol))))
      (goto-char end)
      (set-mark beg)
      (goto-char end)
      (activate-mark))))

(defun git-appraise-viewed-e2e-test--file-viewed-indicator-p (path)
  "Return non-nil when PATH's file heading shows a viewed indicator."
  (with-current-buffer (git-appraise-pr-buffer-name)
    (goto-char (point-min))
    (re-search-forward
     (concat "^modified[ \t]+.*" (regexp-quote path) "\\(?:[ \t]\\|$\\)"))
    (string-match-p "✓"
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))))

;;; req-2026-11-25-030:
;; covers: git-appraise-viewed.el
;; covers-fn: git-appraise-viewed-toggle, git-appraise-viewed-toggle-file, git-appraise-viewed--region-file-sections, git-appraise-viewed--toggle-files
;; enforcement: test
(ert-deftest viewed-e2e/req-2026-11-25-030-region-bulk-toggle-viewed ()
  "WHEN a region selects multiple file sections and the user toggles viewed,
THE SYSTEM SHALL apply a group viewed action across the selected files, including folded files."
  (git-appraise-viewed-e2e-test--with-review
    (git-appraise-viewed-e2e-test--build directory revision)
    (let* ((paths (git-appraise-viewed-e2e-test--file-paths-in-order))
           (selected (cl-subseq paths 0 2))
           (unselected (nth 2 paths))
           (selected-items (cl-loop for p in selected
                                    sum (if (equal p "big.txt") 2 1))))
      ;; Fold the first selected file to prove folded selection works.
      (with-current-buffer (git-appraise-pr-buffer-name)
        (goto-char (point-min))
        (re-search-forward
         (concat "^modified[ \t]+.*" (regexp-quote (car selected)) "\\(?:[ \t]\\|$\\)"))
        (let ((sec (git-appraise-eldoc--file-section (magit-current-section))))
          (git-appraise-folding--set-file sec t)))
      (git-appraise-viewed-e2e-test--set-region-over-paths selected)
      (with-current-buffer (git-appraise-pr-buffer-name)
        (call-interactively #'git-appraise-viewed-toggle))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (should (string-match-p
               (format "%d/4 viewed" selected-items)
               (git-appraise-viewed-e2e-test--rendered)))
      (dolist (p selected)
        (should (git-appraise-viewed-e2e-test--file-viewed-indicator-p p)))
      (should-not (git-appraise-viewed-e2e-test--file-viewed-indicator-p unselected))
      ;; Group semantics: all selected files fully viewed -> unmark all selected.
      (git-appraise-viewed-e2e-test--set-region-over-paths selected)
      (with-current-buffer (git-appraise-pr-buffer-name)
        (call-interactively #'git-appraise-viewed-toggle))
      (git-appraise-test--drain-lazy (get-buffer (git-appraise-pr-buffer-name)))
      (should (string-match-p "0/4 viewed" (git-appraise-viewed-e2e-test--rendered))))))

(provide 'git-appraise-viewed-e2e-test)
;;; git-appraise-viewed-e2e-test.el ends here
