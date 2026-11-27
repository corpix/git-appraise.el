;;; git-appraise-test-viewed-fixture.el --- Multi-file review fixture -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'git-appraise)
(require 'git-appraise-test-helpers)
(require 'git-appraise-store)
(require 'git-appraise-viewed)

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

(defconst git-appraise-viewed-e2e-test--fixtures-dir
  git-appraise-test--fixture-directory
  "Directory containing viewed-e2e fixture files.")

(defun git-appraise-viewed-e2e-test--normalize-buffer (text)
  "Normalize dynamic parts of review buffer TEXT for fixture comparison."
  (replace-regexp-in-string
   "^[0-9a-f]\\{7\\} \\[[^]]+\\] review changes$"
   "<HASH> [<DATE>] review changes" text t t))

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


(provide 'git-appraise-test-viewed-fixture)
;;; git-appraise-test-viewed-fixture.el ends here
