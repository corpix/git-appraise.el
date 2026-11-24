;;; git-appraise-section-history.el --- Historical review comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Older line comments belong to their original commit, not to a guessed
;; current diff line.  Render them beside the affected file when possible.

;;; Code:
(require 'a)
(require 'dash)
(require 'magit-section)
(require 'git-appraise-section-shared)
(require 'git-appraise-comment-format)
(require 'git-appraise-viewed)

(declare-function git-appraise-section--propertize-commit-link
                  "git-appraise-section-header" (rev display))
(defvar git-appraise-commit-link-map)

(defclass git-appraise-historical-comment-section (magit-section) ())
(defclass git-appraise-historical-comments-header-section (magit-section) ())

(cl-defmethod magit-section-ident-value ((section git-appraise-historical-comment-section))
  "Identify SECTION by its underlying comment."
  (a-get (oref section value) 'databaseId))

(defface git-appraise-historical-rejected-face
  '((t :inherit error))
  "Left marker for a historical comment rejecting a change."
  :group 'git-appraise)
(defface git-appraise-historical-resolved-face
  '((t :inherit success))
  "Left marker for an explicitly resolved historical comment."
  :group 'git-appraise)
(defface git-appraise-historical-neutral-face
  '((t :inherit shadow))
  "Left marker for a historical FYI comment."
  :group 'git-appraise)

(defun git-appraise-history--nodes ()
  "Return the review's historical line comments."
  (a-get-in (git-appraise-db--pullreq-raw-infos)
            '(historicalComments nodes)))

(defun git-appraise-history--file-count (path)
  "Return the number of comments associated with PATH."
  (+ (--count (equal (a-get it 'path) path)
               (git-appraise-history--nodes))
     (--count (equal (a-get it 'path) path)
              (--mapcat (a-get-in it '(comments nodes))
                        (git-appraise-db--pullreq-raw-comments)))))

(defun git-appraise-history--marker-face (node)
  "Return the left-marker face for NODE's decision state."
  (pcase (a-get node 'state)
    ("CHANGES_REQUESTED" 'git-appraise-historical-rejected-face)
    ("APPROVED" 'git-appraise-historical-resolved-face)
    (_ 'git-appraise-historical-neutral-face)))

(defun git-appraise-history--paint-border (section node)
  "Paint a slim left marker beside historical SECTION for NODE."
  (let ((face (git-appraise-history--marker-face node)))
    (save-excursion
      (goto-char (oref section start))
      (dotimes (_ (min 100 (count-lines (oref section start)
                                      (oref section end))))
        (when (< (point) (oref section end))
          (let ((ov (make-overlay (point) (min (1+ (point)) (point-max)))))
            (overlay-put ov 'before-string
                         (propertize "│ " 'face face))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'git-appraise-historical-border t))
          (forward-line 1))))))

(defun git-appraise-history--insert-node (node)
  "Insert historical comment NODE folded under the current section."
  (let* ((path (a-get node 'path))
         (line (a-get node 'line))
         (commit (a-get node 'commit))
         (state (a-get node 'state))
         (author (a-get-in node '(author login)))
         (when-text (git-appraise-comment-format-time
                     (a-get node 'createdAt)))
         (short-commit (if (and commit (> (length commit) 12))
                           (substring commit 0 12)
                         (or commit "?")))
         (suffix (concat (if when-text (concat " · " when-text) "")
                         (pcase state
                           ("CHANGES_REQUESTED" " · REJECTED")
                           ("APPROVED" " · RESOLVED")
                           (_ ""))))
         hash-beg hash-end
         (section
          (magit-insert-section
              (git-appraise-historical-comment-section node t)
            (insert (propertize
                     (format "Earlier comment by %s · %s:%s @ "
                             (or author "unknown") path line)
                     'font-lock-face 'magit-section-heading))
            (setq hash-beg (point))
            (insert (if commit
                        (git-appraise-section--propertize-commit-link
                         commit short-commit)
                      (propertize short-commit
                                  'font-lock-face 'magit-section-heading)))
            (setq hash-end (point))
            (insert (propertize suffix 'font-lock-face 'magit-section-heading))
            (magit-insert-heading)
            (insert (or (a-get node 'bodyText) ""))
            (insert ?\n))))
    (when (and commit hash-beg hash-end)
      (put-text-property hash-beg hash-end
                         'keymap git-appraise-commit-link-map))
    (git-appraise-history--paint-border section node)
    (git-appraise-section--hide-if-hidden section)
    section))

(defun git-appraise-history-insert-for-file (path)
  "Insert historical comments for PATH inside its diff section."
  (--each (--filter (equal (a-get it 'path) path)
                    (git-appraise-history--nodes))
    (git-appraise-history--insert-node it)))

(defun git-appraise-history-insert-unmatched ()
  "Insert historical comments whose files are absent from this diff."
  (let ((nodes (--filter
                (not (and (hash-table-p git-appraise-viewed--buffer-inventory)
                          (gethash (a-get it 'path)
                                   git-appraise-viewed--buffer-inventory)))
                (git-appraise-history--nodes))))
    (when nodes
      (git-appraise-section--hide-if-hidden
       (magit-insert-section
           (git-appraise-historical-comments-header-section nil t)
         (insert (format "Historical comments (%d)" (length nodes)))
         (magit-insert-heading)
         (--each nodes (git-appraise-history--insert-node it)))))))

(provide 'git-appraise-section-history)
;;; git-appraise-section-history.el ends here
