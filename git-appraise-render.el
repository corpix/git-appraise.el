;;; git-appraise-render.el --- Safe review buffer replacement -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the previous section tree, markers and overlays until a render succeeds.

;;; Code:

(require 'dash)
(require 'magit-section)

(defun git-appraise-render--discard-old-section-ui ()
  "Drop Magit's cached UI references to the replaced section tree.
Those sections have markers in the backup buffer, which is killed
at the end of this render.  A later post-command highlight must only
walk the new tree."
  (mapc #'delete-overlay magit-section-highlight-overlays)
  (mapc #'delete-overlay magit-section-selection-overlays)
  (setq magit-section-pre-command-section nil
        magit-section-highlight-overlays nil
        magit-section-selection-overlays nil
        magit-section-highlighted-sections nil
        magit-section-focused-sections nil
        magit--ellipses-sections nil
        magit-section-highlight-force-update t))

(defun git-appraise-render--call (render)
  "Call RENDER, restoring the current review buffer if it fails or quits.
Swap the old text into a private buffer instead of erasing it: copying text
alone cannot preserve section markers or folding overlays.  Buffer-local
values are restored without re-entering the major mode."
  (let* ((target (current-buffer))
         (backup (generate-new-buffer " *git-appraise-render-backup*"))
         (locals (buffer-local-variables))
         (position (point))
         (modified (buffer-modified-p))
         (windows (current-window-configuration))
         (swapped nil)
         (complete nil))
    (unwind-protect
        (save-restriction
          (widen)
          (buffer-swap-text backup)
          (setq swapped t)
          (goto-char (point-min))
          (prog1 (funcall render)
            (git-appraise-render--discard-old-section-ui)
            (setq complete t)))
      (when (and swapped (not complete))
        (with-current-buffer target
          (let ((inhibit-read-only t))
            (widen)
            (buffer-swap-text backup)
            (--each locals
              ;; Some intrinsic buffer locals are read-only.  Rendering cannot
              ;; change them; do not try to assign their unchanged values.
              (when (and (consp it)
                         (not (and (boundp (car it))
                                   (eq (symbol-value (car it)) (cdr it)))))
                (set (make-local-variable (car it)) (cdr it))))
            (goto-char position)
            (set-buffer-modified-p modified)))
        (set-window-configuration windows))
      (kill-buffer backup))))

(defun git-appraise-render--wash-sequence (washer)
  "Wash sections with WASHER, aborting if a successful call makes no progress.
A washer must advance through the input or remove text.  Preserve its return
value exactly: a non-nil diagnostic in place of nil would otherwise make
`magit-wash-sequence' loop forever."
  (magit-wash-sequence
   (lambda ()
     (let ((start (point))
           (size (buffer-size)))
       (let ((result (funcall washer)))
         (when (and result
                    (<= (point) start)
                    (>= (buffer-size) size)
                    (not (eobp)))
           (error "Code review washer made no progress: %s" washer))
         result)))))

(provide 'git-appraise-render)
;;; git-appraise-render.el ends here
