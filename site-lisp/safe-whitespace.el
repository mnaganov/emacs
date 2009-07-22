(defvar skip-whitespace-check nil
  "If non-nil, inhibits behaviour of
  `maybe-delete-trailing-whitespace', which is typically a
  write-file-hook.  This variable may be buffer-local, to permit
  extraneous whitespace on a per-file basis.")
(make-variable-buffer-local 'skip-whitespace-check)


(defun buffer-whitespace-normalized-p ()
  "Returns non-nil if the current buffer contains no tab characters
nor trailing whitespace.  This predicate is useful for determining
whether to enable automatic whitespace normalization.  Simply applying
it blindly to other people's files can cause enormously messy diffs!"
  (save-excursion
    (not  (or (progn (goto-char (point-min))
                     (search-forward "\t" nil t))
              (progn (goto-char (point-min))
                     (re-search-forward " +$" nil t))))))


(defun whitespace-check-find-file-hook ()
  (unless (buffer-whitespace-normalized-p)
    (message "Disabling whitespace normalization for this buffer...")
    (setq skip-whitespace-check t)))


;; Install hook so we don't accidentally normalise non-normal files.
(setq find-file-hook
      (cons #'whitespace-check-find-file-hook find-file-hook))


(defun toggle-whitespace-removal ()
  "Toggle the value of `skip-whitespace-check' in this buffer."
  (interactive)
  (setq skip-whitespace-check (not skip-whitespace-check))
  (message "Whitespace trimming %s"
           (if skip-whitespace-check "disabled" "enabled")))


(defun maybe-delete-trailing-whitespace ()
  "Calls `delete-trailing-whitespace' iff buffer-local variable
 skip-whitespace-check is nil.  Returns nil."
  (or skip-whitespace-check
      (delete-trailing-whitespace))
  nil)

(defun maybe-untabify-source-buffer ()
  "Calls `untabify-source-buffer' iff buffer-local variable
 skip-whitespace-check is nil.  Returns nil."
  (or skip-whitespace-check
      (untabify-source-buffer))
  nil)

(provide 'safe-whitespace)
