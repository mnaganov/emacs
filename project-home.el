;; Keybindings

(require 'ag)
(global-set-key [f5] (lambda () (interactive) (revert-buffer nil t)))
(cond ((eq system-type 'windows-nt)
       (global-set-key [f6] 'rgrep))
      ((eq system-type 'gnu/linux)
       (global-set-key [f6] 'ag-project-regexp))
      (t
       (global-set-key [f6] 'find-grep)))

(require 'find-things-fast)
(global-set-key '[f1] 'ftf-find-file)

;; On Mac and Windows WSL, add commands for converting the clipboard from HTML to MD and back.
(cond ((eq system-type 'darwin)
       (setq shell-command-history '("pandoc -f markdown_github -t html | pbcopy"
                                     "pbpaste | pandoc -f html-native_divs-native_spans -t markdown | sed -e '1h;2,$H;$!d;g' -e 's/{[^{}]*}//g'")))
      ((is-windows-wsl)
       (setq shell-command-history '("pandoc -f markdown_github -t html | clip.exe"
                                     "powershell.exe Get-Clipboard | pandoc -f html-native_divs-native_spans -t markdown_github"))))

(add-to-list 'auto-mode-alist '("\\.ino$" . c-mode))

(defun smart-quotes-dwim (beg end)
  "Convert straight quotes to typographic ones in region, or whole buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      ;; 1. Primes: 5" -> 5″ , 6' -> 6′
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]\\)\"" nil t)
        (replace-match "\\1″" t))
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]\\)'" nil t)
        (replace-match "\\1′" t))
      ;; 2. Apostrophes: don't, I'm
      (goto-char (point-min))
      (while (re-search-forward "\\([[:alpha:]]\\)'\\([[:alpha:]]\\)" nil t)
        (replace-match "\\1’\\2" t))
      ;;    Decade abbreviations: '90s
      (goto-char (point-min))
      (while (re-search-forward "'\\([0-9]\\)" nil t)
        (replace-match "’\\1" t))
      ;; 3. Paired double quotes
      (goto-char (point-min))
      (while (re-search-forward "\"\\([^\"]*\\)\"" nil t)
        (replace-match "“\\1”" t))
      ;; 4. Paired single quotes
      (goto-char (point-min))
      (while (re-search-forward "'\\([^']*\\)'" nil t)
        (replace-match "‘\\1’" t)))))
