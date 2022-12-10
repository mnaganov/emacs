;; Keybindings
(global-set-key [f5] (lambda () (interactive) (revert-buffer nil t)))
(if (eq system-type 'windows-nt)
    (global-set-key [f6] 'rgrep)
    (global-set-key [f6] 'find-grep))

(require 'find-things-fast)
(global-set-key '[f1] 'ftf-find-file)

;; On Mac and Windows WSL, add commands for converting the clipboard from HTML to MD and back.
(cond ((eq system-type 'darwin)
       (setq shell-command-history '("pandoc -f markdown_github -t html | pbcopy"
                                     "pbpaste | pandoc -f html-native_divs-native_spans -t markdown_github")))
      ((is-windows-wsl)
       (setq shell-command-history '("pandoc -f markdown_github -t html | clip.exe"
                                     "powershell.exe Get-Clipboard | pandoc -f html-native_divs-native_spans -t markdown_github"))))

(add-to-list 'auto-mode-alist '("\\.ino$" . c-mode))
