;; Compile command
(require 'compile)

;; Keybindings
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t)))
(global-set-key [f6] 'find-grep)

(require 'find-things-fast)
(global-set-key '[f1] 'ftf-find-file)

;; On Mac, add commands for converting the clipboard from HTML to MD
;; and back.
(if (eq system-type 'darwin)
    (setq shell-command-history '("pandoc -f markdown_github -t html | pbcopy"
                                  "pbpaste | pandoc -f html-native_divs-native_spans -t markdown_github")))
