;; Compile command
(require 'compile)

;; Keybindings
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t)))
(global-set-key [f6] 'find-grep)

(require 'find-things-fast)
(global-set-key '[f1] 'ftf-find-file)
