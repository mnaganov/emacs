;; Compile command
(require 'compile)

;; Keybindings
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t)))

(require 'find-things-fast)
(global-set-key '[f1] 'ftf-find-file)
