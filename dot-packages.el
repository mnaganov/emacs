(add-to-list 'load-path (concat emacs-root "site-lisp"))
(setq packages-root (concat emacs-root "site-lisp/"))

;; Gambit Scheme
(require 'gambit)

;; Haskell
(load (concat packages-root "haskell-mode-2.4/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)

;; GIT VC support
(add-to-list 'load-path (concat packages-root "git"))
(require 'git)
(require 'vc-git)
(add-to-list 'vc-handled-backends 'GIT)
(global-auto-revert-mode)
;; Buffer for Git
(shell (generate-new-buffer "=git="))
