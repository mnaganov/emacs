;; Google stuff
(require 'google)
(require 'google3-eglot)
(google3-eglot-setup)
(when (string-match "android" use-project)
    (setq google3-eglot-c++-server 'clangd)
    (add-hook 'c++-mode-hook 'eglot-ensure)
    (add-hook 'java-mode-hook 'eglot-ensure))

;; Turn off flycheck since flymake is also active
(global-flycheck-mode -1)

;; Stylize the eldoc buffer properly
(save-selected-window
  (eldoc-display-in-buffer '(("## Hello eldoc!  ")) t)
  (with-current-buffer eldoc--doc-buffer
    (markdown-mode)
    (setq show-trailing-whitespace nil)))
