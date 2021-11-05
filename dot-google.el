;; Google stuff
(require 'google)
(require 'google3-eglot)
(google3-eglot-setup)
(if (string-match "android" use-project)
    (setq google3-eglot-c++-server 'clangd))
(require 'google-ycmd)

;; Turn off flycheck since flymake is also active
(global-flycheck-mode -1)

;; Stylize the eldoc buffer properly
(save-selected-window
  (eldoc-display-in-buffer '(("## Hello eldoc!  ")) t)
  (with-current-buffer eldoc--doc-buffer
    (markdown-mode)
    (setq show-trailing-whitespace nil)))
