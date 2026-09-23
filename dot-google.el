;; Google stuff
(require 'google)

(defun my-project-try-android (dir)
  "Detect top-level Android checkout by locating .repo."
  (when-let ((root (locate-dominating-file dir ".repo")))
    ;; Return a project object pointing to the top of the Android repo checkout.
    ;; (cons 'transient root) is universally understood by all versions of project.el and eglot.
    (cons 'transient (file-name-as-directory (expand-file-name root)))))

;; 2. Configure Google3 Eglot & clangd for Android
(when (string-match-p "android" use-project)
  (require 'google3-eglot)
  (customize-set-variable 'google3-eglot-c++-server 'clangd)
  ;; limit clangd resource usage
  (setq google3-eglot-clangd-args
        '("-j=4"
          "--background-index=false"
          "--clang-tidy=false"))
  (google3-eglot-setup)
  (add-hook 'project-find-functions #'my-project-try-android -50))

;; Turn off flycheck since flymake is also active
(global-flycheck-mode -1)

;; Stylize the eldoc buffer properly
(save-selected-window
  (eldoc-display-in-buffer '(("## Hello eldoc!  ")) t)
  (with-current-buffer eldoc--doc-buffer
    (markdown-mode)
    (setq show-trailing-whitespace nil)))
