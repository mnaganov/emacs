;; Project Chrome stuff

;; Compile command
(require 'compile)

(setq compile-history
      '("PATH=$HOME/goma:$HOME/depot_tools:$PATH ninja -j150 -l30 -C out/Release chrome"
        "PATH=$HOME/goma:$HOME/depot_tools:$PATH ninja -j150 -l30 -C out/Debug chrome"))

(setq compile-command
   "PATH=$HOME/goma:$HOME/depot_tools:$PATH ninja -j150 -l30 -C out/Release chrome")

;; Use Ack instead of Find-grep
(require 'ack)

;; Keybindings
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t)))
(if (eq system-type 'windows-nt)
    ;; ack hangs in cygwin under emacs, use good 'ol find-grep instead
    (progn
     (global-set-key [f6] 'find-grep)
     (custom-set-variables
      '(grep-find-command "gnu-find . -type f \\( -name \"*.cc\" -or -name \"*.h\" \\) -print0 | xargs -0 -e grep -nH -e ")))
    (global-set-key [f6] 'ack))
(global-set-key [f7] 'compile)
(global-set-key [f8] 'recompile)

(defun set-webkit-c-style ()
   (setq c-basic-offset 4)
   (setq tab-width 8)
   (setq indent-tabs-mode nil)
   (setq fill-column 120)
   (c-set-offset 'substatement-open 0))

(defun maybe-set-webkit-c-style ()
  (if (and (char-or-string-p buffer-file-name) (string-match "webkit" (downcase buffer-file-name)) (not (string-match "glue" (downcase buffer-file-name))))
      (set-webkit-c-style)))

(defun set-webkit-js2-style ()
   (setq js2-basic-offset 4)
   (setq tab-width 8)
   (setq fill-column 120)
   (setq indent-tabs-mode nil))

(defun maybe-set-webkit-js2-style ()
  (if (and (char-or-string-p buffer-file-name) (string-match "webkit" (downcase buffer-file-name)))
      (set-webkit-js2-style)))

(defun set-android-java-style ()
   (setq c-basic-offset 4)
   (setq fill-column 100)
   (setq c-continued-statement-offset 8)
   (c-set-offset 'statement-cont '++)
   (c-set-offset 'arglist-intro '++)
   (c-set-offset 'arglist-cont-nonempty '++))

(defun maybe-set-android-java-style ()
  (if (and (char-or-string-p buffer-file-name)
           (or (string-match "android" (downcase buffer-file-name))
               (string-match "chrome" (downcase buffer-file-name))))
      (set-android-java-style)))

(load "~/chrome/src/third_party/clang_format/script/clang-format.el")
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)
            (local-set-key  (kbd "C-\\") 'clang-format-region)
            (setq tags-revert-without-query t)
            (google-set-c-style)
            (maybe-set-webkit-c-style)))

(add-hook 'js2-mode-hook 'maybe-set-webkit-js2-style)

(add-hook 'java-mode-hook 'maybe-set-android-java-style)

(add-hook 'python-mode-hook
          (lambda()
            (setq python-indent 2)))

(require 'find-things-fast)
(global-set-key '[f1] 'ftf-find-file)

(defun open-shell-buffer (buffer-name startup-command)
  (switch-to-buffer (shell buffer-name))
  (set-marker comint-last-output-start (point))
  (insert startup-command)
  (comint-send-input nil t))
(open-shell-buffer "=chrome=" "cd ~/chrome/src")
(open-shell-buffer "=webkit=" "cd ~/chrome/src/third_party/WebKit")
