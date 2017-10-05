;; == Set up bundled Emacs packages ==

;; ido
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t) ;; fuzzy matching

;; Move between windows using M-Arrows
(require 'windmove)
(windmove-default-keybindings 'meta)
;; This is a fallback for terminals
(global-set-key (kbd "\e <up>") 'windmove-up)
(global-set-key (kbd "\e <down>") 'windmove-down)
(global-set-key (kbd "\e <left>") 'windmove-left)
(global-set-key (kbd "\e <right>") 'windmove-right)

;; Use C-c left and C-c right to undo/redo window configuration changes
(winner-mode 1)

;; When opening another file with the same name, instead of <N> suffix,
;; use directory name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer t)
(setq uniquify-ignore-buffers-ru "^\\*")

;; Support ANSI control sequences in shell
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

;; Whitespace highlighting
(require 'whitespace)
(setq whitespace-style (quote (face trailing tabs empty)))
(global-whitespace-mode 1)

;; == Set up external packages ==

(add-to-list 'load-path (concat emacs-root "site-lisp"))
(setq packages-root (concat emacs-root "site-lisp/"))


;; == OS-specific setup ==
(if (eq system-type 'windows-nt)
    (load-file (concat emacs-root "dot-windows.el")))
(when (not window-system)
     (require 'xterm-frobs)
     (defun my-xterm-title-hook ()
       (xterm-set-window-title (format-mode-line frame-title-format)))
     (add-hook 'post-command-hook 'my-xterm-title-hook))

;; == Other packages ==

;; Haskell
(load (concat packages-root "haskell-mode-2.4/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)


;; JS2
(if (version< emacs-version "25.0")
    (autoload 'js2-mode (concat "js2-mode-emacs" (number-to-string emacs-major-version)) nil t)
    (progn
     (add-to-list 'load-path (concat packages-root "js2-mode"))
     (autoload 'js2-mode "js2-mode" "JS2 mode" t)))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'js2-mode-reset nil t)
(if use-google-stuff
    (require 'js2-google))


;; Fill-Column-Indicator
(setq-default fill-column 80)
(require 'fill-column-indicator)
(add-hook 'c++-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'java-mode-hook 'fci-mode)
(add-hook 'html-mode-hook 'fci-mode)
(add-hook 'javascript-mode-hook 'fci-mode)
(add-hook 'js2-mode-hook 'fci-mode)


;; Gambit Scheme
(require 'gambit)


;; GIT VC support
(add-to-list 'load-path (concat packages-root "git"))
(require 'git)
(require 'vc-git)
(add-to-list 'vc-handled-backends 'git)
(global-auto-revert-mode)

;; C++ style
(unless use-google-stuff
    (require 'google-c-style)
    (unless (or (string-match "chrome" use-project) (string-match "v8" use-project) (string-match "android" use-project))
            (add-hook 'c-mode-common-hook 'google-set-c-style))
    (add-hook 'c-mode-common-hook 'google-make-newline-indent))

;; Fix indentation for Java annotations
(add-hook 'java-mode-hook
          '(lambda ()
             "Treat Java 1.5 @-style annotations as comments."
             (setq c-comment-start-regexp
                   "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
             (modify-syntax-entry ?@ "< b"
                                  java-mode-syntax-table)))

;; NXML
(add-to-list 'load-path (concat packages-root "nxml-mode"))
(load "rng-auto.el")
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|gxp\\)\\'" . nxml-mode))

;; Yasnippet
(add-to-list 'load-path (concat packages-root "yasnippet"))
(require 'yasnippet)
(yas-global-mode 1)

;; gyp
(require 'gyp)

;; Octave
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; flx-ido
(require 'flx-ido)
(flx-ido-mode t)
(setq flx-ido-use-faces nil)
(setq gc-cons-threshold 20000000)

;; protobuf
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

;; direx -- Directory tree browser
(require 'direx)
(defun direx-window-p (&optional w)
  (and (window-dedicated-p w)
       (direx:buffer-live-p (window-buffer w))))
(defun find-dedicated-direx-window ()
  (let ((dedicated nil))
    (walk-windows (function (lambda (w)
                              (if (direx-window-p w)
                                  (setq dedicated w)))))
    dedicated))
;; Opens the directory list on the left side of the frame
;; To close it, either use C-x 0, or C-c left-arrow.
(defun direx-on-the-left ()
  (interactive)
  (let ((direx-wnd (find-dedicated-direx-window)) (current-dir default-directory))
    (if (not direx-wnd)
         (setq direx-wnd (split-window (frame-root-window) -40 'left)))
    (set-window-dedicated-p direx-wnd nil)
    (select-window direx-wnd)
    (direx:find-directory-reuse current-dir)
    (set-window-dedicated-p direx-wnd t)))
;; Un-pins the dedicated direx window before switching buffer,
;; and pins back again if the buffer is still direx. Uses ido-switch-buffer.
(defun direx-switch-buffer ()
  (interactive)
  (if (direx-window-p)
      (set-window-dedicated-p nil nil))
  (unwind-protect
   (ido-switch-buffer)
   (if (direx:buffer-live-p (window-buffer))
       (set-window-dedicated-p nil t))))
;; Obsolete -- now dired is used for that, see dot-emacs.el
;; (global-set-key (kbd "C-x C-j") 'direx-on-the-left)
;; (global-set-key (kbd "C-x b") 'direx-switch-buffer)

;; Groovy
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\\.\\(groovy\\|gradle\\)\\'" . groovy-mode))

;; Train shell to hide SSO passwords.
(unless use-google-stuff
        (setq comint-password-prompt-regexp
              (concat
               "\\("
               comint-password-prompt-regexp
               "\\)\\|[Ee]nter \\(same \\)?passphrase"
               "\\|SSO password"
               "\\|Enter password"
               "\\|Please enter the pass-phrase to decrypt these private key(s)"
               "\\|Please enter your unix login (kerberos) password:"
               "\\|Password for"))
        (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt))
