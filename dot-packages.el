;; == Set up bundled Emacs packages ==

;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; fuzzy matching

;; Move between windows using M-Arrows
(require 'windmove)
(windmove-default-keybindings 'meta)

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

;; == Set up external packages ==

(add-to-list 'load-path (concat emacs-root "site-lisp"))
(setq packages-root (concat emacs-root "site-lisp/"))

;; == OS-specific setup ==
(if (eq system-type 'windows-nt)
    (load-file (concat emacs-root "dot-windows.el")))


;; == Other packages ==

;; CSS
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))


;; Haskell
(load (concat packages-root "haskell-mode-2.4/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)


;; Gambit Scheme
(require 'gambit)


;; GIT VC support
(add-to-list 'load-path (concat packages-root "git"))
(require 'git)
(require 'vc-git)
(add-to-list 'vc-handled-backends 'GIT)
(global-auto-revert-mode)
;; Buffer for Git
(shell (generate-new-buffer "=git="))


;; C++ style
(unless use-google-stuff
    (require 'google-c-style)
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent))


;; JS2
(autoload 'js2-mode (format "js2-emacs%d" emacs-major-version) nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (make-local-hook 'after-revert-hook)
            (add-hook 'after-revert-hook 'js2-mode-reset nil t)))
(if use-google-stuff
    (require 'js2-google))


;; MMM
(require 'javascript-mode)
(add-to-list 'load-path (concat packages-root "mmm-mode-0.4.8"))

(defun fix-javascript-mode ()
  "Various personal customizations for javascript-mode."
  (setq c-basic-offset 2)
  (setq fill-column 80)
  (font-lock-mode 1)
  (local-set-key "\C-c\C-c" 'comment-region)
  (local-set-key "\C-c\C-u" 'uncomment-region))

(autoload 'javascript-fix-indentation "javascript-indent")
(add-hook 'javascript-mode-hook 'javascript-fix-indentation)
(add-hook 'javascript-mode-hook 'fix-javascript-mode)

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(add-to-list 'mmm-mode-ext-classes-alist
             '(html-mode "\\.s?html?\\'" html-js))


;; NXML
(add-to-list 'load-path (concat packages-root "nxml-mode"))
(load "rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|gxp\\)\\'" . nxml-mode)
            auto-mode-alist))


;; Yasnippet
(add-to-list 'load-path (concat packages-root "yasnippet-0.5.6"))
(require 'yasnippet)
(setq yas/extra-mode-hooks '(nxml-mode-hook js2-mode-hook))
(yas/initialize)
(yas/load-directory (concat packages-root "yasnippet-0.5.6/snippets"))
