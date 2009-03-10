(add-to-list 'load-path (concat emacs-root "site-lisp"))
(setq packages-root (concat emacs-root "site-lisp/"))

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
