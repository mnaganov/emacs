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
(add-hook 'shell-mode-hook (lambda () (whitespace-mode 0)))
(add-hook 'lisp-interaction-mode-hook (lambda () (whitespace-mode 0)))

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


(when window-system
  ;; enable wheelmouse support by default
  (mwheel-install)
  ;; use extended compound-text coding for X clipboard
  (set-selection-coding-system 'compound-text-with-extensions)
  ;; Fixing Cut and Paste under X
  (setq x-select-enable-clipboard t))

(require 'osc52)

;; If emacs runs as a console program in a terminal or under screen
;; clipboard functions need to be set up in order to interact with
;; external clipboards
(unless window-system
  (cond ((getenv "DISPLAY")
         ;; In this case the host has X running. We assume that emacs runs under screen.
         (defun xsel-cut-function (text &optional push)
           ;; I used to use a temp buffer and `call-process-region` here,
           ;; but for really large selections this could end up with some
           ;; garbage being passed at the end of the text to the clipboard
           ;; program. Writing the selection into the file seems to work
           ;; better.
           (let ((temp-file (make-temp-file "clip")))
             (write-region text nil temp-file nil 0)
             (if (eq system-type 'darwin)
                 (call-process "pbcopy" temp-file)
               (call-process "xclip" temp-file 0 nil "-in" "-selection" "clipboard"))))
         (defun osc52-then-xsel-cut-function (text &optional push)
           ;; For remote sessions, this allows using remote's clipboard for pasting.
           ;; Otherwise, after copying from remote via OSC52 one has to paste using terminal.
           (osc52-select-text-dcs text)
           (xsel-cut-function text))
         (defun xsel-paste-function ()
           ;; Find out what is current selection by xsel. If it is different
           ;; from the top of the kill-ring (car kill-ring), then return
           ;; it. Else, nil is returned, so whatever is in the top of the
           ;; kill-ring will be used.
           (let ((xsel-output
                  (shell-command-to-string (if (eq system-type 'darwin) "pbpaste" "xsel --clipboard --output"))))
             (unless (string= (car kill-ring) xsel-output)
               xsel-output)))
         (setq interprogram-cut-function 'osc52-then-xsel-cut-function)
         (setq interprogram-paste-function 'xsel-paste-function))
        ((getenv "STY")
         ;; On the host, X is not running, emacs runs under screen remotely.
         ;; We assume that on the client side a graphical terminal is used.
         ;; Paste from external clipboard has to be done using the terminal.
         (setq interprogram-cut-function 'osc52-select-text-dcs))
        ))

;; == Other packages ==

;; Fill-Column-Indicator
(setq-default fill-column 80)
(require 'fill-column-indicator)
(add-hook 'c++-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'java-mode-hook 'fci-mode)
(add-hook 'js2-mode-hook 'fci-mode)

;; Flyspell for comments
(add-hook 'c++-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'java-mode-hook 'flyspell-prog-mode)
(add-hook 'js2-mode-hook 'flyspell-prog-mode)

;; GIT support
(require 'vc-git)
(global-auto-revert-mode)

;; C++ style
(defun c-style-comment-dwim (&optional arg)
  "Call `comment-dwim' with comments style override to C."
  (interactive "*P")
  (let ((comment-start "/*") (comment-padding "") (comment-end "*/"))
    (call-interactively 'comment-dwim)))

(unless use-google-stuff
    (require 'google-c-style)
    (unless (or (string-match "chrome" use-project) (string-match "v8" use-project) (string-match "android" use-project))
            (add-hook 'c-mode-common-hook 'google-set-c-style))
    (add-hook 'c-mode-common-hook 'google-make-newline-indent))
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "M-*") 'c-style-comment-dwim)))

;; Fix indentation for Java annotations
(add-hook 'java-mode-hook
          '(lambda ()
             "Treat Java 1.5 @-style annotations as comments."
             (setq c-comment-start-regexp
                   "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
             (modify-syntax-entry ?@ "< b"
                                  java-mode-syntax-table)))

;; gyp
(require 'gyp)

;; Octave
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; Faust
(require 'faust-mode)
(add-to-list 'auto-mode-alist '("\\.dsp$" . faust-mode))

;; flx-ido
(require 'flx-ido)
(flx-ido-mode t)
(setq flx-ido-use-faces nil)
(setq gc-cons-threshold 20000000)

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

;; which function
(which-function-mode 1)
(setq mode-line-misc-info
      (delete (assoc 'which-function-mode mode-line-misc-info) mode-line-misc-info)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (setq mode-line-misc-info
          (delete (assoc 'which-function-mode mode-line-misc-info) mode-line-misc-info)
          header-line-format which-func-header-line-format)))
