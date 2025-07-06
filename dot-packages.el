;; == Set up bundled Emacs packages ==

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
;; Since we use "inverse" color scheme (see dot-custom.el), invert the order
;; of grayscale colors which are set via ESC[38;5;⟨n⟩m sequence.
(defun invert-ansi-grayscale (l)
  (let ((color (car l)))
    (if (and (>= color 232) (<= color 255))
        (list (- 487 color)) l)))
(advice-add 'ansi-color--code-as-hex :filter-args 'invert-ansi-grayscale)
;; Use of property faces puts less strain compared to overlays used by default
(setq ansi-color-apply-face-function #'ansi-color-apply-text-property-face)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

;; Whitespace highlighting
(require 'whitespace)
(setq whitespace-style (quote (face trailing tabs empty)))
(setq whitespace-global-modes '(not shell-mode lisp-interaction-mode))
(global-whitespace-mode 1)

;; == Set up external packages ==

;; (byte-recompile-directory (expand-file-name "~/emacs/site-lisp") 0)
(add-to-list 'load-path (concat emacs-root "site-lisp"))
(add-to-list 'load-path (concat emacs-root "site-lisp/chatgpt-shell"))
(add-to-list 'load-path (concat emacs-root "site-lisp/expand-region"))
(add-to-list 'load-path (concat emacs-root "site-lisp/haskell-mode"))
(add-to-list 'load-path (concat emacs-root "site-lisp/rust-mode"))
(setq packages-root (concat emacs-root "site-lisp/"))

;; Swap contents of the buffers
(require 'buffer-move)
;; C-b and C-f are "canonical" move by character shortcuts, however
;; I only use arrows for that, and I do move buffers around a lot.
(global-set-key (kbd "C-b") 'buf-move-left)
(global-set-key (kbd "C-f") 'buf-move-right)

;; == OS-specific setup ==
(if (eq system-type 'windows-nt)
    (load-file (concat emacs-root "dot-windows.el")))
(when (not window-system)
  (require 'term/xterm)
  (unless (fboundp 'xterm-set-window-title)
    (defun xterm-set-window-title (&optional terminal)
      "Set the window title of the Xterm TERMINAL. The title is constructed from `frame-title-format'."
      (unless (display-graphic-p terminal)
        (send-string-to-terminal
         (format "\e]2;%s\a" (format-mode-line frame-title-format))
         terminal))))
  (add-hook 'post-command-hook 'xterm-set-window-title))

(defun is-windows-wsl ()
  (string-match-p (regexp-quote "WINDOWS/system32") (getenv "PATH")))

(when window-system
  ;; enable wheelmouse support by default
  (mwheel-install)
  ;; use extended compound-text coding for X clipboard
  (set-selection-coding-system 'compound-text-with-extensions)
  ;; Fixing Cut and Paste under X
  (setq x-select-enable-clipboard t))

(require 'osc52)

;; Test command to check whether "direct" OSC52 works:
;; printf "\033]52;c;$(printf testOSC52 | base64)\a"
;; Test command to check whether DCS-wrapped OSC52 works:
;; printf "\033P\033]52;c;$(printf testOSC52DCS | base64)\a\033\\"

;; This is a fallback in case the terminal stack does not support OSC52.
;; The usual suspects are the stable release of mosh (1.4.0), and Gnome terminal.
;; In order to enable clipboard exchange, after starting the "xclip server" one
;; must establish a reverse SSH tunnel to the remote machine for the port 3333:
;;   ssh -R 3333:localhost:3333 <remote-host>
(setq xclip-tunnel-port 3333)
(defun xclip-server-start ()
  (interactive)
  (setq xclip-server-process (start-process "xclip-server"
                 nil
                 "bash"
                 "-c"
                 (format "while [ true ]; do nc -l 127.0.0.1 %s | xclip -selection clipboard; done" xclip-tunnel-port))))
(defun remote-xclip-cut-function (text &optional push)
  (let ((temp-file (make-temp-file "clip")))
    ;; Use a temp file to avoid issues with large selections.
    (write-region text nil temp-file nil 0)
    (call-process "bash" nil 0 nil "-c" (format "cat %s > /dev/tcp/127.0.0.1/%s" temp-file xclip-tunnel-port))))

;; If emacs runs as a console program in a terminal or under screen
;; clipboard functions need to be set up in order to interact with
;; external clipboards
(unless window-system
  (cond ((or (getenv "DISPLAY") (is-windows-wsl))
         ;; In this case the host has X running. We assume that emacs runs under screen.
         (defun xsel-cut-function (text &optional push)
           ;; I used to use a temp buffer and `call-process-region` here,
           ;; but for really large selections this could end up with some
           ;; garbage being passed at the end of the text to the clipboard
           ;; program. Writing the selection into the file seems to work
           ;; better.
           (let ((temp-file (make-temp-file "clip")))
             (write-region text nil temp-file nil 0)
             (cond ((eq system-type 'darwin) (call-process "pbcopy" temp-file))
                   ((is-windows-wsl) (call-process "clip.exe" temp-file))
                   (t (call-process "xclip" temp-file 0 nil "-in" "-selection" "clipboard")))))
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
         (setq interprogram-paste-function 'xsel-paste-function)
         (when (getenv "DISPLAY") (xclip-server-start)))
        ((getenv "STY")
         ;; On the host, X is not running, emacs runs under screen remotely.
         ;; We assume that on the client side a graphical terminal is used.
         ;; Also apply the fallback via "xclip server".
         ;; Paste from external clipboard has to be done using the terminal.
         (defun osc52-then-remote-xclip-cut-function (text &optional push)
           (osc52-select-text-dcs text)
           (remote-xclip-cut-function text))
         (setq interprogram-cut-function 'osc52-then-remote-xclip-cut-function))
        ))

;; == Other packages ==

;; Fill-Column-Indicator
(setq-default fill-column 80)
(add-hook 'c-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'c++-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'java-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'js2-mode-hook 'display-fill-column-indicator-mode)

;; Flyspell for comments
(add-hook 'c-mode-hook 'flyspell-prog-mode)
(add-hook 'c++-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'java-mode-hook 'flyspell-prog-mode)
(add-hook 'js2-mode-hook 'flyspell-prog-mode)

;; Expand region: press Alt-Del repeatedly to expand the region
;; starting from inside.
;; I've chosen Alt-Del because on my keyboard, 'Del' is near to '=',
;; so Alt-= runs 'count-words-region', and Alt-Del expands the region.
(require 'expand-region)
(global-set-key (kbd "M-<delete>") 'er/expand-region)

;; Langtool, if present
;; Download from https://languagetool.org/download/LanguageTool-stable.zip
(setq langtool-language-tool-jar "~/LanguageTool-6.1/languagetool-commandline.jar")
(setq langtool-default-language "en-US")
(if (and (or (eq system-type 'gnu/linux) (eq system-type 'darwin)) (file-exists-p langtool-language-tool-jar))
    (require 'langtool))

;; GIT and Mercurial support
(require 'vc-git)
(require 'vc-hg)
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

;; gyp
(require 'gyp)

;; Octave
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; Haskell
(require 'haskell-mode-autoloads)

;; Rust
(require 'rust-mode)

;; Faust
(require 'faust-mode)
(add-to-list 'auto-mode-alist '("\\.dsp$" . faust-mode))

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

;; Shell completion
(require 'native-complete)
(with-eval-after-load 'shell
  (native-complete-setup-bash))

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'gfm-mode-hook 'visual-line-mode)

;; String Inflection: switch between various naming styles.
(require 'string-inflection)
(global-set-key (kbd "C-c C-b") 'string-inflection-all-cycle)
;; C-c C-b in C mode is for sending bug reports.
(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-c C-b") 'string-inflection-all-cycle)))

;; chatgpt-shell
;; (require 'chatgpt-shell)
;; (set-variable chatgpt-shell-openai-key "...")
;; (chatgpt-shell-swap-model)
;; (chatgpt-shell-swap-system-prompt)
