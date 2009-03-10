;; == Google stuff ==

(if use-google-stuff
    (load-file (concat emacs-root "dot-google.el")))


;; == My stuff ==

;; When setting mark, highlight selection
(setq transient-mark-mode t)

;; Paren match highlighting
(show-paren-mode t)

;; Enable CUA key bindings: C-z/x/c/v
(cua-mode t)

;; Auto-scroll in *compilation* buffer
(setq compilation-scroll-output t)

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Disable toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Don't use TAB for indenting
(setq-default indent-tabs-mode nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Instead of 'yes or no' use 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; Show column number in mode line
(column-number-mode 1)

;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
(defvar autosave-dir (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; 80 (100 or whatever) chars warning
(defun font-lock-width-keyword (width)
 "Return a font-lock style keyword for a string beyond width WIDTH
thatuses 'font-lock-warning-face'."
 `((,(format "^%s\\(.+\\)" (make-string width ?.))
    (1 font-lock-warning-face t))))

(font-lock-add-keywords 'c++-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'java-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'html-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'javascript-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'js2-mode (font-lock-width-keyword 80))

;; Start shell and server
(shell)
(server-start)


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


;; == Set up external packages ==

(load-file (concat emacs-root "dot-packages.el"))


;; == Project stuff ==

(if (not (eq use-project nil))
    (load-file (concat emacs-root "project-" use-project ".el")))
