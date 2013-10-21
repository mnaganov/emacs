;; == Google stuff ==

(if use-google-stuff
    (load-file (concat emacs-root "dot-google.el")))


;; == My stuff ==

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(when window-system
  ;; enable wheelmouse support by default
  (mwheel-install)
  ;; use extended compound-text coding for X clipboard
  (set-selection-coding-system 'compound-text-with-extensions)
  ;; Fixing Cut and Paste under X
  (setq x-select-enable-clipboard t))

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
  (when (getenv "DISPLAY")
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
              xsel-output )))
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
  ;; Idea from
  ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
))

;; Paren match highlighting
(show-paren-mode t)

;; Enable CUA key bindings: C-z/x/c/v
(cua-mode t)

;; Auto-scroll in *compilation* buffer
(setq compilation-scroll-output t)

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Disable toolbar and menubar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Disable default "kill emacs" binding. Usually I press it incidentally.
(global-unset-key "\C-x\C-c")

;; Don't use TAB for indenting
(setq-default indent-tabs-mode nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Instead of 'yes or no' use 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; Show column number in mode line
(column-number-mode 1)

;; Set colors for mode line and buffer separators (they are actually inverse)
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "black")

;; Don't unsplit windows on ESC ESC ESC
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (flet ((one-window-p (&optional nomini all-frames) t)) ad-do-it))

;; Rebind buffers list to Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Mute sound
(setq bell-volume 0)
(setq sound-alist nil)

;; Show full path to file in frame title
(setq-default frame-title-format
              (list '((buffer-file-name
                       " %f"
                       (dired-directory
                        dired-directory
                        (revert-buffer-function " %b" ("%b - Dir:  " default-directory)))))))

;; Copy full file path of the buffer to clipboard on C-c f
(defun kill-buffer-file-name ()
  (interactive)
  (kill-new (buffer-file-name)))
(global-set-key (kbd "C-c f") 'kill-buffer-file-name)

;; Copy just file name of the buffer to clipboard on C-C n
(defun kill-buffer-file-name-nondirectory ()
  (interactive)
  (kill-new (file-name-nondirectory (buffer-file-name))))
(global-set-key (kbd "C-c n") 'kill-buffer-file-name-nondirectory)

;; Quit without annoying confirmation
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defun generate-temp-dir-name (name)
  "Generate a name for a temporary dir"
  (if (eq system-type 'windows-nt)
      (concat (getenv "TEMP") "\\" name "\\")
      (if (eq system-type 'darwin)
          (concat "/Users/" (user-login-name) "/." name "/")
          (concat "/tmp/" name "/" (user-login-name) "/"))))

(defun generate-persistent-dir-name (name)
  "Generate a name for a persistent storage dir"
  (if (eq system-type 'windows-nt)
      (concat (getenv "TEMP") "\\" name "\\") ;; FIXME
      (if (eq system-type 'darwin)
          (concat "/Users/" (user-login-name) "/." name "/")
          (concat (getenv "HOME") "/." name "/" system-name "/"))))

;; Save / restore desktop
(setq desktop-path (list (generate-persistent-dir-name "emacs_desktop")))
(make-directory (car desktop-path) t)
(desktop-save-mode t)
(setq desktop-save t)
(setq desktop-restore-eager 10)

;; Set 2x2 windows configuration
(split-window-vertically)
(save-selected-window
 (other-window 1)
 (switch-to-buffer nil)
 (split-window-horizontally)
 (other-window 1)
 (switch-to-buffer nil))
(split-window-horizontally)
(save-selected-window
 (other-window 1)
 (switch-to-buffer nil))

;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
(defvar autosave-dir nil)
(setq autosave-dir (generate-temp-dir-name "emacs_autosaves"))
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
(defvar backup-dir (generate-temp-dir-name "emacs_backups"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Always open .h files in c++-mode.
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; Disable Undo for shell buffers
(add-hook 'shell-mode-hook 'buffer-disable-undo)

;; == Set up packages ==

(load-file (concat emacs-root "dot-packages.el"))


;; == Set up customizations ==

(setq custom-file (concat emacs-root "dot-custom.el"))
(load custom-file)


;; == Start up commands ==
(shell)
(server-start)


;; == Project stuff ==

(if (not (eq use-project nil))
    (load-file (concat emacs-root "project-" use-project ".el")))
