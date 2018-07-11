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

;; If emacs is run in a terminal, clipboard functions have no
;; effect. Instead, use system-specific CLI clipboard interaction.
(unless window-system
  (when (getenv "DISPLAY")
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
  (defun xsel-paste-function ()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output
           (shell-command-to-string (if (eq system-type 'darwin) "pbpaste" "xsel --clipboard --output"))))
      (unless (string= (car kill-ring) xsel-output)
              xsel-output)))
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
))

;; Paren match highlighting
(show-paren-mode t)

;; Enable CUA key bindings: C-z/x/c/v
(cua-mode t)
(unless window-system (global-set-key [f9] 'cua-set-rectangle-mark))

;; Auto-scroll in *compilation* buffer
(setq compilation-scroll-output t)

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Disable toolbar and menubar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Disable default "kill emacs" binding. Usually I press it incidentally.
(global-unset-key "\C-x\C-c")
;; Disable C-PgUp and C-PgDn -- used for tab switching in Chrome.
(global-unset-key [C-next])
(global-unset-key [C-prior])

;; Don't use TAB for indenting
(setq-default indent-tabs-mode nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Instead of 'yes or no' use 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; Show column number in mode line
(column-number-mode 1)

;; Set colors for mode line and buffer separators
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "black")
;; (set-face-foreground 'mode-line-inactive "white")
;; (set-face-background 'mode-line-inactive "black")
;; (set-face-foreground 'vertical-border "white")
;; (set-face-background 'vertical-border "black")
(set-face-foreground 'shadow "brightcyan")
(set-face-background 'match "white")
(set-face-background 'secondary-selection "white")
(set-face-foreground 'header-line "white")
(set-face-background 'header-line "brightblue")
(set-face-underline 'header-line nil)

;; Don't unsplit windows on ESC ESC ESC
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

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
                        (revert-buffer-function " %b" ("%b - Dir: " default-directory)))))))

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
  (cl-flet ((process-list ())) ad-do-it))

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

(if (< (frame-text-width) 300)
    (progn
     ;; Set 2x2 windows configuration
     (split-window-vertically)
     (save-selected-window
      (other-window 1)
      (split-window-horizontally))
     (split-window-horizontally))
    (progn
     ;; Set 3x2 windows configuration with merged center
     (split-window-horizontally)
     (save-selected-window
      (other-window 1)
      (split-window-vertically))
     (split-window-horizontally)
     (split-window-vertically)
     (balance-windows)
     ;; Prevent the center window from being auto splitted on occasion
     (setq split-height-threshold (* (frame-text-height) 2))))

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

;; Track current dir in shell via procfs on Linux

(defun shell-procfs-dirtrack (str)
  (prog1 str
         (when (string-match comint-prompt-regexp str)
               (let ((directory (file-symlink-p
                                 (format "/proc/%s/cwd"
                                         (process-id
                                          (get-buffer-process
                                           (current-buffer)))))))
                 (when (file-directory-p directory)
                       (cd directory))))))

(define-minor-mode shell-procfs-dirtrack-mode
  "Track shell directory by inspecting procfs."
  nil nil nil
  (cond (shell-procfs-dirtrack-mode
         (when (bound-and-true-p shell-dirtrack-mode)
               (shell-dirtrack-mode 0))
         (when (bound-and-true-p dirtrack-mode)
               (dirtrack-mode 0))
         (add-hook 'comint-preoutput-filter-functions
                   'shell-procfs-dirtrack nil t))
        (t
         (remove-hook 'comint-preoutput-filter-functions
                      'shell-procfs-dirtrack t))))

(if (eq system-type 'gnu/linux)
   (add-hook 'shell-mode-hook 'shell-procfs-dirtrack-mode))

;; Set fill column for the scratch buffer to a large value so it can be
;; used for joining columns of text into a single line by executing
;; `fill-region' (M-Q) on the selection.
;; We identify the *scratch* buffer via its mode.
(add-hook 'lisp-interaction-mode-hook (lambda()
                                        (setq fill-column 10000)))

;; == Set up packages ==

(load-file (concat emacs-root "dot-packages.el"))

;; Use dired as a dedicated "project tree window"
(defun dired-buffer-live-p (buffer)
  (and (buffer-live-p buffer)
       (eq (buffer-local-value 'major-mode buffer) 'dired-mode)))
(defun dired-window-p (&optional w)
  (and (window-dedicated-p w)
       (dired-buffer-live-p (window-buffer w))))
(defun find-dedicated-dired-window ()
  (let ((dedicated nil))
    (walk-windows (function (lambda (w)
                              (if (dired-window-p w)
                                  (setq dedicated w)))))
    dedicated))
;; Opens directories list on the left side of the frame
;; To close it, either use C-x 0, or C-c left-arrow.
(defun dired-on-the-left ()
  (interactive)
  (let ((dired-wnd (find-dedicated-dired-window)) (current-dir default-directory))
    (if (not dired-wnd)
        (setq dired-wnd (split-window (frame-root-window) -40 'left)))
    (set-window-dedicated-p dired-wnd nil)
    (select-window dired-wnd)
    (let ((already-opened (dired-buffers-for-dir current-dir)))
      (if (not already-opened)
          (progn
           (switch-to-buffer (dired-noselect current-dir "-lR"))
           (dired-hide-details-mode)
           (dired-hide-all)
           (auto-revert-mode))
          (switch-to-buffer (car already-opened))))
    (set-window-dedicated-p dired-wnd t)
    ;; Apply dired-shorten-directory-names hook
    (revert-buffer)))
;; Un-pins the dedicated dired window before switching buffer,
;; and pins back again if the buffer is still dired. Uses ido-switch-buffer.
(defun dired-switch-buffer ()
  (interactive)
  (let ((restore-dedicated nil))
    (when (dired-window-p)
          (set-window-dedicated-p nil nil)
          (setq restore-dedicated t))
    (unwind-protect
     (ido-switch-buffer)
     (if (and restore-dedicated (dired-buffer-live-p (window-buffer)))
         (set-window-dedicated-p nil t)))))
(defun find-first-dired-dir-name ()
  (save-excursion
   (goto-char (point-min))
   (if (re-search-forward dired-subdir-regexp nil t)
       (match-string 1)
       nil)))
(defun dired-shorten-directory-names ()
  (if (dired-window-p)
      (let ((dir-name (find-first-dired-dir-name)))
        (if dir-name
            (while (re-search-forward (concat "^. " dir-name) nil t)
                   (replace-match "  ."))))))
(add-hook 'dired-after-readin-hook 'dired-shorten-directory-names)
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(global-set-key (kbd "C-x C-j") 'dired-on-the-left)
(global-set-key (kbd "C-x b") 'dired-switch-buffer)

;; == Set up customizations ==

(setq custom-file (concat emacs-root "dot-custom.el"))
(load custom-file)


;; == Start up commands ==
(shell)
(server-start)


;; == Project stuff ==

(if (not (eq use-project nil))
    (load-file (concat emacs-root "project-" use-project ".el")))
