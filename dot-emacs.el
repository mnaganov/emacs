;; == My stuff ==

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Use C-h as delete instead of invoking help. For help, use M-x help-command
(global-set-key "\C-h" 'delete-backward-char)

;; I love to use diff mode and use it more often than macros,
;; but it's a disaster when it's applied to a wrong buffer.
(defun my-switch-fundamental-to-diff-mode ()
  (interactive
   (if (eq major-mode 'fundamental-mode)
       (diff-mode))))
(global-set-key [f3] 'my-switch-fundamental-to-diff-mode)

;; Reminder:
;; C-x 8 _ m inserts em dash,
;; C-x 8 _ n inserts en dash
;; C-x 8 SPC inserts nbsp
;; To see other shortcuts, type C-x 8 C-h

;; Turn off bell
(setq ring-bell-function 'ignore)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Paren match highlighting
(show-paren-mode t)

;; Enable CUA key bindings: C-z/x/c/v
(cua-mode t)
(global-set-key [f9] 'cua-set-rectangle-mark)

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Disable toolbar, menubar, and tooltips
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Disable default "kill emacs" binding. Usually I press it incidentally.
(global-unset-key "\C-x\C-c")
;; Disable C-PgUp and C-PgDn -- used for tab switching in Chrome.
(global-unset-key [C-next])
(global-unset-key [C-prior])

;; Don't use TAB for indenting
(setq-default indent-tabs-mode nil)

;; Highlight current line
(global-hl-line-mode 1)
;; But turn it off for compile and comint buffers since it can be slow
;; for very long lines. Also turn off undo.
(defun my-turn-off-slow-funcs ()
  (set (make-local-variable 'global-hl-line-mode) nil)
  (buffer-disable-undo))
(require 'compile)
(add-hook 'compilation-mode-hook 'my-turn-off-slow-funcs)
(require 'comint)
(add-hook 'comint-mode-hook 'my-turn-off-slow-funcs)

;; Instead of 'yes or no' use 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; Show column number in mode line
(column-number-mode 1)

;; Disable bidirectional editing to speed up lines handling
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Don't unsplit windows on ESC ESC ESC
(defun my-around-keyboard-escape-quit (orig-fun &rest args)
  "Prevent keyboard-escape-quit from deleting other windows.
Advises `keyboard-escape-quit` around."
  (let ((orig-one-window-p-fn (symbol-function 'one-window-p)))
    (cl-letf (((symbol-function 'one-window-p) (lambda (&optional nomini all-frames) t)))
      (apply orig-fun args))))

(advice-add 'keyboard-escape-quit :around #'my-around-keyboard-escape-quit)

;; Rebind buffers list to Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Mute sound
(setq bell-volume 0)
(setq sound-alist nil)

;; Frame title: <compact host name>:<one-of full-file-path dired-directory buffer-name>
(setq system-name-for-frame-title (when (string-match (concat (user-login-name) "[.-]?") (system-name))
                                    (replace-match "" nil nil (system-name))))
;; (symbol then else) shows <then> when <symbol> isn't empty, otherwise <else>
(setq frame-title-format (list
                          "" system-name-for-frame-title ":"
                          '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Exit search normally, save the `search-string' on kill-ring.
(defun my-isearch-kill-and-exit ()
  (interactive)
  (isearch-done)
  (isearch-clean-overlays)
  (kill-new isearch-string))
(define-key isearch-mode-map (kbd "M-w") 'my-isearch-kill-and-exit)
;; Avoid using C-w in isearch for SSH-in-a-tab compatibility. Use C-d.
(define-key isearch-mode-map (kbd "C-d") 'isearch-yank-word-or-char)

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
(defun my-around-save-buffers-kill-emacs (orig-fun &rest args)
  "Prevent "Active processes exist" query when quitting Emacs.
Advises `save-buffers-kill-emacs` around."
  (cl-letf (((symbol-function 'process-list) (lambda () nil)))
    (apply orig-fun args)))

(advice-add 'save-buffers-kill-emacs :around #'my-around-save-buffers-kill-emacs)

(defun my-generate-emacs-data-dir-name (base-name storage-type)
  "Generate a name for an Emacs data directory for BASE-NAME.
STORAGE-TYPE can be 'temp or 'persistent."
  (let ((user (user-login-name)))
    (cond
     ((eq system-type 'windows-nt)
      ;; For Windows, both temp and persistent (with FIXME) currently use TEMP
      (concat (getenv "TEMP") "\\" base-name "\\"))
     ((eq system-type 'darwin)
      ;; macOS uses ~/.<base-name>/ for both, effectively persistent
      (concat "/Users/" user "/." base-name "/"))
     (t
      ;; Linux/other Unix-like
      (if (eq storage-type 'temp)
          (concat "/tmp/" base-name "/" user "/")
        ;; Persistent for Linux/other
        (concat (getenv "HOME") "/." base-name "/" system-name "/"))))))

(defun generate-temp-dir-name (name)
  "Generate a name for a temporary dir using NAME as base."
  (my-generate-emacs-data-dir-name name 'temp))

(defun generate-persistent-dir-name (name)
  "Generate a name for a persistent storage dir using NAME as base."
  (my-generate-emacs-data-dir-name name 'persistent))

(defun calculate-left-window-width (center-width)
  "Calculate the width of the left window, based on the width of the center window."
  ;; The left and right windows are 5/9 and 4/9.
  (/ (* (- (frame-text-width) center-width) 5) 9))

;; Automatically set up a 2- or 3-column window layout at startup
;; if running in text mode and the frame is wide enough.
(unless (or (display-graphic-p) (< (frame-text-width) 160)) ; Only apply if not graphical and frame width >= 160
  (if (< (frame-text-width) 235)
      ;; If frame width is between 160 and 234 columns, split into two vertical windows.
      (split-window-vertically)
    ;; If frame width is 235 columns or more, attempt a 3-column layout.
    (let* ((initial-center-width 104) ; Preferred initial width for the central window
           (calculated-left-width (calculate-left-window-width initial-center-width))
           ;; If the calculated left width is very large (>120), it implies a very wide screen.
           ;; In such cases, increase the center window's width for better balance.
           (final-center-width (if (> calculated-left-width 120)
                                   120
                                 initial-center-width))
           (final-left-width (if (> calculated-left-width 120)
                                 (calculate-left-window-width final-center-width)
                               calculated-left-width)))
      ;; Create 3 vertical windows.
      ;; The `calculate-left-window-width` function aims for a 5/9 ratio for the left window
      ;; and implicitly 4/9 for the right window, relative to the space remaining after
      ;; the center window is allocated.
      (split-window-horizontally final-left-width)
      (other-window 1) ; Move to the newly created right window
      (split-window-horizontally final-center-width) ; Split it, creating the center and final right window
      ;; Configure splitting behavior for the center window (which is now current)
      ;; Prevent the center window from being auto-splitted vertically too easily by new operations.
      (setq split-height-threshold (* (frame-text-height) 2))
      ;; Allow the center window to be split horizontally if needed by other operations.
      (setq split-width-threshold nil)
      ;; The rightmost window (which was the result of the second split)
      ;; is further split vertically in half.
      (save-selected-window
       (other-window 1) ; Move to the rightmost window
       (split-window-vertically)))))

;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
(defvar autosave-dir nil)
(setq autosave-dir (generate-temp-dir-name "emacs_autosaves"))
(defun make-autosave-dir ()
  (make-directory autosave-dir t))
(make-autosave-dir)
(run-with-timer (* 60 60 24) (* 60 60 24) 'make-autosave-dir)

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

;; Minibuffer completion
(fido-mode t)

(defun open-shell-buffer (buffer-name startup-cmd)
  (save-selected-window
    (with-current-buffer (shell buffer-name)
      (set-marker comint-last-output-start (point))
      (funcall startup-cmd)
      (comint-send-input nil t))))

(defvar my-cs-command-config nil "Alist expected to be set by project files. ((COMMAND . DEFAULT_LENGTH) . HISTORY_SYM)")
(define-compilation-mode my-cs-mode "CS"
  "CodeSearch compilation mode."
  (setq-local compilation-error-face compilation-info-face))

(defun my-cs-generalized (command-args)
  (interactive
   (unless my-cs-command-config
     (error "my-cs-command-config is not set for this project"))
   (list (read-shell-command "Run cs (like this): "
                             (car my-cs-command-config) ; (COMMAND . DEFAULT_LENGTH)
                             (cdr my-cs-command-config)))) ; HISTORY_SYM
  (compilation-start command-args 'my-cs-mode))

;; == Set up packages ==

(load-file (concat emacs-root "dot-packages.el"))

(require 'dired)
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
;; To expand/collapse a directory, use '$' (dired-hide-subdir)
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
           (dired-hide-details-mode 1)
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
              (replace-match "  ·")
              (while (and (re-search-forward "[^/:]*\\(/\\|:\\)" nil t)
                          (seq-contains (match-string-no-properties 0) ?/))
                (replace-match "·")))))))
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
(save-selected-window
  (shell))
(server-start)


;; == Project stuff ==

(if (not (eq use-project nil))
    (load-file (concat emacs-root "project-" use-project ".el")))

;; == Google stuff ==

(if use-google-stuff
    (load-file (concat emacs-root "dot-google.el")))
