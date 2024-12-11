;; Project Android stuff

(setq compile-history
      '(". build/envsetup.sh && lunch aosp_cf_x86_64_only_phone-trunk_staging-userdebug && . build/make/rbesetup.sh && SOONG_GEN_COMPDB=1 SOONG_LINK_COMPDB_TO=$ANDROID_BUILD_TOP m"
        ". build/envsetup.sh && lunch aosp_cf_x86_64_only_phone-trunk_staging-userdebug && . build/make/rbesetup.sh && SOONG_GEN_COMPDB=1 SOONG_LINK_COMPDB_TO=$ANDROID_BUILD_TOP ROLLING_TF_SUBPROCESS_OUTPUT=0 atest VtsHalAudioCoreTargetTest"
        ". build/envsetup.sh && lunch tokay-trunk_staging-userdebug && . build/make/rbesetup.sh && SOONG_GEN_COMPDB=1 SOONG_LINK_COMPDB_TO=$ANDROID_BUILD_TOP m"
        ". build/envsetup.sh && lunch tokay-trunk_staging-userdebug && . build/make/rbesetup.sh && SOONG_GEN_COMPDB=1 SOONG_LINK_COMPDB_TO=$ANDROID_BUILD_TOP ANDROID_SERIAL=3B141FDAQ000DJ ROLLING_TF_SUBPROCESS_OUTPUT=0 atest CtsMediaTestCases:AudioTrackTest"
        "ANDROID_HOME=~/Android ANDROID_NDK_HOME=~/android-ndk-r22b ./gradlew assembleDebug"))
(setq compile-command
   ". build/envsetup.sh && lunch tokay-trunk_staging-userdebug && . build/make/rbesetup.sh && SOONG_GEN_COMPDB=1 SOONG_LINK_COMPDB_TO=$ANDROID_BUILD_TOP m")

(custom-set-variables
 '(google-use-coding-style nil))

;; Keybindings

(require 'find-things-fast)
(require 'ag)
(defvar my-cs-command '("cs --corpus=android   | sed -n -e 's/^android\\///p'" .  21)
  "The command to be run by the cs function.")
(define-compilation-mode my-cs-mode "CS"
  "CodeSearch compilation mode."
  (setq-local compilation-error-face
              compilation-info-face))
(defun my-cs (command-args)
  (interactive
   (list (read-shell-command "Run cs (like this): "
                             my-cs-command
                             'my-cs-history)))
  (compilation-start command-args 'my-cs-mode))

(global-set-key [f1] 'ftf-find-file)
(global-set-key [f2] 'my-cs)
(global-set-key [f4] 'eglot-rename)
(global-set-key [f5] (lambda () (interactive) (revert-buffer nil t)))
(global-set-key [f6] 'ag-project-regexp)
;; Enable comint as 'atest' sometimes is asking questions
(defun my-compile-with-comint ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'compile))
(global-set-key [f7] 'my-compile-with-comint)
(global-set-key [f8] 'recompile)

(defun set-android-coding-style ()
   (setq c-basic-offset 4)
   (setq fill-column 100)
   (setq c-continued-statement-offset 8)
   (c-set-offset 'statement-cont '++)
   (c-set-offset 'arglist-intro '++)
   (c-set-offset 'arglist-cont-nonempty '++))

;; Smarter find-other-file that can search in the entire git repo.
;; Great for dealing with Android irregularities in files placement.
(require 'cff)
(setq cff-use-helm-choice nil)
(setq cff-interface-dirs '("include" "interface"))
(setq cff-interface-regexps '(("^I.*\\.h$" . (lambda (base)
                                               (concat "I" (concat base ".h"))))
                              ("^I.*\\.hpp$" (lambda (base)
                                               (concat "I" (concat base ".hpp"))))))

(add-to-list 'auto-mode-alist '("\\.bp$" . js-mode))

(add-to-list 'auto-mode-alist '("\\.aidl$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.hal$" . c++-mode))
(defun maybe-set-hal-style ()
  (if (and (char-or-string-p buffer-file-name)
           (or (string-match "\\.aidl$" (downcase buffer-file-name))
               (string-match "\\.hal$" (downcase buffer-file-name))))
      (setq fill-column 80)))

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o") 'cff-find-other-file)
            (setq tags-revert-without-query t)
            (google-set-c-style)
            (set-android-coding-style)
            (maybe-set-hal-style)))

(add-hook 'java-mode-hook
          (lambda()
            (set-android-coding-style)
            (maybe-set-hal-style)
            ;; This is to avoid over-indenting of anonymous classes
            (c-set-offset 'substatement-open 0)
            (if (assoc 'inexpr-class c-offsets-alist)
                (c-set-offset 'inexpr-class 0))))

(add-hook 'python-mode-hook
          (lambda()
            (setq python-indent 4)))

;; A quick and dirty solution for supporting `ESC [ F` (cursor to previous line)
;; ANSI escape sequence. It is important to insert this *before* ansi-color-process-output
;; because the latter strips out all CSI sequences.
(defun ansi-csi-cursor-movement (_ignored)
  (let ((start-marker (if (and (markerp comint-last-output-start)
                               (eq (marker-buffer comint-last-output-start)
                                   (current-buffer))
                               (marker-position comint-last-output-start))
                          comint-last-output-start
                        (point-min-marker)))
        (end-marker (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char start-marker)
      ;; Find the next escape sequence.
      (while (re-search-forward ansi-color-control-seq-regexp end-marker t)
        ;; Extract escape sequence.
        (let ((esc-beg (match-beginning 0))
              (esc-end (point)))
          (when (eq (char-before esc-end) ?F)
            (goto-char (line-beginning-position))
            (previous-logical-line)
            (delete-region (point) esc-end))
          )))))
(add-hook 'comint-output-filter-functions 'ansi-csi-cursor-movement)

;; Special handling for logcat files.
(require 'logview)
(defun logcat-no-undo-and-read-only-hook ()
  (when (s-suffix? "logcat" buffer-file-name)
    (buffer-disable-undo)
    (read-only-mode 1)
    (view-mode)
    (logview-mode)
    (logview-choose-submode "Android" "Android")))
(add-hook 'find-file-hook 'logcat-no-undo-and-read-only-hook)

(defun open-shell-buffer (buffer-name startup-cmd)
  (save-selected-window
    (with-current-buffer (shell buffer-name)
      (set-marker comint-last-output-start (point))
      (funcall startup-cmd)
      (comint-send-input nil t))))
(open-shell-buffer "=tokay=" (lambda() (insert-file-contents "~/screen/tokay.cfg" nil)))
(open-shell-buffer "=husky=" (lambda() (insert-file-contents "~/screen/husky.cfg" nil)))
(open-shell-buffer "=raven=" (lambda() (insert-file-contents "~/screen/raven.cfg" nil)))
(open-shell-buffer "=cuttlefish=" (lambda() (insert-file-contents "~/screen/cuttlefish.cfg" nil)))
(open-shell-buffer "=aosp=" (lambda() (insert-file-contents "~/screen/aosp.cfg" nil)))
;; (open-shell-buffer "=toolbox=" (lambda() (insert (concat "cd ~/code/master && "
;;                                        ". build/envsetup.sh && lunch tokay-trunk_staging-userdebug && "
;;                                        "export ANDROID_SERIAL=3B141FDAQ000DJ "
;;                                        "ANDROID_HOME=~/Android ANDROID_NDK_HOME=~/android-ndk-r22d"))))
