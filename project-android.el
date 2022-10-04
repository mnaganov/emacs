;; Project Android stuff

(setq compile-history
      '(". build/envsetup.sh && lunch cf_x86_64_phone-userdebug && . build/make/rbesetup.sh && SOONG_GEN_COMPDB=1 SOONG_LINK_COMPDB_TO=$ANDROID_BUILD_TOP m"
        ". build/envsetup.sh && lunch cf_x86_64_phone-userdebug && . build/make/rbesetup.sh && SOONG_GEN_COMPDB=1 SOONG_LINK_COMPDB_TO=$ANDROID_BUILD_TOP atest VtsHalAudioCoreTargetTest"
        ". build/envsetup.sh && lunch redfin-userdebug && . build/make/rbesetup.sh && SOONG_GEN_COMPDB=1 SOONG_LINK_COMPDB_TO=$ANDROID_BUILD_TOP m"
        ". build/envsetup.sh && lunch redfin-userdebug && . build/make/rbesetup.sh && SOONG_GEN_COMPDB=1 SOONG_LINK_COMPDB_TO=$ANDROID_BUILD_TOP ANDROID_SERIAL=06131FDD4000HP atest CtsMediaTestCases:AudioTrackTest"
        "ANDROID_HOME=~/Android ANDROID_NDK_HOME=~/android-ndk-r22b ./gradlew assembleDebug"))
(setq compile-command
   ". build/envsetup.sh && lunch redfin-userdebug && . build/make/rbesetup.sh && SOONG_GEN_COMPDB=1 SOONG_LINK_COMPDB_TO=$ANDROID_BUILD_TOP m")

(custom-set-variables
 '(google-use-coding-style nil))

;; Keybindings

(require 'find-things-fast)
(require 'ag)
(defvar my-cs-command '("cs --corpus=android   | sed -n -e 's/^android\\///p'" .  21)
  "The command to be run by the cs function.")
(define-compilation-mode my-cs-mode "CS"
  "CodeSearch compilation mode."
  nil)
(defun my-cs (command-args)
  (interactive
   (list (read-shell-command "Run cs (like this): "
                             my-cs-command
                             'my-cs-history)))
  (compilation-start command-args 'my-cs-mode))

(global-set-key [f1] 'ftf-find-file)
(global-set-key [f2] 'my-cs)
(global-set-key [f4] 'eglot-rename)
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t)))
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

;; Support ANSI control sequences in the compilation buffer
;; but only for the actual compilation (not grep etc).
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (when (bound-and-true-p compilation-shell-minor-mode)
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun open-shell-buffer (buffer-name startup-cmd)
  (save-selected-window
    (with-current-buffer (shell buffer-name)
      (set-marker comint-last-output-start (point))
      (funcall startup-cmd)
      (comint-send-input nil t))))
(open-shell-buffer "=sargo=" (lambda() (insert-file-contents "~/screen/sargo.cfg" nil)))
(open-shell-buffer "=bramble=" (lambda() (insert-file-contents "~/screen/bramble.cfg" nil)))
(open-shell-buffer "=redfin=" (lambda() (insert-file-contents "~/screen/redfin.cfg" nil)))
(open-shell-buffer "=raven=" (lambda() (insert-file-contents "~/screen/raven.cfg" nil)))
(open-shell-buffer "=cuttlefish=" (lambda() (insert-file-contents "~/screen/cuttlefish.cfg" nil)))
(open-shell-buffer "=aosp=" (lambda() (insert-file-contents "~/screen/aosp.cfg" nil)))
(open-shell-buffer "=toolbox=" (lambda() (insert (concat "cd ~/code/master && "
                                       ". build/envsetup.sh && lunch bramble-userdebug && "
                                       "export ANDROID_SERIAL=02151FQC200016 "
                                       "ANDROID_HOME=~/Android ANDROID_NDK_HOME=~/android-ndk-r22d"))))
