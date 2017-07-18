;; Project Android stuff

;; Compile command
(require 'compile)

(setq compile-history
      '(". build/envsetup.sh && lunch angler-eng && USE_GOMA=true chrt -b 0 make -j42 -l10"
        ". build/envsetup.sh && lunch bullhead-eng && USE_GOMA=true chrt -b 0 make -j42 -l10"
        ". build/envsetup.sh && lunch sailfish-eng && USE_GOMA=true chrt -b 0 make -j42 -l10"
        ". build/envsetup.sh && lunch angler-eng && USE_GOMA=true mmm -j42 -l10 frameworks/av"
        ". build/envsetup.sh && lunch bullhead-eng && USE_GOMA=true mmm -j42 -l10 frameworks/av"
        ". build/envsetup.sh && lunch sailfish-eng && USE_GOMA=true mmm -j42 -l10 frameworks/av"
        "ANDROID_HOME=~/Android/Sdk ANDROID_NDK_HOME=~/android-ndk-r11c ./gradlew assembleDebug"))
(setq compile-command
   ". build/envsetup.sh && lunch sailfish-eng && USE_GOMA=true chrt -b 0 make -j42 -l10")

;; Keybindings

(require 'find-things-fast)
(require 'ack)
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
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t)))
(global-set-key [f6] 'ack)
(global-set-key [f7] 'compile)
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

(add-to-list 'auto-mode-alist '("\\.hal$" . c++-mode))
(defun maybe-set-hal-style ()
  (if (and (char-or-string-p buffer-file-name) (string-match "\\.hal$" (downcase buffer-file-name)))
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
            ;; This is to avoid over-indenting of anonymous classes
            (c-set-offset 'substatement-open 0)
            (if (assoc 'inexpr-class c-offsets-alist)
                (c-set-offset 'inexpr-class 0))))

(add-hook 'python-mode-hook
          (lambda()
            (setq python-indent 2)))

(defun open-shell-buffer (buffer-name startup-command)
  (switch-to-buffer (shell buffer-name))
  (set-marker comint-last-output-start (point))
  (insert startup-command)
  (comint-send-input nil t))
(open-shell-buffer "=angler=" "cd ~/code/master && . build/envsetup.sh && lunch angler-eng && export ANDROID_SERIAL=84B0115625000687")
(open-shell-buffer "=bullhead=" "cd ~/code/master && . build/envsetup.sh && lunch bullhead-eng && export ANDROID_SERIAL=0060b1211cc9a54c")
(open-shell-buffer "=sailfish=" "cd ~/code/master && . build/envsetup.sh && lunch sailfish-eng && export ANDROID_SERIAL=HT6540300151")
