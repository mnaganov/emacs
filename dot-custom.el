(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-reuse-buffers t)
 '(company-idle-delay 3)
 '(compilation-scroll-output t)
 '(direx:closed-icon "→")
 '(direx:leaf-icon " ")
 '(direx:open-icon "↓")
 '(eldoc-echo-area-prefer-doc-buffer t)
 '(eldoc-echo-area-use-multiline-p nil)
 '(js2-auto-indent-p nil)
 '(js2-basic-offset 2)
 '(js2-cleanup-whitespace nil)
 '(js2-indent-on-enter-key nil)
 '(js2-mirror-mode nil)
 '(ns-command-modifier 'meta))
;; The following 16-color palette is recommended.
;; It's based on Colorized Light, with most of shades of gray replaced
;; with bright colors to use as backgrounds.
;; [
;;  "#073642",
;;  "#b22222",
;;  "#228b22",
;;  "#b58900",
;;  "#268bd2",
;;  "#8b2252",
;;  "#0087af",
;;  "#eee8d5",
;;  "#002b36",
;;  "#cb4b16",
;;  "#e3ffb3",
;;  "#ffffaf",
;;  "#b3e5ff",
;;  "#3a5fcd",
;;  "#93a1a1",
;;  "#fdf6e3"
;; ]
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-context ((t (:inherit shadow))))
 '(diff-file-header ((t (:foreground "brightblack"))))
 '(diff-header ((t (:background "brightcyan"))))
 '(diff-refine-added ((t (:inherit diff-added :background "brightgreen"))))
 '(diff-refine-removed ((t (:inherit diff-removed :background "brightblue"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "cyan"))))
 '(error ((t (:foreground "red" :weight bold))))
 '(eww-form-text ((t (:foreground "brightblack" :background "brightcyan"))))
 '(font-lock-builtin-face ((t (:foreground "black"))))
 '(font-lock-comment-face ((t (:foreground "red"))))
 '(font-lock-constant-face ((t (:foreground "cyan"))))
 '(font-lock-function-name-face ((t (:foreground "brightblack" :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "brightmagenta" :weight normal))))
 '(font-lock-preprocessor-face ((t (:foreground "brightblack"))))
 '(font-lock-string-face ((t (:foreground "magenta"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(help-key-binding ((t (:bold t))))
 '(hi-yellow ((t (:background "brightyellow"))))
 '(highlight ((t (:background "white"))))
 '(isearch ((t (:background "brightgreen"))))
 '(lazy-highlight ((t (:background "brightyellow"))))
 '(magit-header-line ((t (:inherit header-line :bold t))))
 '(region ((t (:background "brightblue"))))
 '(show-paren-match ((t (:bold t))))
 '(show-paren-mismatch ((t (:background "red"))))
 '(warning ((t (:foreground "brightred" :bold t))))
 '(which-func ((t (:inherit header-line))))
 '(whitespace-tab ((t (:background "red" :foreground "white"))))
 '(widget-field ((t (:background "brightcyan" :foreground "brightwhite")))))
