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
 '(ns-command-modifier (quote meta)))
;; The following 16-color palette is recommended.
;; It's based on Colorized Light, with most of shades of gray replaced
;; with bright colors to use as backgrounds.
;; Terminal background: #fdf6e3 (same as brightwhite)
;;          foreground: #657b83
;; [
;;  "#073642",
;;  "#b22222",
;;  "#228b22",
;;  "#b58900",
;;  "#268bd2",
;;  "#8b2252",
;;  "#0087af",
;;  "#eee8d5",
;;  "#93a1a1",
;;  "#cb4b16",
;;  "#e3ffb3",
;;  "#ffffaf",
;;  "#e5f6ff",
;;  "#3a5fcd",
;;  "#ebecec",
;;  "#fdf6e3"
;; ]
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-mode-line-fail ((t (:foreground "red" :bold t))))
 '(compilation-mode-line-run ((t (:foreground "brightmagenta" :bold t))))
 '(cua-global-mark ((t (:background "brightgreen"))))
 '(cua-rectangle ((t (:background "brightcyan"))))
 '(custom-face-tag ((t (:foreground "brightblack"))))
 '(custom-group-tag ((t (:foreground "brightblack"))))
 '(custom-group-tag1 ((t (:foreground "brightblack"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-context ((t (:foreground "brightblack"))))
 '(diff-file-header ((t (:foreground "black"))))
 '(diff-header ((t (:background "brightcyan"))))
 '(diff-refine-added ((t (:inherit diff-added :background "brightgreen"))))
 '(diff-refine-removed ((t (:inherit diff-removed :background "brightblue"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "cyan"))))
 '(error ((t (:foreground "red" :weight bold))))
 '(eww-form-text ((t (:foreground "black" :background "brightcyan"))))
 '(font-lock-builtin-face ((t (:foreground "black"))))
 '(font-lock-comment-face ((t (:foreground "red"))))
 '(font-lock-constant-face ((t (:foreground "cyan"))))
 '(font-lock-function-name-face ((t (:bold t))))
 '(font-lock-keyword-face ((t (:foreground "brightmagenta"))))
 '(font-lock-preprocessor-face ((t (:foreground "black"))))
 '(font-lock-string-face ((t (:foreground "magenta"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(font-lock-warning-face ((t (:inherit warning))))
 '(help-key-binding ((t (:bold t))))
 '(hi-yellow ((t (:background "brightyellow"))))
 '(highlight ((t (:background "white"))))
 '(isearch ((t (:background "brightgreen"))))
 '(isearch-fail ((t (:background "brightred" :foreground "brightwhite"))))
 '(lazy-highlight ((t (:background "brightyellow"))))
 '(magit-header-line ((t (:inherit header-line :bold t))))
 '(match ((t (:background "brightyellow"))))
 '(minibuffer-prompt ((t (:foreground "brightmagenta"))))
 '(region ((t (:background "brightcyan"))))
 '(show-paren-match ((t (:bold t))))
 '(show-paren-mismatch ((t (:background "red"))))
 '(smerge-base ((t (:background "brightyellow"))))
 '(smerge-lower ((t (:background "brightgreen"))))
 '(smerge-markers ((t (:background "brightcyan"))))
 '(smerge-upper ((t (:background "brightblue"))))
 '(warning ((t (:foreground "brightred" :bold t))))
 '(which-func ((t (:inherit header-line))))
 '(whitespace-big-indent ((t (:inherit whitespace-tab))))
 '(whitespace-tab ((t (:background "brightred" :foreground "white"))))
 '(widget-field ((t (:background "brightcyan" :foreground "black"))))
 '(widget-single-line-field ((t (:background "brightgreen" :foreground "black")))))
