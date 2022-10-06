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
 '(compilation-mode-line-fail ((t (:inherit error))))
 '(compilation-mode-line-run ((t (:foreground "blue" :bold t))))
 '(cua-global-mark ((t (:inherit match))))
 '(cua-rectangle ((t (:inherit region))))
 '(custom-face-tag ((t (:foreground "brightblack"))))
 '(custom-group-tag ((t (:inherit custom-face-tag))))
 '(custom-group-tag1 ((t (:inherit custom-face-tag))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-context ((t (:foreground "brightblack"))))
 '(diff-error ((t (:inherit error))))
 '(diff-file-header ((t (:foreground "black"))))
 '(diff-header ((t (:background "brightcyan"))))
 '(diff-refine-added ((t (:inherit diff-added :background "brightgreen"))))
 '(diff-refine-changed ((t (:inherit diff-changed :bold t))))
 '(diff-refine-removed ((t (:inherit diff-removed :background "brightblue"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "cyan"))))
 '(ediff-current-diff-A ((t (:inherit smerge-upper))))
 '(ediff-current-diff-Ancestor ((t (:inherit smerge-markers))))
 '(ediff-current-diff-B ((t (:inherit smerge-base))))
 '(ediff-current-diff-C ((t (:inherit smerge-lower))))
 '(ediff-even-diff-A ((t (:inherit ediff-current-diff-A))))
 '(ediff-even-diff-Ancestor ((t (:inherit ediff-current-diff-Ancestor))))
 '(ediff-even-diff-B ((t (:inherit ediff-current-diff-B))))
 '(ediff-even-diff-C ((t (:inherit ediff-current-diff-C))))
 '(ediff-fine-diff-A ((t (:inherit ediff-current-diff-A :bold t))))
 '(ediff-fine-diff-Ancestor ((t (:inherit ediff-current-diff-Ancestor :bold t))))
 '(ediff-fine-diff-B ((t (:inherit ediff-current-diff-B :bold t))))
 '(ediff-fine-diff-C ((t (:inherit ediff-current-diff-C :bold t))))
 '(ediff-odd-diff-A ((t (:inherit ediff-current-diff-A))))
 '(ediff-odd-diff-Ancestor ((t (:inherit ediff-current-diff-Ancestor))))
 '(ediff-odd-diff-B ((t (:inherit ediff-current-diff-B))))
 '(ediff-odd-diff-C ((t (:inherit ediff-current-diff-C))))
 '(error ((t (:foreground "red" :bold t))))
 '(eww-form-text ((t (:inherit widget-field))))
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
 '(header-line ((t (:inherit mode-line))))
 '(header-line-highlight ((t (:inherit mode-line-inactive))))
 '(hi-yellow ((t (:background "brightyellow"))))
 '(highlight ((t (:background "white"))))
 '(isearch ((t (:inherit match))))
 '(isearch-fail ((t (:background "brightred" :foreground "brightyellow"))))
 '(lazy-highlight ((t (:background "brightyellow"))))
 '(magit-header-line ((t (:inherit header-line :bold t))))
 '(magit-diff-added ((t (:inherit diff-added))))
 '(magit-diff-added-highlight ((t (:inherit diff-added :background "brightgreen"))))
 '(magit-diff-base ((t (:foreground "yellow"))))
 '(magit-diff-base-highlight ((t (:inherit magit-diff-base :background "brightyellow"))))
 '(magit-diff-context ((t (:inherit diff-context))))
 '(magit-diff-context-highlight ((t (:inherit diff-context :background "white"))))
 '(magit-diff-file-heading ((t (:inherit diff-file-header))))
 '(magit-diff-hunk-heading ((t (:inherit diff-header))))
 '(magit-diff-refine-changed ((t (:inherit diff-changed :bold t))))
 '(magit-diff-removed ((t (:inherit diff-removed))))
 '(magit-diff-removed-highlight ((t (:inherit diff-removed :background "brightblue"))))
 '(match ((t (:background "brightgreen"))))
 '(minibuffer-prompt ((t (:foreground "brightmagenta"))))
 '(mode-line ((t (:foreground "white" :background "black"))))
 '(mode-line-inactive ((t (:foreground "black" :background "white"))))
 '(region ((t (:background "brightcyan"))))
 '(show-paren-match ((t (:bold t))))
 '(show-paren-mismatch ((t (:inherit isearch-fail))))
 '(secondary-selection ((t (:background "brightblue"))))
 '(smerge-base ((t (:background "brightyellow"))))
 '(smerge-lower ((t (:background "brightgreen"))))
 '(smerge-markers ((t (:inherit diff-header))))
 '(smerge-refined-added ((t (:inherit diff-added :bold t))))
 '(smerge-refined-changed ((t (:inherit diff-changed :bold t))))
 '(smerge-refined-removed ((t (:inherit diff-removed :bold t))))
 '(smerge-upper ((t (:background "brightblue"))))
 '(vertical-border ((t (:inherit mode-line-inactive))))
 '(warning ((t (:foreground "brightred" :bold t))))
 '(which-func ((t (:inherit header-line))))
 '(whitespace-big-indent ((t (:inherit whitespace-tab))))
 '(whitespace-tab ((t (:background "brightred" :foreground "white"))))
 '(widget-field ((t (:background "brightcyan" :foreground "black"))))
 '(widget-single-line-field ((t (:background "brightgreen" :foreground "black")))))
