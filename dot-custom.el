(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-reuse-buffers t)
 '(company-idle-delay 3)
 '(compilation-scroll-output t)
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
;; Terminal background: #fdf6e4 (same as brightwhite)
;;          foreground: #657b83
;;          cursor:     #b1b5d8 (brightmagenta, 40% opacity)
;; [
;;  "#043642",
;;  "#b12621",
;;  "#1f892b",
;;  "#b68900",
;;  "#268bd2",
;;  "#94245c",
;;  "#249482",
;;  "#eee8d6",
;;  "#93a1a1",
;;  "#c7692b",
;;  "#eef9c2",
;;  "#ffeac7",
;;  "#dfe3ec",
;;  "#3657cb",
;;  "#ddf3ed",
;;  "#fdf6e4"
;; ]
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-bright-black ((t (:inherit ansi-color-black :bold t))))
 '(ansi-color-bright-blue ((t (:inherit ansi-color-blue :bold t))))
 '(ansi-color-bright-cyan ((t (:inherit ansi-color-cyan :bold t))))
 '(ansi-color-bright-green ((t (:inherit ansi-color-green :bold t))))
 '(ansi-color-bright-magenta ((t (:inherit ansi-color-magenta :bold t))))
 '(ansi-color-bright-red ((t (:inherit ansi-color-red :bold t))))
 '(ansi-color-bright-white ((t (:inherit ansi-color-white :bold t))))
 '(ansi-color-bright-yellow ((t (:inherit ansi-color-yellow :bold t))))
 '(compilation-mode-line-fail ((t (:inherit error))))
 '(compilation-mode-line-run ((t (:foreground "blue" :bold t))))
 '(completions-common-part ((t (:foreground "blue"))))
 '(cua-global-mark ((t (:inherit match))))
 '(cua-rectangle ((t (:inherit region))))
 '(custom-face-tag ((t (:foreground "brightblack"))))
 '(custom-group-tag ((t (:inherit custom-face-tag))))
 '(custom-group-tag1 ((t (:inherit custom-face-tag))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-context ((t (:foreground "brightblack"))))
 '(diff-error ((t (:inherit error))))
 '(diff-file-header ((t (:foreground "black"))))
 '(diff-header ((t (:background "brightyellow"))))
 '(diff-refine-added ((t (:inherit diff-added :background "brightgreen"))))
 '(diff-refine-changed ((t (:inherit diff-changed :bold t))))
 '(diff-refine-removed ((t (:inherit diff-removed :background "brightblue"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "blue"))))
 '(ediff-current-diff-A ((t (:background "brightblue"))))
 '(ediff-current-diff-Ancestor ((t (:background "brightred" :foreground "brightwhite"))))
 '(ediff-current-diff-B ((t (:background "brightyellow"))))
 '(ediff-current-diff-C ((t (:background "brightgreen"))))
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
 '(eglot-highlight-symbol-face ((t (:inherit underline))))
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
 '(isearch-fail ((t (:background "red" :foreground "brightwhite"))))
 '(langtool-correction-face ((t (:background "brightyellow"))))
 '(langtool-errline ((t (:inherit error :underline t))))
 '(lazy-highlight ((t (:background "brightyellow"))))
 '(link ((t (:foreground "blue" :underline t))))
 '(magit-blame-heading ((t (:inherit diff-header))))
 '(magit-branch-local ((t (:inherit default :bold t))))
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
 '(magit-header-line ((t (:inherit header-line :bold t))))
 '(magit-section-highlight ((t (:inherit highlight))))
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
 '(smerge-markers ((t (:background "brightred" :foreground "brightwhite"))))
 '(smerge-refined-added ((t (:inherit diff-added :bold t))))
 '(smerge-refined-change ((t (:inherit diff-changed))))
 '(smerge-refined-removed ((t (:inherit diff-removed :bold t))))
 '(smerge-upper ((t (:background "brightblue"))))
 '(vertical-border ((t (:inherit mode-line-inactive))))
 '(warning ((t (:foreground "brightred" :bold t))))
 '(which-func ((t (:inherit header-line))))
 '(whitespace-big-indent ((t (:inherit whitespace-tab))))
 '(whitespace-tab ((t (:background "brightred" :foreground "white"))))
 '(widget-field ((t (:background "brightcyan" :foreground "black"))))
 '(widget-single-line-field ((t (:background "brightblue" :foreground "black")))))
