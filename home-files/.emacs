;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq package-enable-at-startup nil)

(setq emacs-root "~/emacs/")
(setq use-google-stuff nil)
(setq use-project "home")
(load-file (concat emacs-root "dot-emacs.el"))
