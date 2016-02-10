;; Google stuff

(setq google-elisp-root use-google-stuff)

;; The main file

(load-file (concat google-elisp-root "/google.el"))
(require 'googlemenu)

;; Preventing Emacs from gratuitously frobbing around in network mounts
(setq
 vc-ignore-dir-regexp
 "\\`\\([\\/][\\/]\\|/net/\\|/home/\\|/afs/\\)\\'"
 locate-dominating-stop-dir-regexp
 "\\`\\(?:[\\/][\\/]\\|/\\(?:net\\|afs\\|home\\|\\.\\.\\.\\)/\\)\\'")

;; Internal plugins dir
(add-to-list 'load-path (concat google-elisp-root "/gnuemacs"))
