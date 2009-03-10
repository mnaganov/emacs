;; Google stuff

;; Make Emacs start blazing fast!
(load-file (concat emacs-root "elisp-cache.el"))
(let ((nfsdir "/home/build/public/eng/elisp")
      (cachedir (concat emacs-root ".elisp-cache")))
   (setq load-path (append load-path (list cachedir nfsdir)))
   (require 'elisp-cache)
   (elisp-cache nfsdir cachedir))

;; The main file
(load-file "/home/build/public/eng/elisp/google.el")
(require 'googlemenu)

;; Preventing Emacs from gratuitously frobbing around in network mounts
(setq
 vc-ignore-dir-regexp
 "\\`\\([\\/][\\/]\\|/net/\\|/home/\\|/afs/\\)\\'"
 locate-dominating-stop-dir-regexp
 "\\`\\(?:[\\/][\\/]\\|/\\(?:net\\|afs\\|home\\|\\.\\.\\.\\)/\\)\\'")

;; Internal plugins dir
(add-to-list 'load-path "/home/build/eng/elisp/gnuemacs")
