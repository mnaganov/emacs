;; Google stuff

(setq google-elisp-root use-google-stuff)

;; Make Emacs start blazing fast! (but not on Windows)
(if nil
(if (not (eq system-type 'windows-nt))
    (progn (load-file (concat emacs-root "elisp-cache.el"))
           (let ((nfsdir google-elisp-root)
                 (cachedir (concat emacs-root ".elisp-cache")))
             (setq load-path (append load-path (list cachedir nfsdir)))
             (require 'elisp-cache)
             (elisp-cache nfsdir cachedir))))
)
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
