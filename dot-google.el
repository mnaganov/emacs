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

;; Train shell to hide SSO passwords.
(setq comint-password-prompt-regexp 
      (concat 
       "\\(" 
       comint-password-prompt-regexp 
       "\\)\\|[Ee]nter \\(same \\)?passphrase" 
       "\\|SSO password" 
       "\\|Enter password" 
       "\\|Please enter the pass-phrase to decrypt these private key(s)" 
       "\\|Enter your LDAP password for Mondrian:" 
       "\\|Please enter your unix login (kerberos) password:"))
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
