;; Project V8 stuff

;; Turn off P4
(if use-google-stuff
    (p4-toggle-vc-mode-off))

;; Compile command
(require 'compile)

(setq compile-history
      '("scons -j4 sample=shell"
        "scons -j4 mode=debug sample=shell"
        "tools/test.py -j4"
        "tools/test.py -j4 --mode=debug"))

(setq compile-command
   "scons -j4 sample=shell")

;; Use Ack instead of Find-grep
(require 'ack)

;; Keybindings
(if (eq system-type 'windows-nt)
    ;; ack hangs in cygwin under emacs, use good 'ol find-grep instead
    (progn
     (global-set-key [f6] 'find-grep)
     (custom-set-variables
      '(grep-find-command "gnu-find . -type f \\( -name \"*.cc\" -or -name \"*.h\" \\) -print0 | xargs -0 -e grep -nH -e ")))
    (global-set-key [f6] 'ack))
(global-set-key [f7] 'compile)
(global-set-key [f8] 'recompile)

;; Delete trailing whitespace when saving buffer
(add-hook 'before-save-hook '(lambda () (delete-trailing-whitespace)))

;; Functions to find project-wide files from inner project directories
(defun get-closest-file-path-crawler (dir file root)
  "Helper function for get-closest-file-path. Searches given FILE
from DIR upwards until ROOT directory. Returns path to file or
nil if file not found."
  (cond ((file-exists-p (expand-file-name file dir)) dir)
	((equal dir root) nil)
	(t (get-closest-file-path-crawler
	    (expand-file-name ".." dir) file root))))
(defun get-closest-file-path (file)
  "Searches given FILE from default directory upwards to
root. Returns path to file or nil if file not found"
  (get-closest-file-path-crawler
   (expand-file-name default-directory) file (expand-file-name "/")))

(setq cc-search-directories '("." "./src"))

;; Crawl up to dir with SConstruct when opening source file
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)
            (let ((scons-path (get-closest-file-path "SConstruct")))
		   (if scons-path (cd scons-path)))
		 (setq tags-revert-without-query t)))
