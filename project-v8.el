;; Project V8 stuff

;; Turn off P4
(p4-toggle-vc-mode-off)

;; Compile command
(setq compile-command-history
      '("scons sample=shell"
        "scons mode=debug sample=shell"))
(setq compile-command
   "scons sample=shell")

;; Find-grep
(setq grep-find-history
      '("find . -type f \\( -name \"*.js\" -or -name \"*.h\" -or -name \"*.cc\" \\) -exec grep -nH -e  {} /dev/null \\;"
        "find . -type f \\( -name \"*.h\" -or -name \"*.cc\" \\) -exec grep -nH -e  {} /dev/null \\;"))

;; Keybindings
(global-set-key [f6] 'find-grep)
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

;; Crawl up to dir with SConstruct when opening source file
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)
            (let ((scons-path (get-closest-file-path "SConstruct")))
		   (if scons-path (cd scons-path)))
		 (setq tags-revert-without-query t)))
