;; Project V8 stuff

;; Compile command
(require 'compile)

(setq compile-history
      '("make -j16 ia32.release"
        "make -j16 ia32.release.check"
        "scons -j8 mode=debug sample=shell"
        "tools/test.py -j8 --mode=debug"))

(setq compile-command
   "make -j16 ia32.release")

;; Use Ag instead of Find-grep
(require 'ag)

;; Keybindings
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t)))
(if (eq system-type 'windows-nt)
    ;; ack hangs in cygwin under emacs, use good 'ol find-grep instead
    (progn
     (global-set-key [f6] 'find-grep)
     (custom-set-variables
      '(grep-find-command "gnu-find . -type f \\( -name \"*.cc\" -or -name \"*.h\" \\) -print0 | xargs -0 -e grep -nH -e ")))
    (global-set-key [f6] 'ag))
(global-set-key [f7] 'compile)
(global-set-key [f8] 'recompile)

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

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)
            ;; Crawl up to dir with SConstruct when opening source file
            (let ((scons-path (get-closest-file-path "SConstruct")))
                   (if scons-path (cd scons-path)))
            (setq tags-revert-without-query t)
            (google-set-c-style)))

(require 'find-things-fast)
(global-set-key '[f1] 'ftf-find-file)
