;; Project V8 stuff

;; Compile command
(require 'compile)

(setq compile-history
      '("scons -j6 sample=shell"
        "scons -j6 mode=debug sample=shell"
        "tools/test.py -j6"
        "tools/test.py -j6 --mode=debug"
        "WebKitTools/Scripts/build-webkit"
        "WebKitTools/Scripts/build-webkit --debug"
        "WebKitTools/Scripts/run-webkit-tests LayoutTests/inspector/ LayoutTests/fast/profiler/"))

(setq compile-command
   "scons -j6 sample=shell")

;; Use Ack instead of Find-grep
(require 'ack)

;; Keybindings
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t)))
(if (eq system-type 'windows-nt)
    ;; ack hangs in cygwin under emacs, use good 'ol find-grep instead
    (progn
     (global-set-key [f6] 'find-grep)
     (custom-set-variables
      '(grep-find-command "gnu-find . -type f \\( -name \"*.cc\" -or -name \"*.h\" \\) -print0 | xargs -0 -e grep -nH -e ")))
    (global-set-key [f6] 'ack))
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

(defun set-webkit-c-style ()
   (setq c-basic-offset 4)
   (setq tab-width 8)
   (setq indent-tabs-mode nil)
   (c-set-offset 'substatement-open 0))

(defun maybe-set-webkit-c-style ()
  (if (string-match "webkit" (downcase buffer-file-name))
      (set-webkit-c-style)))

(defun set-webkit-js2-style ()
   (setq js2-basic-offset 4)
   (setq tab-width 8)
   (setq indent-tabs-mode nil))

(defun maybe-set-webkit-js2-style ()
  (if (string-match "webkit" (downcase buffer-file-name))
      (set-webkit-js2-style)))

(setq cc-search-directories '("." "./src"))

;; Crawl up to dir with SConstruct when opening source file
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)
            (let ((scons-path (get-closest-file-path "SConstruct")))
                   (if scons-path (cd scons-path)))
            (setq tags-revert-without-query t)
            (maybe-set-webkit-c-style)))

(add-hook 'js2-mode-hook 'maybe-set-webkit-js2-style)
