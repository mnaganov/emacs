;; Windows-specific part

;;(require 'setup-cygwin)

(let ((grep-execs-path "c:/Program Files/Git/usr/bin/"))
  (setq grep-find-template
        (concat "\"" grep-execs-path "find.exe\" <D> <X> -type f <F> -exec \"" grep-execs-path "grep.exe\" <C> -nH --null <R> \"{}\" \";\"")))
