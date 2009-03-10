;;; javascript-indent.el --- fixes javascript-mode indentation
;;
;; Copyright (C) 2006 Karl Landström
;; Author: Karl Landström <kland@comhem.se>

;; ----------------------------------------------------------------------
;; Note:  Karl's javascript.el really does a great job with indentation.
;; However, it completely and utterly breaks comment filling, sending your
;; beautiful jsdoc comments straight to the trash compactor.  So I've
;; switched back to using javascript-mode.el (bundled with XEmacs, but it
;; works just fine in FSF Emacs), and I've gutted Karl's javascript.el to
;; just provide the indentation stuff, not the font-locking or commenting.
;;
;; To install it, add the file javascript-indent.el to your load path, and
;;
;;  (autoload 'javascript-indent "javascript-indent")
;;  (add-hook 'javascript-mode-hook 'javascript-fix-indentation)
;;
;; to your .emacs file.
;;
;; The only real downside to these indenter function is that they set the
;; underscore (_) to be a word-character, so forward-word skips right over
;; underscores.  Doesn't bother me much, as I have some custom functions
;; I often use for navigation that stop at CamelCase boundaries, underscores,
;; and other punctuation.  Ask me if you need them.
;; 
;; Steve Yegge (stevey@google.com)
;; ----------------------------------------------------------------------

;;; Code:

(require 'cc-mode)

(defcustom javascript-indent-level 2 
  "Number of spaces for each indentation step."
  :group 'languages)

(defcustom javascript-auto-indent-flag t
  "Automatic indentation with punctuation characters. If non-nil, the
current line is indented when certain punctuations are inserted."
  :group 'languages)


;; --- Keymap ---

(defvar javascript-mode-map nil 
  "Keymap used in JavaScript mode.")

(unless javascript-mode-map 
  (setq javascript-mode-map (make-sparse-keymap)))

(when javascript-auto-indent-flag
  (mapc (lambda (key) 
	  (define-key javascript-mode-map key 'javascript-insert-and-indent))
	'("{" "}" "(" ")" ":" ";" ",")))

(defun javascript-insert-and-indent (key)
  "Run command bound to key and indent current line. Runs the command
bound to KEY in the global keymap and indents the current line."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  ;(indent-according-to-mode)
  )


;; --- Syntax Table And Parsing ---

(defvar javascript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)

    ;; The syntax class of underscore should really be `symbol' ("_")
    ;; but that makes matching of tokens much more complex as e.g.
    ;; "\\<xyz\\>" matches part of e.g. "_xyz" and "xyz_abc". Defines
    ;; it as word constituent for now.
    (modify-syntax-entry ?_ "w" table)

    table)
  "Syntax table used in JavaScript mode.")


(defun js-re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `js-re-search-forward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-forward 
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse))) 
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            (t
             (setq count (1- count))))
      (setq saved-point (point))))
  (point))


(defun js-re-search-forward (regexp &optional bound noerror count)
  "Search forward but ignore strings and comments. Invokes
`re-search-forward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr 
         (cond ((null count)
                '(js-re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(js-re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(js-re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun js-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js-re-search-backward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-backward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse))) 
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse) 
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            (t
             (setq count (1- count))))))
  (point))


(defun js-re-search-backward (regexp &optional bound noerror count)
  "Search backward but ignore strings and comments. Invokes
`re-search-backward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr 
         (cond ((null count)
                '(js-re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(js-re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(js-re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

;; --- Indentation ---

(defconst js-possibly-braceless-keyword-re
  (regexp-opt
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with")
   'words)
  "Regular expression matching keywords that are optionally
  followed by an opening brace.")

(defconst js-indent-operator-re
  (concat "[-+*/%<>=&^|?:]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
  of continued expressions.")


(defun js-looking-at-operator-p ()
  "Return non-nil if text after point is an operator (that is not
a comma)."
  (save-match-data
    (and (looking-at js-indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (js-re-search-backward "[?:{]\\|\\<case\\>" nil t)
                    (looking-at "?")))))))


(defun js-continued-expression-p ()
  "Returns non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js-looking-at-operator-p)
        (and (js-re-search-backward "\n" nil t)
	     (progn 
	       (skip-chars-backward " \t")
	       (backward-char)
	       (and (js-looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun js-end-of-do-while-loop-p ()
  "Returns non-nil if word after point is `while' of a do-while
statement, else returns nil. A braceless do-while statement
spanning several lines requires that the start of the loop is
indented to the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\<while\\>")
	(if (save-excursion 
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion 
	      (backward-list) (backward-word 1) (looking-at "\\<do\\>"))
	  (js-re-search-backward "\\<do\\>" (point-at-bol) t)
	  (or (looking-at "\\<do\\>")
	      (let ((saved-indent (current-indentation)))
		(while (and (js-re-search-backward "^[ \t]*\\<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "[ \t]*\\<do\\>")
		     (not (js-re-search-forward 
			   "\\<while\\>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun js-ctrl-statement-indentation ()
  "Returns the proper indentation of the current line if it
starts the body of a control statement without braces, else
returns nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (> (count-lines (point-min) (point)) 1)
                 (not (looking-at "{"))
                 (js-re-search-backward "[[:graph:]]" nil t)
                 (not (looking-at "[{([]"))
                 (progn
                   (forward-char) 
                   (backward-sexp)
                   (when (looking-at "(") (backward-word 1))
                   (and (save-excursion
                          (skip-chars-backward " \t}" (point-at-bol))
                          (bolp))
                        (looking-at js-possibly-braceless-keyword-re)
                        (not (js-end-of-do-while-loop-p))))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) javascript-indent-level)))))


(defun js-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (let ((ctrl-stmt-indent (js-ctrl-statement-indentation))
          (same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
          (continued-expr-p (js-continued-expression-p)))
      (cond (ctrl-stmt-indent)
            ((nth 1 parse-status)
             (goto-char (nth 1 parse-status))
             (if (looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
                 (let ((p (parse-partial-sexp (point-at-bol) (point))))
                   (when (save-excursion (skip-chars-backward " \t)")
                                         (looking-at ")"))
                     (backward-list))
                   (if (nth 1 p)
                       (progn (goto-char (1+ (nth 1 p)))
                              (skip-chars-forward " \t"))
                     (back-to-indentation))
                   (cond (same-indent-p
                          (current-column))
                         (continued-expr-p
                          (+ (current-column) (* 2 javascript-indent-level)))
                         (t
                          (+ (current-column) javascript-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column)))
	    (continued-expr-p javascript-indent-level)
            (t 0)))))


(defun javascript-indent-line ()
  "Indent the current line as JavaScript source text."
  (interactive)
  (let ((parse-status 
         (save-excursion (parse-partial-sexp (point-min) (point-at-bol))))
        (offset (- (current-column) (current-indentation))))
    (when (not (nth 8 parse-status))
      (indent-line-to (js-proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))


(defun javascript-fix-indentation ()
  "Fixes standard javascript-mode indentation."
  (use-local-map javascript-mode-map)
  (set-syntax-table javascript-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'javascript-indent-line))

(provide 'javascript-indent)

;;; javascript-indent.el ends here
