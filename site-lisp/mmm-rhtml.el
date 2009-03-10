;;; mmm-rhtml.el --- MMM submode class for Google RHTML files
;;
;; Copyright (C) 2005, Google, Inc.
;;
;; Author:  Steve Yegge <stevey@google.com>

;;; Commentary:
;;
;; This file contains the definition of an MMM Mode submode class for
;; editing Ruby .rhtml files.

;;; Installation:
;;
;; 0. Make sure the following elisp packages are installed at your site:
;;    - mmm-mode
;;    - ruby-mode
;;    - javascript
;;    - css-mode
;; 1. Use your favorite html-editing mode for .rhtml files, e.g.:
;;    (add-to-list 'auto-mode-alist '("\\.rhtml$" . 'html-mode))
;; 2. Copy this file into your load-path where Emacs can find it.
;; 3. Add this line to your .emacs:
;;    (require 'mmm-rhtml)

;; Usage Notes:
;;  - you can use M-x mmm-rhtml-narrow-to-region (C-x n r) to narrow
;;    the buffer to just the contents of the current erb tag.
;;    However, be warned that indenting Ruby code in this state will
;;    only work properly if the first expression or statement inside
;;    the tag starts on its own line, at the far left column.
;;
;;  - indenting Ruby code inside erb tags generally works.  You can use
;;    the tab key to indent the current line, or M-x indent-region to
;;    reindent the selected region.  The indentation is always relative
;;    to the column position of the opening-tag (i.e. "<%" or "<%=").
;;    See above for caveats about indenting when narrowed to the tag.
;;
;;    The support for indenting Ruby code hasn't been thoroughly tested,
;;    and there may still be situations where it doesn't work right.
;;
;;  - indenting JavaScript code works correctly, but assumes that the
;;    enclosing <script> tag is left-justified in the buffer.
;;
;;  - mmm-mode doesn't auto-reparse the buffer as you edit.  If you
;;    add a new erb tag, M-x mmm-parse-buffer (bound to the F5 key) to
;;    make it re-parse, which will fix up the submode regions.

;;; TODO(stevey):
;; - make css font-lock work inside inline style attrs
;; - add support for the 1-line erb syntax (i.e. line starts with %)
;; - make the keybindings customizable
;; - add some utility bindings for generating common rhtml tags
;; - find some way to bundle the dependencies (e.g. vm-style single .elc)
;; - add some support refactoring (e.g. styles) and pretty-printing

;;; History:
;; - November 2005:  added support for indenting ruby code inside tags
;; - September 2005:  initial version

;;; Code:

(require 'mmm-auto)

;; these are the three submodes supported by mmm-rhtml
(require 'css-mode)
(require 'javascript-mode)
(require 'ruby-mode)

(defvar mmm-rhtml-version "2005-11-20"
  "Version number for mmm-rhtml package.")

(setq mmm-global-mode 'maybe)  ; don't enable in all buffers

;; make some softer region faces; mmm's are rather garish

(defface mmm-rhtml-css-face '((t (:background "mint cream")))
  "Face used for background color of CSS regions in RHTMLs."
  :group 'mmm-faces)

(defface mmm-rhtml-js-face '((t (:background "alice blue")))
  "Face used for background color of JavaScript regions in RHTMLs."
  :group 'mmm-faces)

(defface mmm-rhtml-ruby-face '((t (:background "seashell")))
  "Face used for background color of Ruby regions in RHTMLs."
  :group 'mmm-faces)


;;; create the RHTML submode classes

(defvar mmm-rhtml-js-tag-regexp
  (concat "<script[^>]*javascript[^>]*"
          ;; this [^/] prevents matching empty script tags
          "[^/]*>"
          ;; have delimiter include rest of line if it's just whitespace.
          ;; this makes the background region coloring look nicer.
          "\\(\\s-*\n\\)?")
  "Matches nonempty script tags whose language is javascript.
Note that the regexp for 'html-js in `mmm-sample.el' also matches
empty script tags that are just there to include a js file, which
they evidently didn't think of.  We don't want them to trigger
javascript-mode.")


(defun mmm-rhtml-create-group ()
  (mmm-add-group
   'rhtml
   (list
    '(css-style-tag
      :submode css
      :face mmm-rhtml-css-face
      ;; delimiters are part of the core syntax (xml/html tags)
      :delimiter-mode nil
      :front "<style[^>]*>\\(\\s-*\n\\)?"
      :back "[ \t]*</style>")

    '(css-style-attr
      :submode css
      :face mmm-rhtml-css-face
      :delimiter-mode nil
      :front "\\<style=\""
      :back "\"")

    '(erb-comment
      :submode text-mode
      :face font-lock-comment-face
      :delimiter-mode nil
      :front "<%#" 
      :back "%>"
      :front-face font-lock-comment-face
      :back-face font-lock-comment-face)

    ;; source: http://scott.elitists.net/users/scott/posts/rails-on-emacs
    '(erb-code
      :submode ruby-mode
      :face mmm-rhtml-ruby-face
      :front "<%=?" 
      :back "-?%>" 
      :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
               (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
               (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))

    ;; should figure out how to allow both quote types.
    ;; I think MMM has a way to do this using backreferences
    ;; in :back referring to submatches in :front
    '(js-attr
      :submode javascript
      :face mmm-rhtml-js-face
      :delimiter-mode nil
      :front "\\<on\\w+='"
      :back "'")

    '(js-attr-doublequote
      :submode javascript
      :face mmm-rhtml-js-face
      :delimiter-mode nil
      :front "\\<on\\w+=\""
      :back "\"")

    (list
     'js-script-tag
     :submode 'javascript
     :face 'mmm-rhtml-js-face
     :delimiter-mode nil
     :front mmm-rhtml-js-tag-regexp
     :back"[ \t]*</script>"
     :insert '((?j js-tag nil @ "<script language=\"JavaScript\">"
                   @ "\n" _ "\n" @ "</script>" @))))))


(defun mmm-rhtml-get-preferred-xml-mode ()
  "Return the major-mode chosen for editing XML in this Emacs session."
  (let ((pref (assoc 'xml mmm-major-mode-preferences)))
    (if pref pref
      (cdr-safe
       (find-if (lambda (entry)
                  (string-match "\\(rhtml\\|xml\\)" (car entry)))
                auto-mode-alist)))))


(defun mmm-rhtml-finish-init ()
  "Does miscellaneous setup after mmm-mode first parses the buffer."
  (return)
  ;; This function (`mmm-rhtml-finish-init') is getting interrupted
  ;; by some race condition, maybe a timer used by nxml-mode.  While
  ;; debugging it I stumbled on this hack, which seems to provide a
  ;; reasonably reliable fix.  We're basically yielding to let nxml
  ;; finish its stuff.  This really needs to be figured out properly.
  (sit-for 0)

  ;; Turn on font-lock in nxml-mode.
  ;; This is necessary because nxml-mode doesn't use font-lock.
  ;; It does its own syntax coloring.  Using font-lock-mode for
  ;; mmm-mode doesn't interfere with nxml-mode's syntax coloring.
  (when (and (eq major-mode 'nxml-mode)
             (not font-lock-mode))
    (font-lock-mode 1)
    (font-lock-fontify-buffer))

  ;; other setup...
  (local-set-key "\C-xnr" 'mmm-rhtml-narrow-to-region)
  (local-set-key [f6] 'mmm-rhtml-reparse)
  (local-set-key [f5] 'mmm-rhtml-reparse))

(defun mmm-rhtml-reparse ()
  "Reparses and re-fontifies the buffer."
  (interactive)
  (mmm-parse-buffer)
  (font-lock-fontify-buffer))

(add-hook 'mmm-mode-hook 'mmm-rhtml-finish-init)

;;; finish load-time setup

;; use submode-specified background coloring
(setq mmm-submode-decoration-level 2)

;; create our rhtml mmm submode classes
(mmm-rhtml-create-group)

;; register for nxml-mode, sgml-mode, or whatever (RHTMLs only)
(mmm-add-mode-ext-class
 'sgml-mode "\\.rhtml$" 'rhtml)

;;; Ruby indentation support
;;
;; ruby-mode's parsing assumes the entire file contains ruby code,
;; so for indentation to work, we have to fool the parser into thinking
;; the contents of the current tag are the entire buffer.
;;
;; By narrowing the editing region to just the current erb tag, ruby-mode
;; computes the proper indentation, relative to the beginning of the
;; region.  However, the tag itself is almost always indented, so we have
;; to add that offset to the computed indentation.  We do this by
;; redefining `ruby-indent-line' to narrow to the tag, compute the indent,
;; remove the narrowing, and then add the tag column offset to the
;; computed indent.  There are two edge cases to handle:
;;
;;  - if we're inside a nested ruby block, the ruby-mode indenter computes
;;    the indentation relative to the *absolute* column of the block.
;;    In this situation we don't need to add the erb tag's start column.
;;
;;  - we don't want to invoke the ruby line indenter if we're on the same
;;    line as the opening delimiter, since the delimiter itself would move.
;;
;; The current approach seems to work pretty well, and requires no changes
;; to ruby-mode.

(defvar mmm-current-submode nil)  ;; in case mmm-mode isn't loaded
(defun mmm-inside-mmm-region-p ()
  "Return t if we're in an active mmm submode."
  (and (boundp 'mmm-mode)
       mmm-current-submode))

(defadvice ruby-calculate-indent (around rhtml-indent-smartly activate)
  "Help ruby-mode indent code properly inside mmm submode regions.
Narrows editing to the bounds of the current submode region during
the calculation of the current line's indent."
  (if (not (mmm-inside-mmm-region-p))
      ad-do-it
    (let ((ovl (mmm-overlay-at)))
      (if ovl
          (save-restriction
            (narrow-to-region (overlay-start ovl)
                              (overlay-end ovl))
            ad-do-it)
        ad-do-it))))

(defun ruby-indent-line (&optional flag)
  "Replacement for `ruby-indent-line' in ruby-mode.el for rhtml buffers.
Adds the offset of the enclosing erb tag to the computed indentation."
  (interactive)
  (if (not (mmm-inside-mmm-region-p))
      (ruby-indent-to (ruby-calculate-indent))
    (let ((indent (ruby-calculate-indent))
          (ovl (mmm-overlay-at)))
      (when (and indent
                 (plusp indent)
                 (or (not ovl)
                     (not (mmm-rhtml-on-first-line ovl))))
        (ruby-indent-to
         (+ indent 
            (cond
             ((null ovl) 0)
             ((mmm-rhtml-in-nested-block ovl) 0)
             (t (mmm-rhtml-get-erb-tag-offset ovl)))))))))

(defun mmm-rhtml-in-nested-block (&optional ovl)
  "Return t if we're in a nested Ruby block within an erb tag.
If it returns nil, the parser thinks we're at the top level,
outside of any block.  OVL should be from `mmm-overlay-at'."
  (interactive) ;; for testing
  (if (interactive-p) 
      (setq ovl (mmm-overlay-at)))
  (let ((nested
         (and ovl
              (not (eq (overlay-start ovl)
                       (save-excursion
                         (save-restriction
                           (mmm-rhtml-narrow-to-region ovl)
                           (ruby-beginning-of-block)
                           (beginning-of-line)
                           (point))))))))
    (if (interactive-p)
        (if nested
            (message "in nested block")
          (message "not in nested block")))
    nested))
         
(defun mmm-rhtml-on-first-line (ovl)
  "Return t if the point is on the same line as the opening <%.
If not inside an erb tag, defaults to t."
  (or (not ovl)
      (eq (point-at-bol)
          (save-excursion
            (goto-char (overlay-start ovl))
            (point-at-bol)))))
          
(defun mmm-rhtml-get-erb-tag-offset (ovl)
  "Return the column of the enclosing erb tag, or 0 if we're not in one.
OVL is the mmm-overlay at the point, if any."
  (if ovl
      (save-excursion
        (goto-char (overlay-start ovl))
        (current-column))
    0))

(defun mmm-rhtml-narrow-to-region (&optional ovl)
  "Restrict editing (with `narrow-to-region') to the current erb tag.
Return t if we actually restricted editing."
  (interactive)
  (let ((ovl (or ovl (mmm-overlay-at))))
    (if ovl
        (narrow-to-region (overlay-start ovl)
                          (overlay-end ovl))
      (when (interactive-p)
        (message "Not inside an erb region.")))))
      
;;; end Ruby indentation support

;; silence byte-compiler
(eval-when-compile
  (defvar mmm-current-submode nil)
  (unless (fboundp 'mmm-overlay-at)
    (defun mmm-overlay-at () nil)))

;; JavaScript indentation support -- currently assumes your script tag
;; is left-justified.  Could probably be fixed pretty easily.  One bug
;; is that narrowing/widening re-forces the fontification, which seems 
;; unnecessary.
(defadvice c-indent-command (around rhtml-js-indenter activate)
  (if (and (boundp 'mmm-current-submode)
           (mmm-inside-mmm-region-p)
           (eq mmm-current-submode 'javascript-mode))
      (save-restriction
        (mmm-rhtml-narrow-to-region)
        ad-do-it)
    ad-do-it))


(defun mmm-rhtml-debug-reload-groups ()
  "Reload the mmm-rhtml submode group and submode classes.
Used for interactive development; you shouldn't need it unless
you're debugging `mmm-rhtml.el'.  After calling this, you'll still
need to re-open your RHTML file to see the effects."
  (interactive)
  (require 'cl)
  (setq mmm-classes-alist
        (remove-if (lambda (entry)
                     (memq (car entry) '(erb-code
                                         css-style-tag
                                         java-rhtml-expr
                                         js-script-tag)))
                   mmm-classes-alist))
  (mmm-rhtml-create-group))


(provide 'mmm-rhtml)

;;; mmm-rhtml.el ends here
