;;; mmm-jhtml.el --- MMM submode class for Google JHTML files
;;
;; Copyright (C) 2006, Google, Inc.
;;
;; Author:  Steve Yegge <stevey@google.com>

;;; Commentary:
;;
;; This file contains the definition of an MMM Mode submode class for
;; editing Mozilla Rhino .jhtml files used by the RnR web framework.

;;; Installation:
;;
;; 0. Make sure the following elisp packages are installed at your site:
;;    - mmm-mode (version 0.4.8 or later)
;;    - javascript-mode
;;    - css-mode
;; 1. Use your favorite html-editing mode for .jhtml files, e.g.:
;;    (add-to-list 'auto-mode-alist '("\\.jhtml$" . 'html-mode))
;; 2. Copy this file into your load-path where Emacs can find it.
;; 3. Add this line to your .emacs:
;;    (require 'mmm-jhtml)

;; Usage Notes:
;;  - you can use M-x mmm-jhtml-narrow-to-region (C-x n r) to narrow
;;    the buffer to just the contents of the current ejs tag.
;;    However, be warned that indenting JavaScript code in this state will
;;    only work properly if the first expression or statement inside
;;    the tag starts on its own line, at the far left column.
;;
;;  - indenting JS code inside ejs tags generally works.  You can use
;;    the tab key to indent the current line, or M-x indent-region to
;;    reindent the selected region.  The indentation is always relative
;;    to the column position of the opening-tag (i.e. "<%" or "<%=").
;;    See above for caveats about indenting when narrowed to the tag.
;;
;;    The support for indenting JS code hasn't been thoroughly tested,
;;    and there may still be situations where it doesn't work right.
;;
;;  - mmm-mode doesn't auto-reparse the buffer as you edit.  mmm-jhtml
;;    is configured to reparse the regions and re-fontify whenever you
;;    save the buffer.  If you make changes and want to reparse on the
;;    fly without saving, M-x mmm-parse-buffer (bound to the F5 key)
;;    to make it re-parse, which will fix up the submode regions.
;;    Basically any time the syntax-coloring is wrong, press F5 and it
;;    should fix it up.

;;; TODO(stevey):
;; - make css font-lock work inside inline style attrs
;; - add support for the 1-line ejs syntax (i.e. line starts with %)
;; - make the keybindings customizable
;; - add some utility bindings for generating common jhtml tags
;; - find some way to bundle the dependencies (e.g. vm-style single .elc)
;; - add some support refactoring (e.g. styles) and pretty-printing

;;; History:
;; - October 2006:  initial version

;;; Code:

(eval-and-compile
  (require 'mmm-auto)
  (require 'cl))  ; for find-if, remove-if

;; These are the two submodes supported by mmm-jhtml.  The main mode
;; is whatever you like for HTML editing (I prefer Emacs 22's sgml-mode).
(require 'css-mode)
(require 'javascript-mode)

(defvar mmm-jhtml-version "2006-11-14"
  "Version number for mmm-jhtml package.")

(defvar mmm-global-mode)
(setq mmm-global-mode 'maybe)  ; don't enable in all buffers

;; make some softer region faces; mmm's are rather garish

(defface mmm-jhtml-css-face '((default :inherit default)
                              (((class grayscale)))
                              (((class color) (background dark)))
                              (t (:background "floral white")))
  "Face used for background color of CSS regions in JHTMLs."
  :group 'mmm-faces)

(defface mmm-jhtml-client-js-face '((default :inherit default)
                                    (((class grayscale)))
                                    (((class color) (background dark)))
                                    (t (:background "seashell")))
  "Face used for background color of JavaScript regions in JHTMLs."
  :group 'mmm-faces)

(defface mmm-jhtml-rhino-face '((default :inherit default)
                                (((class grayscale)))
                                (((class color) (background dark)))
                                (t (:background "ghost white")))
  "Face used for background color of <% ... %> code blocks in JHTMLs."
  :group 'mmm-faces)

(defface mmm-jhtml-rhino-expr-face '((default :inherit default)
                                     (((class grayscale)))
                                     (((class color) (background dark)))
                                     (t (:background "mint cream")))
  "Face used for background color of <%= ... %> expressions in JHTMLs."
  :group 'mmm-faces)

(defface mmm-jhtml-noi18n-face '((default :inherit default)
                                 (((class grayscale)))
                                 (((class color) (background dark)))
                                 (t (:background "white smoke")))
  "Face used for background color of non-i18n regions in JHTMLs."
  :group 'mmm-faces)

(defface mmm-jhtml-noi18n-tag-face '((t (:foreground "red")))
  "Face used for ! in open-tag for non-i18n regions in JHTMLs."
  :group 'mmm-faces)

(defface mmm-jhtml-i18n-face '((default :inherit default)
                               (((class grayscale)))
                               (((class color) (background dark)))
                               (t (:background "white")))
  "Face used for background color of i18n regions in JHTMLs."
  :group 'mmm-faces)

(defface mmm-jhtml-i18n-tag-face '((t (:foreground "peru")))
  "Face used for background color of i18n regions in JHTMLs."
  :group 'mmm-faces)

(defvar mmm-jhtml-js-tag-regexp
  (concat "<script\\(?:[^>]*?javascript[^>]*\\)?"
          ;; this [^/] prevents matching empty script tags
          "[^/]*>"
          ;; have delimiter include rest of line if it's just whitespace.
          ;; this makes the background region coloring look nicer.
          "\\(?:\\s-*\n\\)?")
  "Match nonempty script tags whose language is javascript.
Note that the regexp for 'html-js in `mmm-sample.el' also matches
empty script tags that are just there to include a js file, which
they evidently didn't think of.  We don't want them to trigger
javascript-mode.")

(defun mmm-jhtml-get-mmm-classes ()
  "Return the list of MMM classes to use for JHTML.
Structured this way to make debugging/reloading easier."
   (list
    '(css-style-tag
      :submode css
      :face mmm-jhtml-css-face
      ;; delimiters are part of the core syntax (html tags)
      :delimiter-mode nil
      :front "<style[^>]*>\\(\\s-*\n\\)?"
      :back "[ \t]*</style>")

    '(css-style-attr
      :submode css
      :face mmm-jhtml-css-face
      :delimiter-mode nil
      :front "\\<style=\\(['\"]\\)"
      :back "~1"
      :save-matches 1)

    ;; Currently there's no real need to have MMM know about most of
    ;; the i18n tags, since all we really do with them is syntax-color
    ;; the start and end tags.  These rules worked at one point.

;;     '(i18n-end-tag
;;       :face mmm-jhtml-i18n-tag-face
;;       ;:front-face mmm-jhtml-i18n-tag-face
;;       :front "<%/_\\(?:\\sw+\\)?"
;;       :back "%>"
;;       ;:front-offset -1  ;; uncomment to include / in face
;;       ;:back-face font-lock-variable-name-face
;;       ;:back-delim -1
;;       )

;;     '(i18n-tag
;;       ;:face mmm-jhtml-rhino-face
;;       ;:front-face mmm-jhtml-i18n-tag-face
;;       :front "<%_\\(?:\\sw+\\)?\\s-"
;;       :back "%>"
;;       ;:back-face font-lock-variable-name-face
;;       ;:back-delim -1
;;       )

    ;; One exception:  an eval tag pair whose code is actual text.
    '(i18n-expr-child-tag
      :submode javascript
      :face mmm-jhtml-rhino-face
      :delimiter-mode nil
      :front "<%_[^>]*_%>\\(\\s-*\n\\)?"
      :back "[ \t]*<%/_%>")

    '(i18n-nomsg-tag
      :face mmm-jhtml-noi18n-face
      :front "<%\\(!_\\|_!\\)"
      :back "%>")

    '(ejs-comment
      :submode text-mode
      :face font-lock-comment-face
      :delimiter-mode nil
      :front "<%#"
      :back "-?%>"
      :include-front t  ;; color the tags as well
      :include-back t
      :front-face font-lock-comment-face
      :back-face font-lock-comment-face
      :insert ((?# ejs-comment    nil @ "<%#" @ " " _ " " @ "%>" @)))

    ;; I want to highlight expressions and code differently, after
    ;; having worked with hundreds of these RnR templates.  Otherwise
    ;; it can be tricky to pick out the little equal-sign that says
    ;; it's supposed to be evaluated and inserted into the html
    ;; stream.
    '(ejs-expr
      :submode javascript
      :face mmm-jhtml-rhino-expr-face
      :front "<%="
      :back "-?%>")

    ;; source: http://scott.elitists.net/users/scott/posts/rails-on-emacs
    '(ejs-code
      :submode javascript
      :face mmm-jhtml-rhino-face
      :front "<%[ \t\r\n]"
      :back "-?%>"
      :insert ((?% ejs-code       nil @ "<%"  @ " " _ " " @ "%>" @)
               (?= ejs-expression nil @ "<%=" @ " " _ " " @ "%>" @)))

    ;; expr:href="..."
    '(ejs-inline
      :submode javascript
      :face mmm-jhtml-rhino-face
      ;:delimiter-mode nil
      :front "\\<expr:\\w+=\\(['\"]\\)"
      :back "~1"
      :save-matches 1)

    ;; match single-quoted or double-quoted inline javascript snippets
    '(js-attr
      :submode javascript
      :face mmm-jhtml-client-js-face
      :delimiter-mode nil
      :front "\\<on\\w+=\\(['\"]\\)"
      :back "~1"
      :save-matches 1)

    (list
     'js-script-tag
     :submode 'javascript
     :face 'mmm-jhtml-client-js-face
     :delimiter-mode nil
     :front mmm-jhtml-js-tag-regexp
     :back"[ \t]*</script>"
     :front-verify 'mmm-jhtml-fail-on-nested-ejs-tags
     :insert '((?j js-tag nil @ "<script language=\"JavaScript\">"
                   @ "\n" _ "\n" @ "</script>" @)))))


(defun mmm-jhtml-fail-on-nested-ejs-tags ()
  "Return non-nil if we should start a submode region at this match.
If you put EJS tags inside a client-side <script> tag, for generating
bits of the client-side JS within the <script> tag using templating,
well, mmm-mode can't deal with nested tags, so you're hosed.  Hosed!
This function checks the last match to make sure there are no nested
EJS tags.  Should inspect (but not modify) the saved match-data.
Should return nil if it finds any nested tags."
  (let ((beg (match-beginning 2))  ; see `mmm-jhtml-js-tag-regexp' groups
        (end (match-end 2)))
    (and beg end
         (save-match-data
           (save-excursion
             (goto-char beg)
             (not (search-forward-regexp "<%" end t)))))))

(defun mmm-jhtml-create-group ()
  "Create the jhtml submode classes."
  (mmm-add-group 'jhtml (mmm-jhtml-get-mmm-classes)))

(defun mmm-jhtml-get-preferred-html-mode ()
  "Return the major-mode chosen for editing HTML in this Emacs session."
  (let ((pref (assoc 'html mmm-major-mode-preferences)))
    (if pref pref
      (cdr-safe
       (find-if (lambda (entry)
                  (string-match "\\(jhtml\\|html\\|xml\\)" (car entry)))
                auto-mode-alist)))))


(defun mmm-jhtml-finish-init ()
  "Does miscellaneous setup after mmm-mode first parses the buffer."

  ;; This function (`mmm-jhtml-finish-init') is getting interrupted
  ;; by some race condition, maybe a timer used by nxml-mode.  While
  ;; debugging it I stumbled on this hack, which seems to provide a
  ;; reasonably reliable fix.  We're basically yielding to let nxml
  ;; finish its stuff.  This really needs to be figured out properly.
  ;;(sit-for 0)

  ;; Turn on font-lock in nxml-mode.
  ;; This is necessary because nxml-mode doesn't use font-lock.
  ;; It does its own syntax coloring.  Using font-lock-mode for
  ;; mmm-mode doesn't interfere with nxml-mode's syntax coloring.
;;   (when (and (eq major-mode 'nxml-mode)
;;              (not font-lock-mode))
;;     (font-lock-mode 1)
;;     (font-lock-fontify-buffer))

  (mmm-jhtml-add-comment-keywords)

  ;; other setup...
  (local-set-key "\C-xnr" 'mmm-jhtml-narrow-to-region)
  (local-set-key [f6] 'mmm-generic-reparse)
  (local-set-key [f5] 'mmm-generic-reparse)
  (local-set-key [f7] 'mmm-jhtml-debug-reload-groups))

(defun mmm-generic-reparse ()
  "Reparses and re-fontifies the buffer."
  (interactive)
  (mmm-parse-buffer)
  (font-lock-fontify-buffer))

;; NOTE:  mmm actually supports finer-grained hooks than this, and I
;; should be taking advantage of them.  In particular, for us, it runs
;; all the following hooks (for which the hook variable is bound):
;;
;;  `mmm-major-mode-hook'
;;  `mmm-universal-class-hook'
;;  `mmm-css-style-tag-hook'
;;  `mmm-css-style-attr-class-hook'
;;  `mmm-text-mode-submode-hook'
;;  `mmm-ejs-comment-class-hook'
;;  `mmm-javascript-mode-submode-hook'
;;  `mmm-ejs-code-class-hook'
;;  `mmm-js-attr-class-hook'
;;  `mmm-js-attr-doublequote-class-hook'
;;  `mmm-js-script-tag-class-hook'
;;  `mmm-jhtml-class-hook'
;;
;; ...and a class-hook for every other submode class we defined.

(add-hook 'mmm-mode-hook 'mmm-jhtml-finish-init)

(defun mmm-jhtml-customize-javascript ()
  "Add some javascript-mode customizations for .jhtml files."
  ;; Unfortunately indentation is really dorked right now,
  ;; so don't exacerbate it by auto-reindenting when braces are inserted.
  (c-toggle-electric-state -1))

(add-hook 'mmm-javascript-mode-submode-hook 'mmm-jhtml-customize-javascript)

;; add some keywords for highlighting jsdoc tags
(defvar mmm-jhtml-ejs-comment-keywords nil
  "Font-lock keywords for highlighting jsdoc tags.")

(setq mmm-jhtml-ejs-comment-keywords
      (list
        (list
         (concat
          "^\\s-+\\(@param\\)\\s-+"
          ;; highlight type and up to 2 extra union types
          "{\\(\\w+\\)\\(?:|\\(\\w+\\)\\)?\\(?:|\\(\\w+\\)\\)?}"
          ;; highlight variable name and optional dot or []
          "\\s-+\\([$a-zA-Z0-9_]+\\)\\(?:\\.\\([$a-zA-Z0-9_]+\\)\\)?")
         '(1 font-lock-reference-face t)
         '(2 font-lock-type-face t)
         '(3 font-lock-type-face t t)
         '(4 font-lock-type-face t t)
         '(5 font-lock-variable-name-face t)
         '(6 font-lock-reference face t t))
        '("^\\s-+\\(@author\\)\\>.+\\<\\(\\w+@google.com\\)?"
          (1 font-lock-reference-face t)
          (2 (face
              :foreground "blue"
              :underline t
              help-echo "send email"
              mouse-face highlight
              keymap mmm-jhtml-email-address-keymap)))))

(defun mmm-jhtml-email-address-keymap ()
  "Return a keymap for which Enter sends an email"
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap text-mode-map)
    (define-key kmap 10 'mmm-jhtml-send-email)))

(defun mmm-jhtml-send-email ()
  "Email recipient under the point."
  (when buffer-read-only
    (let ((beg
           (save-excursion
             (while (and (not (bolp))
                         (get-text-property (1- (point)) 'mouse-face))
               (forward-char -1))
             (point)))
          (end
           (save-excursion
             (while (and (not (eolp))
                         (get-text-property (1+ (point)) 'mouse-face))
               (forward-char 1))
             (point))))
      (unless (fboundp 'mail)
        (require 'sendmail))
      (mail nil (buffer-substring-no-properties beg end)
            (or buffer-file-name (buffer-name))))))

(defun mmm-jhtml-add-comment-keywords ()
  (font-lock-add-keywords nil mmm-jhtml-ejs-comment-keywords))

;(add-hook 'mmm-ejs-comment-class-hook 'mmm-jhtml-add-comment-keywords)

;;; finish load-time setup

;; use submode-specified background coloring
(setq mmm-submode-decoration-level 2)

;; create our jhtml mmm submode classes
(mmm-jhtml-create-group)

;; register for nxml-mode, sgml-mode, or whatever (JHTMLs only)
(mmm-add-mode-ext-class
 (mmm-jhtml-get-preferred-html-mode) "\\.jhtml$" 'jhtml)

;; http://sourceforge.net/mailarchive/forum.php?thread_id=9223713&forum_id=5108
(defun save-mmm-c-locals ()
  (with-temp-buffer
    (javascript-mode)
    (dolist (v (buffer-local-variables))
      (when (string-match "\\`c-" (symbol-name (car v)))
        (add-to-list 'mmm-save-local-variables `(,(car v) nil
                                                 ,mmm-c-derived-modes))))))

;; mmm-mode hardcodes the buffer-local variables to save, and the list is
;; outdated for Emacs 22.  The function above updates the list.
(if (string-match "^22" emacs-version)
    (save-mmm-c-locals))

;;; JavaScript indentation support - totally horked at the moment.

(defun mmm-inside-mmm-region-p ()
  "Return the mmm submode if we're in an active mmm submode."
  (and (boundp 'mmm-mode)
       mmm-current-submode))

(defadvice c-indent-command (around jhtml-js-indenter activate)
  (if (and (boundp 'mmm-mode)
           (boundp 'mmm-current-submode)
           (eq mmm-current-submode 'javascript-mode))
      (save-restriction
        (mmm-jhtml-narrow-to-region)
        ad-do-it)
    ad-do-it))

(defun mmm-jhtml-narrow-to-region (&optional ovl)
  "Restrict editing (with `narrow-to-region') to the current ejs tag.
Return t if we actually restricted editing."
  (interactive)
  (let ((ovl (or ovl (mmm-overlay-at))))
    (if ovl
        (narrow-to-region (overlay-start ovl)
                          (overlay-end ovl))
      (when (interactive-p)
        (message "Not inside an ejs region.")))))

;; silence byte-compiler
(eval-when-compile
  (defvar mmm-current-submode nil)
  (unless (fboundp 'mmm-overlay-at)
    (defun mmm-overlay-at () nil)))

(defun mmm-generic-reparse-on-save ()
  "Reparse regions and refontify on save if we're in an MMM buffer."
  (and (boundp 'mmm-mode)
       mmm-mode
       (mmm-generic-reparse)))

(add-hook 'after-save-hook 'mmm-generic-reparse-on-save)

(defun mmm-jhtml-debug-reload-groups ()
  "Reload the mmm-jhtml submode group and submode classes.
Used for interactive development; you shouldn't need it unless
you're debugging `mmm-jhtml.el'.  After calling this, you may still
need to re-open your JHTML file to see the effects.  Sometimes it works."
  (interactive)
  (require 'cl)
  ;; Remove all the submode classes we defined
  (setq mmm-classes-alist
        (let ((class-names (mapcar 'car (mmm-jhtml-get-mmm-classes))))
          (remove-if (lambda (entry)
                       (or
                        (memq (car entry) class-names)
                        ;; catch spurious extra entries
                        (string-match "jhtml" (symbol-name (car entry)))))
                     mmm-classes-alist)))
  (mmm-jhtml-create-group)
  (mmm-generic-reparse))

;; needed in order to use faces in font-lock keywords
(setq mmm-jhtml-i18n-tag-face 'mmm-jhtml-i18n-tag-face
      mmm-jhtml-noi18n-tag-face 'mmm-jhtml-noi18n-tag-face)

(defvar mmm-jhtml-sgml-keywords
  '(("<%_\\([a-z0-9]+\\)"
     (1 mmm-jhtml-i18n-tag-face)))
  "Extra keywords to highlight in sgml-mode")

(setq mmm-jhtml-sgml-keywords
      '(("<\\(%/?_[a-zA-Z0-9]*\\)[^>]*\\(%\\)>"  ;; start tag
         (1 mmm-jhtml-i18n-tag-face)
         (2 mmm-jhtml-i18n-tag-face))
        ("<\\(%/_[a-zA-Z0-9]*\\)%>"  ;; end tag
         (1 mmm-jhtml-i18n-tag-face))
        ("\\([_/]_[a-zA-Z0-9]*\\)[^>]*\\(%\\)>"  ;; end special start tag
         (1 mmm-jhtml-i18n-tag-face)
         (2 mmm-jhtml-i18n-tag-face))
        ("<\\(%\\)\\(!\\)\\(_\\)"
         (1 mmm-jhtml-i18n-tag-face)
         (2 mmm-jhtml-noi18n-tag-face)
         (3 mmm-jhtml-i18n-tag-face))))

(add-hook 'sgml-mode-hook
          (lambda ()
            (font-lock-add-keywords nil mmm-jhtml-sgml-keywords)))

(provide 'mmm-jhtml)

;;; mmm-jhtml.el ends here
