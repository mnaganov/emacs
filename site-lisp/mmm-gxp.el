;;; mmm-gxp.el --- MMM submode class for Google GXP files

;; Copyright (C) 2005, Google, Inc.

;; Author:  Steve Yegge <stevey@google.com>

;;; Commentary:

;; This file contains the definition of an MMM Mode submode class for
;; editing Google XML Pages (aka "GXP") files.  You set the major mode
;; you prefer for XML files (I recommend nxml-mode, but it doesn't work
;; in XEmacs), and then this file does the rest.

;;; Installation:

;; 1. Copy this file into your load-path where Emacs can find it.
;; 2. Make sure `mmm-mode' is installed at your site.
;; 3. Add this line to your .emacs:
;;    (require 'mmm-gxp)
;; 4. Make sure you have a working javascript.el and css-mode.el --
;;    /home/stevey/emacs/site-lisp has copies that work with GNU Emacs.
;;
;; Note:  If you use `nxml-mode' or anything other than the Emacs default
;; for editing GXP files, make sure you configure it in your
;; `auto-mode-alist' in your startup files before requiring mmm-gxp.

;;; Code:

(require 'mmm-auto)

(setq mmm-global-mode 'maybe)  ; don't enable in all buffers

;; make some more subtle region faces; mmm's are rather garish

(defface mmm-gxp-css-face '((t (:background "mint cream")))
  "Face used for background color of CSS regions in GXPs."
  :group 'mmm-faces)

(defface mmm-gxp-js-face '((t (:background "floral white")))
  "Face used for background color of JavaScript regions in GXPs."
  :group 'mmm-faces)

(defface mmm-gxp-java-face '((t (:background "old lace")))
  "Face used for background color of Java regions in GXPs."
  :group 'mmm-faces)


;;; create the GXP submode classes

(defvar mmm-gxp-js-tag-regexp
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


(defun mmm-gxp-create-group ()
  (mmm-add-group
   'gxp
   (list
    '(css-style-tag
      :submode css
      :face mmm-gxp-css-face
      :delimiter-mode nil
      :front "<style[^>]*>\\(\\s-*\n\\)?"
      :back "[ \t]*</style>")

    '(java-gxp-expr
      :submode java
      :front "\\(expr:\\w+=\\|gxp:expr[^>]+eval=\\)'"
      :face mmm-gxp-java-face
      :delimiter-mode nil
      :back "'")

    ;; should figure out how to allow both quote types.
    ;; I think MMM has a way to do this using backreferences
    ;; in :back referring to submatches in :front
    '(js-attr
      :submode javascript
      :face mmm-gxp-js-face
      :delimiter-mode nil
      :front "\\<on\\w+='"
      :back "'")

    '(js-attr-doublequote
      :submode javascript
      :face mmm-gxp-js-face
      :delimiter-mode nil
      :front "\\<on\\w+=\""
      :back "\"")

    (list
     'js-script-tag
     :submode 'javascript
     :face 'mmm-gxp-js-face
     :delimiter-mode nil
     :front mmm-gxp-js-tag-regexp
     :back"[ \t]*</script>"
     :insert '((?j js-tag nil @ "<script language=\"JavaScript\">"
                   @ "\n" _ "\n" @ "</script>" @))))))


(defun mmm-gxp-debug-reload-groups ()
  "Reloads the mmm-gxp submode group and submode classes.
Used for interactive development; you shouldn't need it unless
you're debugging `mmm-gxp.el'.  After calling this, you'll still
need to re-open your GXP file to see the effects."
  (interactive)
  (require 'cl)
  (setq mmm-classes-alist
        (remove-if (lambda (entry)
                     (memq (car entry) '(gxp
                                         css-style-tag
                                         java-gxp-expr
                                         js-script-tag)))
                   mmm-classes-alist))
  (mmm-gxp-create-group))


(defun mmm-gxp-get-preferred-xml-mode ()
  "Returns the major-mode chosen for editing XML in this Emacs session."
  (let ((pref (assoc 'xml mmm-major-mode-preferences)))
    (if pref pref
      (cdr-safe
       (find-if (lambda (entry)
                  (string-match "\\(gxp\\|xml\\)" (car entry)))
                auto-mode-alist)))))


(defun mmm-gxp-finish-init ()
  "Turns on font-lock in nxml-mode.
This is necessary because nxml-mode doesn't use font-lock.
It does its own syntax coloring.  Using font-lock-mode for
mmm-mode doesn't interfere with nxml-mode's syntax coloring."

  ;; this is unbelievably wacky, and needs to be figured
  ;; out properly.  This function seems to be getting
  ;; interrupted by something happening in parallel,
  ;; possibly a timer going off.  If we yield here, then
  ;; whatever it is does its thing, and we can continue
  ;; uninterrupted.
  (sit-for 1)

  (when (and (eq major-mode 'nxml-mode)
             (not font-lock-mode))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (set-buffer-modified-p nil)))


;;; do it.
(progn
  ;; use submode-specified background coloring
  (setq mmm-submode-decoration-level 2)

  ;; create our gxp mmm submode classes
  (mmm-gxp-create-group)

  ;; register for nxml-mode, sgml-mode, or whatever (GXPs only)
  (mmm-add-mode-ext-class
   (mmm-gxp-get-preferred-xml-mode) "\\.gxp$" 'gxp)

  ;; do more stuff
  (add-hook 'mmm-mode-hook 'mmm-gxp-finish-init))


(provide 'mmm-gxp)

;;; mmm-gxp.el ends here
