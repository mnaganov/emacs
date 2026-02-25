;;; comint-9term.el --- Advanced ANSI escape sequences for comint-mode

;; Restriction: do not add `(require 'ansi-color)`
;; Restriction: do not add `(provide 'comint-9term)`

(defvar-local comint-9term-saved-pos nil)
(defvar-local comint-9term-scroll-bottom nil)
(defvar-local comint-9term-lines-below-scroll 0)
(defvar-local comint-9term-scroll-offset 0)
(defvar-local comint-9term-virtual-col nil)
(defvar-local comint-9term-partial-seq "")
(defvar-local comint-9term-height-override nil)
(defvar-local comint-9term-origin nil)
(defvar-local comint-9term-term-height nil)

(defconst comint-9term-control-seq-regexp
  "\e\\(\\[\\([?0-9;]*\\)\\([A-Za-z]\\)\\|]\\(?:.*?\\)\\(?:\a\\|\e\\\\\\)\\|\\([78]\\)\\)"
  "Regexp matching supported ANSI escape sequences.")

(defun comint-9term-parse-params (params-str &optional default)
  (setq default (or default 1))
  (if (or (not params-str) (string= params-str ""))
      (list default)
    (let ((params (mapcar (lambda (s) (if (string= s "") default (string-to-number s)))
                          (split-string params-str ";"))))
      params)))

(defun comint-9term-max-height ()
  (or comint-9term-height-override
      comint-9term-term-height
      (let ((env-lines (getenv "LINES")))
        (if env-lines
            (string-to-number env-lines)
          (condition-case nil
              (frame-height)
            (error 24))))))

(defun comint-9term-start-line ()
  (let* ((total (line-number-at-pos (point-max)))
         (max-h (comint-9term-max-height))
         (effective-h (if (eq comint-9term-scroll-bottom 1) 1 max-h))
         (origin (or comint-9term-origin 1)))
    (max (1- origin)
         comint-9term-scroll-offset
         (if (> total effective-h) (- total effective-h) 0))))

(defun comint-9term-pad-to-virtual-col ()
  "Insert spaces if virtual column is set, filling the gap."
  (when comint-9term-virtual-col
    (let ((gap (- comint-9term-virtual-col (current-column))))
      (when (> gap 0)
        (insert (make-string gap ?\s))))
    (setq comint-9term-virtual-col nil)))

(defun comint-9term-move-to-column (col)
  "Move to column COL, setting virtual column if target is unreachable."
  (move-to-column col nil)
  (if (< (current-column) col)
      (setq comint-9term-virtual-col col)
    (setq comint-9term-virtual-col nil)))

(defun comint-9term-handle-csi (char params)
  (let ((n (or (nth 0 params) 1))
        (m (or (nth 1 params) 1))
        (max-h (comint-9term-max-height)))
    (cond
     ((eq char ?A) ; CUU - Cursor Up
      (let ((col (or comint-9term-virtual-col (current-column))))
        (forward-line (- (max 1 n)))
        (comint-9term-move-to-column col)))
     ((eq char ?B) ; CUD - Cursor Down
      (let ((col (or comint-9term-virtual-col (current-column))))
        (forward-line (max 1 n))
        (comint-9term-move-to-column col)))
     ((eq char ?C) ; CUF - Cursor Forward
      (let ((curr (or comint-9term-virtual-col (current-column))))
        (comint-9term-move-to-column (+ curr (max 1 n)))))
     ((eq char ?D) ; CUB - Cursor Backward
      (let ((curr (or comint-9term-virtual-col (current-column))))
        (comint-9term-move-to-column (max 0 (- curr (max 1 n))))))
     ((eq char ?F) ; CPL - Cursor Previous Line
      (let ((inhibit-field-text-motion t))
        (forward-line (- (max 1 n)))
        (beginning-of-line)
        (setq comint-9term-virtual-col nil)))
     ((eq char ?G) ; CHA - Cursor Horizontal Absolute
      (comint-9term-move-to-column (1- (max 1 n))))
     ((or (eq char ?H) (eq char ?f)) ; CUP / HVP - Cursor Position
      (setq n (min n max-h))
      (let* ((start-line (comint-9term-start-line))
             (target-line (+ start-line (max 1 n))))
        (goto-char (point-min))
        (let ((lines-left (forward-line (1- (max 1 target-line)))))
          (when (> lines-left 0)
            (insert (make-string lines-left ?\n)))))
      (comint-9term-move-to-column (1- (max 1 m))))
     ((eq char ?J) ; ED - Erase in Display
      (setq comint-9term-virtual-col nil)
      (cond
       ((= n 0) (delete-region (point) (point-max)))
       ((= n 1) (delete-region (point-min) (point)))
       ((= n 2) (delete-region (point-min) (point-max)))))
     ((eq char ?K) ; EL - Erase in Line
      (let ((beg (line-beginning-position))
            (end (line-end-position))
            (cur (point)))
        (comint-9term-pad-to-virtual-col)
        (setq cur (point))
        (setq end (line-end-position))
        (cond
         ((= n 0) (delete-region cur end))
         ((= n 1)
          (let ((target (min (1+ cur) end)))
            (delete-region beg target)
            (insert (make-string (- target beg) ?\s))))
         ((= n 2) (delete-region beg end)))))
     ((eq char ?r) ; DECSTBM - Set Scrolling Region
      (let ((bottom (nth 1 params)))
        (if (and bottom (> n 0))
            (progn
              (setq bottom (min bottom max-h))
              (setq comint-9term-scroll-bottom bottom)
              (setq comint-9term-lines-below-scroll 1))
          (setq comint-9term-scroll-bottom nil)
          (setq comint-9term-lines-below-scroll 0)))))))

(defun comint-9term-insert-and-overwrite (text)
  "Insert TEXT at process-mark."
  (when (> (length text) 0)
    (let* ((proc (get-buffer-process (current-buffer)))
           (pm (process-mark proc)))
      (goto-char pm)
      (dolist (c (append text nil))
        (cond
         ((eq c ?\n)
          (setq comint-9term-virtual-col nil)
          (let* ((should-scroll
                  (and (> comint-9term-lines-below-scroll 0)
                       (let* ((start (comint-9term-start-line))
                              (current (line-number-at-pos)))
                         (>= (- current start) comint-9term-scroll-bottom)))))
            (if should-scroll
                (progn
                  (end-of-line)
                  (insert "\n")
                  (setq comint-9term-scroll-offset (1+ comint-9term-scroll-offset)))
              (let ((lines-left (forward-line 1)))
                (if (> lines-left 0)
                    (insert "\n")
                  (if (and (eobp) (not (eq (char-before) ?\n)))
                      (insert "\n")))))))
         ((eq c ?\r)
          (setq comint-9term-virtual-col nil)
          (beginning-of-line))
         ((eq c ?\b)
          (if comint-9term-virtual-col
              (let ((new-col (max 0 (1- comint-9term-virtual-col))))
                (comint-9term-move-to-column new-col))
            (if (> (current-column) 0) (backward-char 1))))
         (t
          (comint-9term-pad-to-virtual-col)
          (while (and (not (eobp)) (get-text-property (point) 'invisible))
            (delete-char 1))
          (if (and (not (eobp)) (not (eq (following-char) ?\n)))
              (delete-char 1))
          (insert (char-to-string c))))
        (set-marker pm (point))))))

(defun comint-9term-filter (string)
  (condition-case err
      (let ((proc (get-buffer-process (current-buffer))))
        (if (not proc)
            string
          (with-current-buffer (process-buffer proc)
            (let ((inhibit-read-only t)
                  (inhibit-field-text-motion t)
                  (start 0)
                  (min-p (process-mark proc)))
              ;; Check for LINES= override
              (when (string-match "LINES=\\([0-9]+\\)" string)
                (setq comint-9term-height-override (string-to-number (match-string 1 string))))
              
              ;; Initialize origin if needed
              (unless comint-9term-origin
                (when (string-match "\e" string)
                  (setq comint-9term-origin (1- (line-number-at-pos (process-mark proc))))))

              ;; Prepend any partial sequence from previous run
              (when (and comint-9term-partial-seq (> (length comint-9term-partial-seq) 0))
                (setq string (concat comint-9term-partial-seq string))
                (setq comint-9term-partial-seq ""))

              (comint-watch-for-password-prompt string)

              (while (string-match comint-9term-control-seq-regexp string start)
                (let* ((pre-text (substring string start (match-beginning 0)))
                       (is-csi (match-beginning 2))
                       (is-sc (match-beginning 4))
                       (seq-end (match-end 0)))
                  (comint-9term-insert-and-overwrite pre-text)
                  (setq min-p (min min-p (point)))
                  (cond
                   (is-csi
                    (let ((char (aref (match-string 3 string) 0))
                          (params (match-string 2 string)))
                      (cond
                       ((memq char '(?A ?B ?C ?D ?F ?G ?H ?f ?J ?K ?r))
                        (comint-9term-handle-csi char (comint-9term-parse-params params (if (memq char '(?J ?K)) 0 1))))
                       ((eq char ?m)
                        (let ((start (point))
                              (sgr (match-string 0 string)))
                          (insert sgr)
                          (when (fboundp 'ansi-color-apply-on-region)
                            (ansi-color-apply-on-region start (point))))))))
                   (is-sc
                    (let ((esc-char (aref (match-string 4 string) 0)))
                      (cond
                       ((eq esc-char ?7)
                        (let ((m (point-marker)))
                          (set-marker-insertion-type m nil)
                          (setq comint-9term-saved-pos m)))
                       ((eq esc-char ?8) (when comint-9term-saved-pos (goto-char comint-9term-saved-pos)))))))
                  (setq min-p (min min-p (point)))
                  (set-marker (process-mark proc) (point))
                  (setq start seq-end)))

              ;; Handle remainder and check for partial sequence
              (let ((remainder (substring string start)))
                (if (string-match "\e" remainder)
                    (let ((esc-idx (match-beginning 0)))
                      (comint-9term-insert-and-overwrite (substring remainder 0 esc-idx))
                      (setq comint-9term-partial-seq (substring remainder esc-idx)))
                  (comint-9term-insert-and-overwrite remainder)
                  (setq comint-9term-partial-seq "")))

              (setq min-p (min min-p (point)))
              (set-marker (process-mark proc) (point))))))
    (error (message "Filter error: %S" err) nil))
  "")

(defun comint-9term-setup ()
  (make-local-variable 'comint-9term-saved-pos)
  (make-local-variable 'comint-9term-scroll-bottom)
  (make-local-variable 'comint-9term-lines-below-scroll)
  (make-local-variable 'comint-9term-scroll-offset)
  (make-local-variable 'comint-9term-virtual-col)
  (make-local-variable 'comint-9term-partial-seq)
  (make-local-variable 'comint-9term-height-override)
  (make-local-variable 'comint-9term-term-height)
  (add-function :filter-return
                (local 'window-adjust-process-window-size-function)
                (lambda (size)
                  (when size
                    (setq comint-9term-term-height (cdr size)))
                  size)
                '((name . comint-9term-resize)))
  (add-hook 'comint-preoutput-filter-functions 'comint-9term-filter nil t)
  
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (setq-local comint-output-filter-functions
                          (remq 'ansi-color-process-output comint-output-filter-functions)))
            nil t))

;; Trace mode

(defvar comint-9term-trace-buffer nil
  "Buffer to store trace logs.")

(defun comint-9term-trace-filter (string)
  "Log STRING to trace buffer if active."
  (when (buffer-live-p comint-9term-trace-buffer)
    (with-current-buffer comint-9term-trace-buffer
      (goto-char (point-max))
      (insert (format ";; Chunk len=%d\n" (length string)))
      (prin1 string (current-buffer))
      (insert "\n")))
  string)

(define-minor-mode comint-9term-trace-mode
  "Toggle tracing of comint output."
  :global nil
  (if comint-9term-trace-mode
      (progn
        (unless (buffer-live-p comint-9term-trace-buffer)
          (setq comint-9term-trace-buffer (get-buffer-create "*comint-9term-trace*"))
          (with-current-buffer comint-9term-trace-buffer
            (erase-buffer)
            (emacs-lisp-mode)
            (insert (format ";; Trace started at %s\n" (current-time-string)))
            (let ((h (or comint-9term-term-height (frame-height))))
               (insert (format "(setq comint-9term-height-override %S)\n" h)))
            (insert (format "(setq width %S)\n" (window-width)))))
        (add-hook 'comint-preoutput-filter-functions 'comint-9term-trace-filter nil t))
    (remove-hook 'comint-preoutput-filter-functions 'comint-9term-trace-filter t)))

(defun comint-9term-replay-trace (file)
  "Replay trace from FILE into current buffer."
  (let ((proc (start-process "trace-replay" (current-buffer) "cat")))
    (set-process-query-on-exit-flag proc nil)
    (let ((trace-data (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string))))
      (with-temp-buffer
        (insert trace-data)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((sexp (condition-case nil
                          (read (current-buffer))
                        (end-of-file nil))))
            (when sexp
              (with-current-buffer (process-buffer proc)
                (if (stringp sexp)
                    (comint-9term-filter sexp)
                  (eval sexp))))))))))

(add-hook 'comint-mode-hook 'comint-9term-setup)
(add-hook 'compilation-shell-minor-mode-hook 'comint-9term-setup)
