;;; comint-9term.el --- Advanced ANSI escape sequences for comint-mode -*- lexical-binding: t -*-

(require 'comint)
(require 'compile)

(defvar comint-9term-trace-buffer nil
  "Buffer to store trace logs.")

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

(defun comint-9term-insert-and-overwrite (text &optional start end)
  "Insert TEXT at process-mark, from START to END."
  (let* ((start (or start 0))
         (end (or end (length text)))
         (len (- end start))
         (idx start)
         (proc (get-buffer-process (current-buffer))))
    (when (and proc (> len 0))
      (let ((pm (process-mark proc)))
        (goto-char pm)
        (while (< idx end)
          (let ((c (aref text idx)))
            (cond
             ((eq c ?\n)
              (setq comint-9term-virtual-col nil)
              (if (and (> comint-9term-lines-below-scroll 0)
                       (>= (- (line-number-at-pos) (comint-9term-start-line)) comint-9term-scroll-bottom))
                  (progn
                    (end-of-line)
                    (insert "\n")
                    (setq comint-9term-scroll-offset (1+ comint-9term-scroll-offset)))
                (let ((lines-left (forward-line 1)))
                  (if (> lines-left 0)
                      (insert "\n")
                    (if (and (eobp) (not (eq (char-before) ?\n)))
                        (insert "\n")))))
              (set-marker pm (point)))
             ((eq c ?\r)
              (setq comint-9term-virtual-col nil)
              (beginning-of-line)
              (set-marker pm (point)))
             ((eq c ?\b)
              (if comint-9term-virtual-col
                  (let ((new-col (max 0 (1- comint-9term-virtual-col))))
                    (comint-9term-move-to-column new-col))
                (if (> (current-column) 0) (backward-char 1)))
              (set-marker pm (point)))
             (t
              (let ((pos (string-match-p "[\n\r\b]" text idx)))
                (let ((chunk-end (if (and pos (< pos end)) pos end)))
                  (comint-9term-write-chunk (substring text idx chunk-end))
                  (setq idx (1- chunk-end))
                  (set-marker pm (point))))))
            (setq idx (1+ idx))))))))

(defun comint-9term-write-chunk (chunk)
  "Write a chunk of normal text, using fast path if possible."
  (comint-9term-pad-to-virtual-col)
  (let ((len (length chunk))
        (p (point)))
    (let ((end-of-line (line-end-position)))
      (if (>= p end-of-line)
          (insert chunk)
        ;; Overwrite: delete existing characters up to the chunk length or EOL.
        (let ((to-delete (min len (- end-of-line p))))
          (delete-region p (+ p to-delete))
          (insert chunk))))))

(defun comint-9term-filter (string)
  (condition-case err
      (let ((proc (get-buffer-process (current-buffer))))
        (if (not proc)
            string
          (with-current-buffer (process-buffer proc)
            (let ((inhibit-read-only t)
                  (inhibit-field-text-motion t)
                  (start 0)
                  (min-p (marker-position (process-mark proc)))
                  (max-p (marker-position (process-mark proc))))
              ;; Check for LINES= override
              (when (string-match "LINES=\\([0-9]+\\)" string)
                (setq comint-9term-height-override (string-to-number (match-string 1 string))))

              ;; Initialize origin if needed
              (unless comint-9term-origin
                (setq comint-9term-origin (1- (line-number-at-pos (process-mark proc)))))

              (sleep-for 0) ; Process signals to prevent massive chunk buffering

              ;; Prepend any partial sequence from previous run
              (when (and comint-9term-partial-seq (> (length comint-9term-partial-seq) 0))
                (when (buffer-live-p comint-9term-trace-buffer)
                  (with-current-buffer comint-9term-trace-buffer
                    (goto-char (point-max))
                    (insert ";; PARTIAL_SEQ_SAVED\n")))
                (setq string (concat comint-9term-partial-seq string))
                (setq comint-9term-partial-seq ""))

              (comint-watch-for-password-prompt string)

              (while (string-match comint-9term-control-seq-regexp string start)
                (let* ((pre-end (match-beginning 0))
                       (is-csi (match-beginning 2))
                       (is-sc (match-beginning 4))
                       (seq-end (match-end 0)))
                  (comint-9term-insert-and-overwrite string start pre-end)
                  (setq min-p (min min-p (point)))
                  (setq max-p (max max-p (point)))
                  (cond
                   (is-csi
                    (let ((char (aref (match-string 3 string) 0))
                          (params (match-string 2 string)))
                      (cond
                       ((memq char '(?A ?B ?C ?D ?F ?G ?H ?f ?J ?K ?r))
                        (comint-9term-handle-csi char (comint-9term-parse-params params (if (memq char '(?J ?K)) 0 1))))
                       ((eq char ?m)
                        (let ((start-p (point))
                              (sgr (match-string 0 string)))
                          (insert sgr)
                          (when (fboundp 'ansi-color-apply-on-region)
                            (ansi-color-apply-on-region start-p (point))))))))
                   (is-sc
                    (let ((esc-char (aref (match-string 4 string) 0)))
                      (cond
                       ((eq esc-char ?7)
                        (let ((m (point-marker)))
                          (set-marker-insertion-type m nil)
                          (setq comint-9term-saved-pos m)))
                       ((eq esc-char ?8) (when comint-9term-saved-pos (goto-char comint-9term-saved-pos)))))))
                  (setq min-p (min min-p (point)))
                  (setq max-p (max max-p (point)))
                  (set-marker (process-mark proc) (point))
                  (setq start seq-end)))

              ;; Handle remainder and check for partial sequence
              (if (string-match "\e" string start)
                  (let ((esc-idx (match-beginning 0)))
                    (comint-9term-insert-and-overwrite string start esc-idx)
                    (setq comint-9term-partial-seq (substring string esc-idx)))
                (comint-9term-insert-and-overwrite string start)
                (setq comint-9term-partial-seq ""))

              (setq min-p (min min-p (point)))
              (setq max-p (max max-p (point)))
              (set-marker (process-mark proc) (point))

              (let ((clamped-max (min max-p (point-max))))
                (when (and (fboundp 'comint--mark-as-output)
                           (not comint-use-prompt-regexp)
                           (< min-p clamped-max))
                  (comint--mark-as-output min-p clamped-max)))))))
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

(provide 'comint-9term)

;;; comint-9term.el ends here
