;;; comint-9term.el --- Advanced ANSI escape sequences for comint-mode

;; Restriction: do not add `(require 'ansi-color)`
;; Restriction: do not add `(provide 'comint-9term)`

(defvar-local comint-9term-saved-pos nil)
(defvar-local comint-9term-scroll-bottom nil)
(defvar-local comint-9term-lines-below-scroll 0)
(defvar-local comint-9term-virtual-col nil)
(defvar-local comint-9term-partial-seq "")
(defvar-local comint-9term-height-override nil)
(defvar-local comint-9term-origin nil)

(defun comint-9term-parse-params (params-str &optional default)
  (setq default (or default 1))
  (if (or (not params-str) (string= params-str ""))
      (list default)
    (let ((params (mapcar (lambda (s) (if (string= s "") default (string-to-number s)))
                          (split-string params-str ";"))))
      params)))

(defun comint-9term-max-height ()
  (or comint-9term-height-override
      (let ((env-lines (getenv "LINES")))
        (if env-lines
            (string-to-number env-lines)
          (condition-case nil
              (frame-height)
            (error 24))))))

(defun comint-9term-start-line ()
  (let ((total (line-number-at-pos (point-max)))
        (height (comint-9term-max-height))
        (origin (or comint-9term-origin 1)))
    (max (1- origin) (if (> total height) (- total height) 0))))

(defun comint-9term-handle-csi (char params)
  (let ((n (or (nth 0 params) 1))
        (m (or (nth 1 params) 1))
        (max-h (comint-9term-max-height)))
    (cond
     ((eq char ?A) ; CUU - Cursor Up
      (let ((col (or comint-9term-virtual-col (current-column))))
        (forward-line (- (max 1 n)))
        (move-to-column col nil)
        (if (< (current-column) col)
            (setq comint-9term-virtual-col col)
          (setq comint-9term-virtual-col nil))))
     ((eq char ?B) ; CUD - Cursor Down
      (let ((col (or comint-9term-virtual-col (current-column))))
        (forward-line (max 1 n))
        (move-to-column col nil)
        (if (< (current-column) col)
            (setq comint-9term-virtual-col col)
          (setq comint-9term-virtual-col nil))))
     ((eq char ?C) ; CUF - Cursor Forward
      (let ((curr (or comint-9term-virtual-col (current-column))))
        (let ((target (+ curr (max 1 n))))
          (move-to-column target nil)
          (if (< (current-column) target)
              (setq comint-9term-virtual-col target)
            (setq comint-9term-virtual-col nil)))))
     ((eq char ?D) ; CUB - Cursor Backward
      (let ((curr (or comint-9term-virtual-col (current-column))))
        (let ((target (max 0 (- curr (max 1 n)))))
          (setq comint-9term-virtual-col nil)
          (move-to-column target nil))))
     ((eq char ?F) ; CPL - Cursor Previous Line
      (let ((inhibit-field-text-motion t))
        (forward-line (- (max 1 n)))
        (beginning-of-line)
        (setq comint-9term-virtual-col nil)))
     ((eq char ?G) ; CHA - Cursor Horizontal Absolute
      (let ((target (1- (max 1 n))))
        (move-to-column target nil)
        (if (< (current-column) target)
            (setq comint-9term-virtual-col target)
          (setq comint-9term-virtual-col nil))))
     ((or (eq char ?H) (eq char ?f)) ; CUP / HVP - Cursor Position
      (setq n (min n max-h))
      (let* ((start-line (comint-9term-start-line))
             (target-line (+ start-line (max 1 n))))
        (goto-char (point-min))
        (let ((lines-left (forward-line (1- (max 1 target-line)))))
          (when (> lines-left 0)
            (insert (make-string lines-left ?\n)))))
      (let ((target (1- (max 1 m))))
        (move-to-column target nil)
        (if (< (current-column) target)
            (setq comint-9term-virtual-col target)
          (setq comint-9term-virtual-col nil))))
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
        (when comint-9term-virtual-col
           (let ((gap (- comint-9term-virtual-col (current-column))))
             (when (> gap 0) (insert (make-string gap ?\s))))
           (setq comint-9term-virtual-col nil)
           (setq cur (point))
           (setq end (line-end-position)))
        (cond
         ((= n 0) (delete-region cur end))
         ((= n 1)
          (let ((target (min (1+ cur) end)))
            (delete-region beg target)
            (insert (make-string (- target beg) ?\s))))
         ((= n 2) (delete-region beg end)))))
     ((eq char ?r) ; DECSTBM - Set Scrolling Region
      (let ((bottom (nth 1 params)))
        (when (and bottom (> n 0))
          (setq bottom (min bottom max-h))
          (setq comint-9term-scroll-bottom bottom)
          (setq comint-9term-lines-below-scroll 1)))))))

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
                (progn (end-of-line) (insert "\n"))
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
                (if (<= new-col (current-column))
                    (progn (setq comint-9term-virtual-col nil) (move-to-column new-col))
                  (setq comint-9term-virtual-col new-col)))
            (if (> (current-column) 0) (backward-char 1))))
         (t
          (when comint-9term-virtual-col
             (let ((gap (- comint-9term-virtual-col (current-column))))
               (when (> gap 0) (insert (make-string gap ?\s))))
             (setq comint-9term-virtual-col nil))
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
                (when (string-match "\033" string)
                  (setq comint-9term-origin (1- (line-number-at-pos (process-mark proc))))))

              ;; Prepend any partial sequence from previous run
              (when (and comint-9term-partial-seq (> (length comint-9term-partial-seq) 0))
                (setq string (concat comint-9term-partial-seq string))
                (setq comint-9term-partial-seq ""))

              (comint-watch-for-password-prompt string)

              (while (string-match "\033\\(\\[\\([?0-9;]*\\)\\([A-Za-z]\\)\\|]\\(?:.*?\\)\\(?:\007\\|\033\\\\\\)\\|\\([78]\\)\\)" string start)
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
                (if (string-match "\033" remainder)
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
  (make-local-variable 'comint-9term-virtual-col)
  (make-local-variable 'comint-9term-partial-seq)
  (make-local-variable 'comint-9term-height-override)
  (add-hook 'comint-preoutput-filter-functions 'comint-9term-filter nil t)
  
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (setq-local comint-output-filter-functions
                          (remq 'ansi-color-process-output comint-output-filter-functions)))
            nil t))

(add-hook 'comint-mode-hook 'comint-9term-setup)
(add-hook 'compilation-shell-minor-mode-hook 'comint-9term-setup)
