(eval-when-compile 'cl)
(defvar location-stack-max-count 1024)
(defvar location-stack-index 0)
(defvar location-stack nil)
(defvar location-stack-last nil)
(defvar location-stack-ignore nil)
(defvar location-stack-enabled t)

(defun location-stack-clear ()
  (interactive)
  (setq location-stack nil)
  (setq location-stack-ignore nil)
  (setq location-stack-last nil)
  (setq location-stack-index 0))

(defun location-stack-current-location ()
  (and (not (minibufferp))
       (cons (current-buffer) (point))))

(defun location-stack-linenumber (location)
  (with-current-buffer (car location)
    (line-number-at-pos (cdr location))))

(defun location-stack-fuzzy-compare (a b)
  (and a b (eq (car a) (car b))
       (<= (abs (- (location-stack-linenumber a) (location-stack-linenumber b))) 1)))

(defun location-stack-push ()
  (let ((added)
        (location))
    ;; (message "push called %s %d ignore %s" (buffer-file-name) (line-number-at-pos) (if location-stack-ignore "yes" "no"))
    (unless location-stack-ignore
      (setq location (location-stack-current-location))
      (when (and location
                 (not (location-stack-fuzzy-compare location (nth location-stack-index location-stack)))
                 (not (location-stack-fuzzy-compare location location-stack-last)))
        (while (> location-stack-index 0)
          (setq location-stack-index (- location-stack-index 1))
          (pop location-stack))

        (push location location-stack)
        (setq added t)
        (when (> (length location-stack) location-stack-max-count)
          (nbutlast location-stack (- (length location-stack) location-stack-max-count))))
      (setq location-stack-last location))
    added))

(defun location-stack-post-command-hook ()
  (when location-stack-enabled
    (location-stack-push)))

(if location-stack-enabled
    (add-hook 'post-command-hook 'location-stack-post-command-hook)
  (remove-hook 'post-command-hook 'location-stack-post-command-hook))

(defun location-stack-goto-location (location)
  (when (buffer-live-p (car location))
    (setq location-stack-ignore t)
    (let ((windows (window-list)))
      (while (and windows
                  (not (eq (window-buffer (car windows))
                           (car location))))
        (setq windows (cdr windows)))
      (if windows
          (select-window (car windows))
        (switch-to-buffer (car location))))
    (goto-char (cdr location))
    (setq location-stack-last location)
    (setq location-stack-ignore nil)))

(defun location-stack-location-to-string (idx)
  (let ((info (nth idx location-stack)))
    (when (buffer-live-p (car info))
      (with-current-buffer (car info)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (cdr info))
            (format "%s:%d:%d:" (buffer-name) (line-number-at-pos (point)) (1+ (- (point) (point-at-bol))))))))))

(defun location-stack-filter-dead-locations ()
  (let* ((cur location-stack-index)
         (stack (cl-remove-if (lambda (item) (not (buffer-live-p (car item)))) location-stack))
         (curitem))
    (while (and (> cur 0)
                (not (buffer-live-p (car (nth cur location-stack)))))
      (cl-decf cur))
    (setq curitem (nth cur location-stack))
    (when curitem
      (let ((idx 0)
            (s stack))
        (while s
          (if (eq curitem (car s))
              (setq location-stack-index idx
               s nil)
            (cl-incf idx)
            (setq s (cdr s))))))
    (setq location-stack stack)))

(defun location-stack-jump (by)
  (interactive)
  (location-stack-filter-dead-locations)
  (let ((instack (nth location-stack-index location-stack))
        (cur (location-stack-current-location)))
    (if (not (equal instack cur))
        (location-stack-goto-location instack)
      (let ((target (+ location-stack-index by)))
        (when (and (>= target 0) (< target (length location-stack)))
          (setq location-stack-index target)
          (location-stack-goto-location (nth location-stack-index location-stack))))))
  (location-stack-print))

(defun location-stack-print ()
  (let* ((msgs)
         (last (min (1- (length location-stack))
                    (+ location-stack-index 3)))
         (idx (max 0 (- location-stack-index 3)))
         (lastmsg (and (> idx 0)
                       (format "...%d" idx))))
    (when lastmsg
      (push lastmsg msgs))
    ;; (when (> (1- (length location-stack)) last)
    ;;   (push (format "%d..." (- (1- (length location-stack)) last)) msgs))
    (while (<= idx last)
      (let ((str (location-stack-location-to-string idx)))
        (if (= idx location-stack-index)
            (push (format  "%d: <%s>" idx str) msgs)
          (push (format "%d: %s" idx str) msgs)))
      (cl-incf idx))
    (message "%s" (mapconcat 'identity msgs " "))))

(defun location-stack-next ()
  (interactive)
  (location-stack-jump -1))

(defun location-stack-previous ()
  (interactive)
  (location-stack-jump 1))

(defun location-stack-toggle ()
  (interactive)
  (location-stack-push)
  (if (= location-stack-index 0)
      (location-stack-previous)
    (location-stack-jump (- location-stack-index))))

(provide 'location-stack)
