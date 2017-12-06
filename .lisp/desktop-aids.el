;; ;; To enable:
;; (require 'desktop-aids)
;; (add-to-list 'desktop-aids-modes 'c-mode)
;; (add-to-list 'desktop-aids-modes 'c++-mode)
;; (add-to-list 'desktop-aids-modes 'js2-mode)
;; (add-to-list 'desktop-aids-modes 'js3-mode)
;; (desktop-aids-mode t)

;; (message "initializing desktop")

(defvar-local --desktop-aids-pending nil)
(defun desktop-aids-sync-buffer ()
  (when (buffer-local-value '--desktop-aids-pending (current-buffer))
    ;; (message "hook called")
    (kill-local-variable '--desktop-aids-pending)
    (remove-hook 'post-command-hook 'desktop-aids-sync-buffer t)
    (if (file-exists-p (buffer-file-name))
        (find-alternate-file (buffer-file-name))
      (message "File has disappeared: %s" (buffer-file-name)_)
      (kill-buffer (current-buffer)))))

(define-derived-mode desktop-aids-lazy-mode nil "desktop-aids-lazy"
  "Good mode"
  (setq --desktop-aids-pending t)
  (add-hook 'post-command-hook 'desktop-aids-sync-buffer nil t))

(defvar --desktop-aids-buffer-list-updated-timer nil)
(defun --desktop-aids-on-buffer-list-updated-timer (&optional arg1 arg2)
  (setq --desktop-aids-buffer-list-updated-timer nil)
  (desktop-aids-sync-buffer))
  ;; (kill-local-variable '--desktop-aids-pending)
  ;; (remove-hook 'post-command-hook 'desktop-aids-post-command-hook t)
  ;; (set-auto-mode))
  ;; (message "timer fired %s" (buffer-name)))

(defun --desktop-aids-buffer-list-update-hook ()
  ;; (message "Called %s" (buffer-name)))
  (unless --desktop-aids-buffer-list-updated-timer
    (setq --desktop-aids-buffer-list-updated-timer
          (run-with-idle-timer .5 t '--desktop-aids-on-buffer-list-updated-timer nil t))))

  ;; (message "hook called %s" (buffer-name))
  ;; (when (buffer-local-value '--desktop-aids-pending (current-buffer))
  ;;   (kill-local-variable '--desktop-aids-pending)
  ;;   (set-auto-mode)))

(defun desktop-aids-lazy-handler (filename buffername misc-data)
;;  (message "got called %s %s" filename buffername)
  (when filename
    (if (not (file-exists-p filename))
        (message "desktop-aids: \"%s\" no longer exists" filename)
      (let* ((existing (get-buffer buffername))
             (buffer (or existing (get-buffer-create buffername))))
        ;; (message "balls %s %s %s" filename existing buffer)
        (with-current-buffer buffer
          ;; (when existing
          ;;   (message "had existing %s %s %d %s" buffername filename (point-max) major-mode))
          (when (not existing)
            ;; (message "recreated %s %s" buffername filename)
            (setq buffer-file-name filename)
            (setq default-directory (file-name-directory filename))
            (set-buffer-modified-p nil))
          (desktop-aids-lazy-mode)
          (current-buffer))))))

(defvar desktop-aids-modes nil)
(define-minor-mode desktop-aids-mode "Good minor mode"
  :init-value nil
  (if desktop-aids-mode
      (progn
        (add-to-list 'desktop-buffer-mode-handlers '(desktop-aids-lazy-mode . desktop-aids-lazy-handler))
        (dolist (mode desktop-aids-modes)
          (add-to-list 'desktop-buffer-mode-handlers `(,mode . desktop-aids-lazy-handler)))
        (desktop-read)
        (desktop-save-mode 1)
        (add-hook 'next-error-hook 'desktop-aids-sync-buffer)
        (when (boundp 'rtags-after-find-file-hook)
          (add-hook 'rtags-after-find-file-hook 'desktop-aids-sync-buffer))

        (desktop-aids-sync-buffer))
        ;; (add-hook 'buffer-list-update-hook '--desktop-aids-buffer-list-update-hook))
    ;; (remove-hook 'buffer-list-update-hook '--desktop-aids-buffer-list-update-hook)
    (remove-hook 'next-error-hook 'desktop-aids-sync-buffer)
    (when (boundp 'rtags-after-find-file-hook)
      (remove-hook 'rtags-after-find-file-hook 'desktop-aids-sync-buffer))
    (desktop-save-mode nil)))

(provide 'desktop-aids)
