;; ;; To enable:
;; (require 'desktop-aids)
;; (add-to-list 'desktop-buffer-mode-handlers '(c-mode . desktop-aids-lazy-handler))
;; (add-to-list 'desktop-buffer-mode-handlers '(c++-mode . desktop-aids-lazy-handler))
;; (add-to-list 'desktop-buffer-mode-handlers '(js2-mode . desktop-aids-lazy-handler))
;; (add-to-list 'desktop-buffer-mode-handlers '(js3-mode . desktop-aids-lazy-handler))
;; (desktop-read)
;; (desktop-save-mode 1)

(defvar-local --desktop-aids-pending nil)
(defun --desktop-aids-post-command-hook ()
  (when (buffer-local-value '--desktop-aids-pending (current-buffer))
    ;; (message "hook called")
    (kill-local-variable '--desktop-aids-pending)
    (remove-hook 'post-command-hook '--desktop-aids-post-command-hook t)
    (set-auto-mode)))

(defun desktop-aids-lazy-handler (filename buffername misc-data)
  ;; (message "got called %s %s" filename buffername)
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
            (insert-file-contents-literally filename)
            (setq buffer-file-name filename)
            (setq default-directory (file-name-directory filename))
            (set-buffer-modified-p nil))
          (setq --desktop-aids-pending t)
          (add-hook 'post-command-hook '--desktop-aids-post-command-hook nil t)
          (current-buffer))))))

(provide 'desktop-aids)
