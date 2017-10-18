;; ;; To enable:
;; (require 'desktop-aids)
;; (add-to-list 'desktop-buffer-mode-handlers '(c-mode . desktop-aids-lazy-handler))
;; (add-to-list 'desktop-buffer-mode-handlers '(c++-mode . desktop-aids-lazy-handler))
;; (add-to-list 'desktop-buffer-mode-handlers '(js2-mode . desktop-aids-lazy-handler))
;; (add-to-list 'desktop-buffer-mode-handlers '(js3-mode . desktop-aids-lazy-handler))
;; (desktop-read)
;; (desktop-save-mode 1)

(defvar-local --desktop-aids-pending nil)
;; (setq desktop-aids-buffer-filter (lambda (buffer filename) (string-match "rtags.el$" filename)))

(defun --desktop-aids-post-command-hook ()
  (when (buffer-local-value '--desktop-aids-pending (current-buffer))
    ;; (message "hook called")
    (kill-local-variable '--desktop-aids-pending)
    (remove-hook 'post-command-hook '--desktop-aids-post-command-hook t)
    (set-auto-mode)))

(defun desktop-aids-lazy-handler (filename buffername misc-data)
  ;; (message "GOT CALLED %s %s %s" filename buffername misc-data)
  (when filename
    (if (not (file-exists-p filename))
        (message "desktop-aids: \"%s\" no longer exists" filename)
      (let ((buffer (get-buffer-create buffername)))
        ;; (condition-case nil
        (with-current-buffer buffer
          (insert-file-contents-literally filename)
          (setq buffer-file-name filename)
          (setq default-directory (file-name-directory filename))
          (set-buffer-modified-p nil)
          (setq --desktop-aids-pending t)
          (add-hook 'post-command-hook '--desktop-aids-post-command-hook nil t)
          (current-buffer))))))
;; (error nil))))))

(provide 'desktop-aids)
