(defvar buffer-local-mode nil)
(make-variable-buffer-local 'buffer-local-mode)

(defun mode-keymap (mode-sym)
  (symbol-value (intern (concat (symbol-name mode-sym) "-map"))))

(defun* buffer-local-set-key (key action)
        (when buffer-local-mode
          (define-key (mode-keymap buffer-local-mode)
                      key action)
          (return-from buffer-local-set-key))
        (let* ((mode-name-loc (gensym "-blm")))
          (eval `(define-minor-mode ,mode-name-loc nil nil nil (make-sparse-keymap)))
          (setq buffer-local-mode mode-name-loc)
          (funcall mode-name-loc 1)
          (define-key (mode-keymap mode-name-loc) key action)))
(provide 'buffer-local-mode)
;;(buffer-local-set-key (kbd "h") (lambda () (interactive) (message "hello")))
