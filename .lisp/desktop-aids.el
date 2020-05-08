;; ;; To enable:
;; (require 'desktop-aids)

(require 'desktop)

(defcustom desktop-aids-max-buffers 50 "Max buffers to store" :group 'desktop-aids :type 'number)
(defvar desktop-aids-save-buffers nil)
(defun desktop-aids-setup-buffers (orig-fun &rest args)
  (setq desktop-aids-save-buffers (make-hash-table :test 'equal :size desktop-aids-max-buffers))
  (let ((count desktop-aids-max-buffers)
        (buffers (buffer-list)))
    (while (and buffers (> count 0))
      (let ((name (buffer-file-name (car buffers))))
        (when name
          (setq count (1- count))
          (puthash name t desktop-aids-save-buffers)))
      (setq buffers (cdr buffers))))
  (apply orig-fun args)
  (setq desktop-aids-save-buffers nil))

(defun desktop-aids-save-buffer-p (orig-fun &rest args)
  (and (gethash (car args) desktop-aids-save-buffers)
       (apply orig-fun args)))
(advice-add 'desktop-save-buffer-p :around 'desktop-aids-save-buffer-p)
(advice-add 'desktop-save :around #'desktop-aids-setup-buffers)

(provide 'desktop-aids)
