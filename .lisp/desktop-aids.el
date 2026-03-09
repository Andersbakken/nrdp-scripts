;;; desktop-aids.el --- Desktop buffer management  -*- lexical-binding: t; -*-
;; ;; To enable:
;; (require 'desktop-aids)

(require 'desktop)

(defcustom desktop-aids-max-buffers 50 "Max buffers to store" :group 'desktop-aids :type 'number)
(defvar desktop-aids-save-buffers nil)
;; Frame size save/restore (in characters to avoid WM decoration shrinkage)
(defvar desktop-aids-frame-size nil "Saved frame size as (width . height) in characters.")
(setq desktop-restore-frames nil)
(add-to-list 'desktop-globals-to-save 'desktop-aids-frame-size)

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
  (setq desktop-aids-frame-size (cons (frame-width) (frame-height)))
  (apply orig-fun args)
  (setq desktop-aids-save-buffers nil))

(defun desktop-aids-restore-frame-size ()
  "Restore frame size after desktop is loaded."
  (when desktop-aids-frame-size
    (let ((w (car desktop-aids-frame-size))
          (h (cdr desktop-aids-frame-size)))
      (run-with-idle-timer 0.5 nil
        `(lambda ()
           (set-frame-size (selected-frame) ,w ,h))))))

(defun desktop-aids-save-buffer-p (orig-fun &rest args)
  (and (gethash (car args) desktop-aids-save-buffers)
       (apply orig-fun args)))
(defun desktop-aids-update-frame-size (&optional _frame)
  "Update saved frame size whenever the frame is resized."
  (setq desktop-aids-frame-size (cons (frame-width) (frame-height))))
(add-hook 'window-size-change-functions #'desktop-aids-update-frame-size)

(advice-add 'desktop-save-buffer-p :around 'desktop-aids-save-buffer-p)
(advice-add 'desktop-save :around #'desktop-aids-setup-buffers)
(add-hook 'desktop-after-read-hook #'desktop-aids-restore-frame-size)

(provide 'desktop-aids)
