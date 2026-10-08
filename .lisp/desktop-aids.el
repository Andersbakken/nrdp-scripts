;;; desktop-aids.el --- Desktop buffer management  -*- lexical-binding: t; -*-
;; ;; To enable:
;; (require 'desktop-aids)

(require 'desktop)

(defvar desktop-aids-max-buffers nil
  "Maximum number of most-recently-used buffers to save, or nil for no limit.")
(defvar desktop-aids--saved-buffer-count 0)
(defvar desktop-aids--original-save-buffer-filter nil)

(defun desktop-aids-save-buffer-p (filename buffer-name mode rest)
  (when (or (null desktop-aids-max-buffers)
            (and (integerp desktop-aids-max-buffers)
                 (< desktop-aids--saved-buffer-count desktop-aids-max-buffers)))
    (when (or (null desktop-aids--original-save-buffer-filter)
              (funcall desktop-aids--original-save-buffer-filter
                       filename buffer-name mode rest))
      (setq desktop-aids--saved-buffer-count
            (1+ desktop-aids--saved-buffer-count))
      t)))

(defun desktop-aids-prepare-save ()
  (unless (eq desktop-buffers-not-to-save-function #'desktop-aids-save-buffer-p)
    (setq desktop-aids--original-save-buffer-filter
          desktop-buffers-not-to-save-function
          desktop-buffers-not-to-save-function
          #'desktop-aids-save-buffer-p))
  (setq desktop-aids--saved-buffer-count 0))
(add-hook 'desktop-save-hook #'desktop-aids-prepare-save)

;; Frame size save/restore (in characters to avoid WM decoration shrinkage)
(defvar desktop-aids-frame-size nil "Saved frame size as (width . height) in characters.")
(setq desktop-restore-frames nil)
(add-to-list 'desktop-globals-to-save 'desktop-aids-frame-size)

(defun desktop-aids-setup-buffers (orig-fun &rest args)
  (setq desktop-aids-frame-size (cons (frame-width) (frame-height)))
  (apply orig-fun args))

(defun desktop-aids-restore-frame-size ()
  "Restore frame size after desktop is loaded."
  (when desktop-aids-frame-size
    (let ((w (car desktop-aids-frame-size))
          (h (cdr desktop-aids-frame-size)))
      (run-with-idle-timer 0.5 nil
        `(lambda ()
           (set-frame-size (selected-frame) ,w ,h))))))

(defun desktop-aids-update-frame-size (&optional _frame)
  "Update saved frame size whenever the frame is resized."
  (setq desktop-aids-frame-size (cons (frame-width) (frame-height))))
(add-hook 'window-size-change-functions #'desktop-aids-update-frame-size)

(advice-add 'desktop-save :around #'desktop-aids-setup-buffers)
(add-hook 'desktop-after-read-hook #'desktop-aids-restore-frame-size)

(provide 'desktop-aids)
