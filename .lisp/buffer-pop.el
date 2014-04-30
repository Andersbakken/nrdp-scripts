;;; Configuration;
;;
;; A configuration sample for your .emacs is as follows.
;;
;; (require 'buffer-pop)
;;
;; Besides, you can set the window height, the number for the percentage
;; for selected window.
;;
;; (buffer-pop-set-window-height 60)
;;

;;; Code:
(defvar buffer-pop-last-buffer nil)
(defvar buffer-pop-last-window nil)
(defvar buffer-pop-window nil)
(defvar buffer-pop-window-height 30) ; percentage for buffer window height
(defvar buffer-pop-window-position "bottom")
(defun buffer-pop-set-window-height (number)
  (interactive "nInput the number for the percentage of selected window height (10-100): ")
  (setq buffer-pop-window-height number))
(defun buffer-pop-set-window-position (position)
  (interactive "sInput the position for buffer-pop (top|bottom): ")
  (setq buffer-pop-window-position position))
(defun buffer-pop (buffer-or-name &optional height)
  (interactive "sInput name of buffer: ")
  (buffer-pop-out)
  (buffer-pop-up buffer-or-name height))
(defun buffer-pop-up (buffer-or-name &optional height)
  (if (not height) (setq height buffer-pop-window-height))
  (let ((w (get-buffer-window buffer-or-name)))
    (if (and nil w)
        (select-window w)
      (progn
          ; save buffer-pop-last-buffer and buffer-pop-last-window to return
          (setq buffer-pop-last-buffer (buffer-name))
          (setq buffer-pop-last-window (selected-window))
          (if (not (eq height 100))
              (let ((buffer-height
                     (if (string= buffer-pop-window-position "bottom")
                         (round (* (window-height)
                                   (/ (- 100 height) 100.0)))
                       (round (* (window-height) (/ height 100.0))))))
                (if (< buffer-height 8) (setq buffer-height 20))
                (setq buffer-pop-window (split-window (selected-window) buffer-height))
                (if (string= buffer-pop-window-position "bottom")
                    (other-window 1))))
          (if (or (bufferp buffer-or-name) (get-buffer buffer-or-name)) (switch-to-buffer buffer-or-name) nil)))))
(defun buffer-pop-out ()
  (interactive)
  (if (not (eq buffer-pop-window-height 100))
      (progn
        (if (and (> (length (window-list)) 1) buffer-pop-window (window-live-p buffer-pop-window)) 
            (delete-window buffer-pop-window))
        (setq buffer-pop-window nil)
        ;; (if (window-live-p buffer-pop-last-window)
        ;;     (progn
        ;;       (if (string= buffer-pop-window-position "bottom")
        ;;           (select-window buffer-pop-last-window))
        ;;       (switch-to-buffer buffer-pop-last-buffer)))
        )))
(provide 'buffer-pop)
;;; buffer-pop.el ends here.