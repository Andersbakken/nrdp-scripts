(require 'bytecomp)

(setq sam-load-module-path (expand-file-name "~/.lisp"))

(defun sam-load-module-add-path(module) (add-to-list 'load-path (expand-file-name (concat sam-load-module-path "/" module))))
(defun sam-load-module(module &optional filename)
  (unless filename (setq filename module))
  (let (filepath)
    (cond
     ((file-exists-p (concat sam-load-module-path "/" module "/" filename ".el")) (setq filepath (expand-file-name (concat sam-load-module-path "/" module "/" filename ".el"))))
     ((file-exists-p (concat sam-load-module-path "/" filename ".el")) (setq filepath (expand-file-name (concat sam-load-module-path "/" filename ".el"))))
     ((file-exists-p (concat sam-load-module-path "/" module "/" filename)) (setq filepath (expand-file-name (concat sam-load-module-path "/" module "/" filename))))
     ((file-exists-p (concat sam-load-module-path "/" filename)) (setq filepath (expand-file-name (concat sam-load-module-path "/" filename))))
     (t (message "No module: %s" module)))
    (if filepath
        (let ((compiled-filepath (byte-compile-dest-file filepath)))
      (add-to-list 'load-path (file-name-directory filepath))
          (if (or (not (file-exists-p compiled-filepath)) (file-newer-than-file-p filepath compiled-filepath))
              (byte-compile-file filepath t)
            (load compiled-filepath))))))
