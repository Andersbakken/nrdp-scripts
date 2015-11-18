;;===================
;; package stuff
;;===================

(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(defun misc-package-install (&optional query)
  (interactive "P")
  (let ((package (unless query (let ((symbol (symbol-at-point)))
                                 (and symbol
                                      (symbolp symbol)
                                      (intern (symbol-name symbol)))))))
    (if package
        (package-install package)
      (call-interactively 'package-install))))

(defun misc-current-packages ()
  (let ((ret)
        (pkgs package-activated-list))
    (while pkgs
      (add-to-list 'ret (car pkgs))
      (setq pkgs (cdr pkgs)))
    (sort ret (lambda (a b) (compare-strings (symbol-name a) nil nil
                                             (symbol-name b) nil nil)))))

(defun misc-create-desired-packages ()
  (interactive)
  (insert "(defvar misc-desired-packages (list ")
  (let ((pkg (misc-current-packages))
        (first t))
    (while pkg
      (if first
          (setq first nil)
        (insert "\n                                   "))
      (insert "'" (symbol-name (car pkg)))
      (setq pkg (cdr pkg)))
    (insert "))\n")))

(defun misc-init-packages (packages)
  (let ((cur (misc-current-packages)))
    (while packages
      (unless (member (car packages) cur)
        (message "Installing missing package %s" (symbol-name (car packages)))
        (package-install (intern (symbol-name (car packages)))))
      (setq packages (cdr packages)))))

(provide 'misc-packages)
