;;===================
;; package stuff
;;===================

(require 'package)

(add-to-list 'package-archives '("gnusecure" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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
  (unless package-activated-list
    (package-refresh-contents))
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

(defvar misc-packages nil)
(defun misc-init-packages (packages)
  (let (updated)
    (while packages
      (push (car packages) misc-packages)
      (when (not (package-installed-p (car packages)))
        (when (not updated)
          (setq updated t)
          (package-refresh-contents))
        (message "Installing missing package %s" (symbol-name (car packages)))
        (package-install (intern (symbol-name (car packages)))))
      (setq packages (cdr packages)))))

(defun misc-list-unaccounted-packages (&optional packages)
  (interactive)
  (unless packages
    (setq packages misc-packages))
  (package-show-package-list
   (cl-remove-if-not (lambda (x) (and (not (memq x packages))
                                      (not (package-built-in-p x))
                                      (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

(provide 'misc-packages)
