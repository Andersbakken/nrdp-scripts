;;===================
;; package stuff (straight.el)
;;===================

;; Disable package.el
 (setq package-enable-at-startup nil)

;; Enable straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun misc-package-install (&optional query)
  "Install a package using straight.el.
With prefix arg QUERY, prompt for package name."
  (interactive "P")
  (let ((package (unless query (let ((symbol (symbol-at-point)))
                                 (and symbol
                                      (symbolp symbol)
                                      (intern (symbol-name symbol)))))))
    (if package
        (straight-use-package package)
      (call-interactively 'straight-use-package))))

(defvar misc-packages nil
  "List of packages managed by misc-init-packages.")

(defun misc-init-packages (packages)
  "Install PACKAGES using straight.el."
  (dolist (pkg packages)
    (push pkg misc-packages)
    (straight-use-package pkg)))

(defun misc-list-unaccounted-packages (&optional packages)
  "List installed packages not in PACKAGES."
  (interactive)
  (unless packages
    (setq packages misc-packages))
  (let ((installed (hash-table-keys straight--recipe-cache)))
    (message "Unaccounted packages: %s"
             (cl-remove-if (lambda (x) (memq x packages)) installed))))

(straight-use-package 'use-package)
(straight-use-package 'el-patch)

(provide 'misc-packages)
