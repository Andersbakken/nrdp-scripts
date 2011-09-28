
(defun lsdev-dirs-internal (&rest match)
  (let ((result)
        (args '("-ta")))
    (if match (dolist (m match) (add-to-list 'args m)))
    (with-temp-buffer
      (apply #'call-process (executable-find "lsdev.pl") nil t nil args)
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^\\([^ ]*\\) \\[\\(.*\\)\\]$")
            (add-to-list 'result (list (match-string 1) (match-string 2))))
        (forward-line)))
    result))

(defun lsdev-name (buffer-or-dir &rest match)
  (let ((dir))
    (if (not (bufferp buffer-or-dir)) (setq dir buffer-or-dir) (if (buffer-file-name buffer-or-dir) (setq dir (file-name-directory (buffer-file-name buffer-or-dir)))))
    (save-excursion
      (if dir (progn (cd dir)
                     (nth 0 (car (apply #'lsdev-dirs-internal "-p" match)))) nil))))

(defun lsdev-root-dir (buffer-or-dir &rest match)
  (let ((dir))
    (if (not (bufferp buffer-or-dir)) (setq dir buffer-or-dir) (if (buffer-file-name buffer-or-dir) (setq dir (file-name-directory (buffer-file-name buffer-or-dir)))))
    (save-excursion
      (if dir (progn (cd dir)
                     (nth 1 (car (apply #'lsdev-dirs-internal "-r" "-p" match)))) nil))))

(defun lsdev-dirs-all (&rest match)
  (apply #'lsdev-dirs-internal "-a" "-l" match))

(defun lsdev-dirs-build (dir &rest match)
  (save-excursion
    (if dir
        (cd dir))
    (apply #'lsdev-dirs-internal "-l" match)))

(provide 'lsdev)