
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

(defun lsdev-name (dir &rest match)
  (save-excursion
    (if dir
        (cd dir))
    (nth 0 (car (apply #'lsdev-dirs-internal "-p" match)))))

(defun lsdev-root-dir (dir &rest match)
  (save-excursion
    (if dir
        (cd dir))
    (nth 1 (car (apply #'lsdev-dirs-internal "-r" "-p" match)))))

(defun lsdev-dirs-all (&rest match)
  (apply #'lsdev-dirs-internal "-a" "-l" match))

(defun lsdev-dirs-build (dir &rest match)
  (save-excursion
    (if dir
        (cd dir))
    (apply #'lsdev-dirs-internal "-l" match)))

(provide 'lsdev)