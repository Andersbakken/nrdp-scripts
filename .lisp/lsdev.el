;; (defun lsdev-parse-key-value(file)
;;   (setq ret nil)
;;   (if file
;;       (with-temp-buffer
;;         (insert-file-contents file)
;;         (goto-char (point-min))
;;         (while (re-search-forward "^\\(.*\\)=\\(.*\\)$" nil t)
;;           (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
;;                 (value (buffer-substring (match-beginning 2) (match-end 2))))
;;             (push (cons key value) ret)))))
;;     ret)

;; (defun lsdev-src-dirs()
;;   (lsdev-parse-key-value "~/.dev_directories")
;;   )

(defun lsdev-dirs-internal (&rest match)
  (interactive)
  (let ((result)
        (args '("-ta")))
    (if match
        (apply #'add-to-list 'args match))
    ;; (set-buffer (create-file-buffer "/tmp/foo"))
    ;; (message default-directory)
    (message args)
    (with-temp-buffer
      (apply #'call-process (executable-find "lsdev.pl") nil t nil args)
      (message (buffer-string))
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^\\([^ ]*\\) \\[\\(.*\\)\\]$")
            (add-to-list 'result (list (match-string 1) (match-string 2))))
        (forward-line)))
    result))

(defun lsdev-dirs-current-name (dir &rest match)
  (save-excursion
    (if dir
        (cd dir))
    (car (car (apply #'lsdev-dirs-internal "-p" match)))))

(defun lsdev-dirs-current-root-dir (dir &rest match)
  (save-excursion
    (if dir
        (cd dir))
    (cdr (car (apply #'lsdev-dirs-internal "-r" "-p" match)))))

(defun lsdev-dirs-all (&rest match)
  (apply #'lsdev-dirs-internal "-a" "-l" match))

(defun lsdev-dirs-build (dir &rest match)
  (save-excursion
    (if dir
        (cd dir))
    (apply #'lsdev-dirs-internal "-l" match)))

(provide 'lsdev)