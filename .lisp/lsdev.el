
(defun lsdev-dirs-internal (&rest match)
  (let ((result)
        (args '("-ta")))
    (if match (dolist (m match) (add-to-list 'args m t)))
    (with-temp-buffer
      (apply #'call-process (executable-find "lsdev.pl") nil t nil args)
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^\\([^ ]*\\) \\[\\(.*\\)\\]$")
            (add-to-list 'result (list (match-string 1) (match-string 2))))
        (forward-line)))
    result))

;; (defun lsdev-test ()
;;   (interactive)
;;   (message (lsdev-dirs-internal "-tn")))

(defun lsdev-get-dir (buffer-or-dir)
  (let ((dir))
    (if (not (bufferp buffer-or-dir))
        (setq dir buffer-or-dir)
      (if (buffer-file-name buffer-or-dir)
          (setq dir (file-name-directory (buffer-file-name buffer-or-dir)))
        (with-current-buffer buffer-or-dir (setq dir default-directory))))
    dir))

(defun lsdev-name (buffer-or-dir &rest match)
  (let ((dir (lsdev-get-dir buffer-or-dir)))
    (if dir (nth 0 (car (apply #'lsdev-dirs-internal "-c" dir "-p" match))) nil)))

(defun lsdev-root-dir (buffer-or-dir &rest match)
  (let ((dir (lsdev-get-dir buffer-or-dir)))
    (if dir (nth 1 (car (apply #'lsdev-dirs-internal "-c" dir "-r" "-p" match))) nil)))

(defun lsdev-dirs-all (&rest match)
  (apply #'lsdev-dirs-internal "-a" "-l" match))

(defun lsdev-dirs-build (dir &rest match)
  (save-excursion
    (if dir
        (cd dir))
    (apply #'lsdev-dirs-internal "-l" match)))

(defun lsdev-dir-for-name(name)
  (let ((ret nil) (hds (lsdev-dirs-all)))
    (while (and hds (not ret))
      (let ((hd (car hds))
            (hds (cdr hds)))
        (if (string-equal (nth 0 hd) name)
            (setq ret (nth 1 hd)))))
    ret))

(defun lsdev-cd-completing (string predicate code)
  (let ((complete-list (make-vector 63 0)))
    (with-temp-buffer
      (call-process (executable-find "lsdev.pl") nil (list t nil) nil "-a" "-l" "-tn" (if string string ""))
      (goto-char (point-min))
      (let ((pattern (if (equal "" string) "\\(.*\\)" (concat ".*\\(" string ".*\\)"))))
        (while (not (eobp))
          (looking-at pattern)
          (intern (match-string 1) complete-list)
          (forward-line))))
    (cond ((eq code nil)
           (try-completion string complete-list predicate))
          ((eq code t)
           (all-completions string complete-list predicate))
          ((eq code 'lambda)
           (if (intern-soft string complete-list) t nil)))))

(defun lsdev-goto-file-at-point ()
  (interactive)
  (find-file (buffer-substring (point-at-bol) (point-at-eol))))

(defun lsdev-cd()
  (interactive)
  (let ((hd (completing-read "Directory: " 'lsdev-cd-completing))
        (prev (current-buffer)))
    (if (get-buffer "*lsdev-complete*")
        (kill-buffer "*lsdev-complete*"))
    (switch-to-buffer (generate-new-buffer "*lsdev-complete*"))
    (call-process (executable-find "lsdev.pl") nil (list t nil) nil "-a" "-l" "-tp" hd)
    (cond ((= (point-min) (point-max)) (switch-to-buffer previous))
          ((= (count-lines (point-min) (point-max)) 1) (find-file (buffer-substring (point-min) (- (point-max) 1))))
          (t (progn
               (setq buffer-read-only t)
               (goto-char (point-min))
               (local-set-key "q" 'bury-buffer)
               (local-set-key (kbd "RET") 'lsdev-goto-file-at-point)
               (local-set-key [return] 'lsdev-goto-file-at-point)
               )))))


(provide 'lsdev)