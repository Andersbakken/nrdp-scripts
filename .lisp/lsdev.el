(defgroup lsdev nil
  "Group for lsdev."
  :group 'tools
  :prefix "lsdev-")

(defcustom lsdev-cd-ignore-builds nil
  "Whether build directories are shown in lsdev-cd"
  :type 'boolean
  :group 'lsdev)

;;general lsdev utilities
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

;;lsdev-cd handling
(defun lsdev-cd-completing (string predicate code)
  (let ((complete-list (make-vector 63 0)))
    (with-temp-buffer
      (call-process (executable-find "lsdev.pl") nil (list t nil) nil "-a" "-l" "-tn" (if string string "") (if lsdev-cd-ignore-builds "-build" ""))
      (goto-char (point-min))
      (let ((pattern (if (equal "" string) "\\(.*\\)" (concat ".*\\(" string ".*\\)"))))
        (if lsdev-cd-ignore-builds
            (setq pattern (concat "src_" pattern)))
        ;; (message (concat "pattern " pattern))
        (while (not (eobp))
          (if (looking-at pattern)
              (intern (match-string 1) complete-list))
          (forward-line))))
    (cond ((eq code nil)
           (try-completion string complete-list predicate))
          ((eq code t)
           (all-completions string complete-list predicate))
          ((eq code 'lambda)
           (if (intern-soft string complete-list) t nil)))))

(defun lsdev-cd-directory-name-at-point()
  (save-excursion
    (beginning-of-line)
    ;; (message (buffer-substring (point-at-bol) (point-at-eol)))
    (if (looking-at "^.* \\[\\(.*\\)\\]$")
        (let ((dir (match-string 1)))
          (if (string-match "/$" dir) dir (concat dir "/"))
          )
      nil)))

(defun lsdev-cd-bury-buffer()
  (interactive)
  (if (string-equal (buffer-name) "*lsdev-complete*")
      (bury-buffer)))

(defun lsdev-cd-open (dirname)
  (if (and dirname (file-exists-p dirname))
      (progn
        (find-file dirname))
    (message (if dirname dirname "empty"))))

(defun lsdev-cd-path-at-point ()
  (interactive)
  (lsdev-cd-open (lsdev-cd-directory-name-at-point)))

(defun lsdev-cd-subdir ()
  (interactive)
  (let ((dirname (lsdev-cd-directory-name-at-point)))
    (lsdev-cd-open (ido-find-file-in-dir dirname))))

(defun lsdev-cd-changedir (&optional quiet)
  (interactive)
  (let ((dir (lsdev-cd-directory-name-at-point)))
    (if (and dir) (progn
                    (cd dir)
                    (if (not quiet) (message (concat "LSDEV-CD " dir)))
                    ))))

(defun lsdev-cd-modeline-function () (lsdev-cd-changedir t) nil)

(defvar lsdev-cd-history nil)
(defun lsdev-cd(&optional ignore-builds)
  (interactive)
  (let ((hd (completing-read "LSDEV Directory: " 'lsdev-cd-completing nil nil nil 'lsdev-cd-history))
        (previous (current-buffer))
        (slash nil))
    (if (string-match "/$" hd)
        (progn
          (setq slash t)
          (setq hd (substring hd 0 -1))))
    (switch-to-buffer (get-buffer-create "*lsdev-complete*"))
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (delete-char (- (point-max) 1))
    (call-process (executable-find "lsdev.pl") nil (list t nil) nil "-a" "-l" hd (if (or ignore-builds lsdev-cd-ignore-builds) "-build" ""))
    (goto-char (point-min))
    (cond ((= (point-min) (point-max)) (lsdev-cd-bury-buffer) (switch-to-buffer previous))
          ;;((= (count-lines (point-min) (point-max)) 1) (if slash (lsdev-cd-subdir) (lsdev-cd-path-at-point)))
          (t (progn
               (save-excursion
                 (let ((first t) (lines (count-lines (point-min) (point-max))))
                   (goto-char (point-min))
                   (while (and (<(count-lines (point-min) (point)) lines) (not (looking-at "\*Builds\*")))
                     (if (looking-at "^build_")
                         (save-excursion
                           (kill-line)(kill-line)
                           (goto-char (point-max))
                           (if first (progn (setq first nil) (insert "\n*Builds*\n")))
                           (insert (current-kill 1))
                           (insert "\n"))
                         (next-line)))))
               (setq buffer-read-only t)
               (local-set-key "q" 'lsdev-cd-bury-buffer)
               (local-set-key "/" 'lsdev-cd-subdir)
               (local-set-key "g" 'lsdev-cd-changedir)
               (local-set-key (kbd "RET") 'lsdev-cd-path-at-point)
               (local-set-key [return] 'lsdev-cd-path-at-point)
               (add-to-list 'mode-line-buffer-identification '(:eval (lsdev-cd-modeline-function)))
               )))))


(provide 'lsdev)