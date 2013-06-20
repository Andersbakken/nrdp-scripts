(require 'bs)
(require 'cl)
(require 'ido)
(require 'git)

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
        (args '("-ta" "-l")))
    (if match (dolist (m match) (add-to-list 'args m t)))
    (with-temp-buffer
      (apply #'call-process (executable-find "lsdev.pl") nil t nil args)
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^\\([^ ]*\\) \\[\\([^]]*\\)\\]")
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

(defvar _lsdev_name nil)

(defun lsdev-name (buffer-or-dir &rest match)
  (let ((name nil))
    (if (bufferp buffer-or-dir) (setq name (with-current-buffer buffer-or-dir _lsdev_name)))
    (unless name
      ;; (message "Calculating lsdev for %s...%s" (if (bufferp buffer-or-dir) (buffer-file-name buffer-or-dir) buffer-or-dir))
      (let ((dir (lsdev-get-dir buffer-or-dir)))
        (if dir (setq name (nth 0 (car (apply #'lsdev-dirs-internal "-c" dir "-p" match)))))
        (if (bufferp buffer-or-dir) (with-current-buffer buffer-or-dir
                                      (set (make-local-variable '_lsdev_name) (if name name t))))))
       (if (stringp name) name nil)))

(defun lsdev-root-dir (buffer-or-dir &rest match)
  (let ((dir (lsdev-get-dir buffer-or-dir)))
    (if dir (nth 1 (car (apply #'lsdev-dirs-internal "-c" dir "-r" "-p" match))) nil)))

(defun lsdev-dirs-all (&rest match)
  (apply #'lsdev-dirs-internal "-a" "-l" match))

(defun lsdev-dirs-build (dir &rest match)
  (save-excursion
    (let ((olddir default-directory)
          (retval))
      (if (and dir (file-directory-p dir))
          (cd dir)
        (setq olddir nil))
      (setq retval (apply #'lsdev-dirs-internal "-l" "-b" match))
      (unless (= (length retval) 1)
        (setq retval (apply #'lsdev-dirs-internal "-l" match)))
      (if olddir
          (cd olddir))
      retval)))

(defun lsdev-dir-for-name(name)
  (let ((ret nil) (hds (lsdev-dirs-all)))
    (while (and hds (not ret))
      (let ((hd (car hds)))
        (setq hds (cdr hds))
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
    (if (looking-at "^[^[]* \\[\\([^]]*\\)\\]")
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

(defun lsdev-compile-at-point (&optional recompile)
  (interactive "P")
  (let ((dir (lsdev-cd-directory-name-at-point)))
    (if dir
        (lsdev-compile-directory dir (if recompile 1 t)))))

(defun lsdev-git-status-at-point ()
  (interactive)
  (let ((root (git-root-dir)))
    (if root
        (git-status root)
      (call-interactively 'git-status))))

(defun lsdev-git-diff-all-at-point (&optional -w)
  (interactive "P")
  (if (git-diff-all -w)
      (delete-other-windows)))

(defun lsdev-recompile-at-point ()
  (interactive)
  (lsdev-compile-at-point t))

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
(defun lsdev-cd-internal(args ignore-builds)
  (let ((previous (current-buffer)))
    (push "-l" args)
    (if (or ignore-builds lsdev-cd-ignore-builds) (push "-build" args))
    (if (get-buffer "*lsdev-complete*") (kill-buffer "*lsdev-complete*"))
    (switch-to-buffer (get-buffer-create "*lsdev-complete*"))
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (erase-buffer)
    (apply #'call-process (executable-find "lsdev.pl") nil (list t nil) nil args)
    (goto-char (point-min))
    (cond ((= (point-min) (point-max)) (lsdev-cd-bury-buffer) (switch-to-buffer previous))
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
               (local-set-key (kbd "q") 'lsdev-cd-bury-buffer)
               (local-set-key (kbd "/") 'lsdev-cd-subdir)
               (local-set-key (kbd "g") 'lsdev-cd-changedir)
               (local-set-key (kbd "s") 'lsdev-git-status-at-point)
               (local-set-key (kbd "d") 'lsdev-git-diff-all-at-point)
               (local-set-key (kbd "=") 'lsdev-git-diff-all-at-point)
               (local-set-key (kbd "D") 'git-diff-all)
               (local-set-key (kbd "c") 'lsdev-compile-at-point)
               (local-set-key (kbd "b") 'lsdev-compile-at-point)
               (local-set-key (kbd "r") 'lsdev-recompile-at-point)
               (local-set-key (kbd "RET") 'lsdev-cd-path-at-point)
               (local-set-key [return] 'lsdev-cd-path-at-point)
               (add-to-list 'mode-line-buffer-identification '(:eval (lsdev-cd-modeline-function)))
               )))))

(defun lsdev-cd(&optional ignore-builds)
  (interactive)
  (let* ((args nil)
         (alternatives (with-temp-buffer
                         (call-process (executable-find "lsdev.pl") nil (list t nil) nil "-a" "-l" "-tn" (if lsdev-cd-ignore-builds "-build" ""))
                         (remove-duplicates (split-string (buffer-string) "[\f\t\n\r\v_-]+") :test 'equal)))
         (hd (ido-completing-read "LSDEV Directory: " alternatives nil t nil 'lsdev-cd-history)))
    (setq lsdev-cd-history (remove-duplicates lsdev-cd-history :from-end t :test 'equal))
    (if (or (string= hd "") (not hd)) (push "-b" args)
      (progn
        (push "-a" args)
        (push "-r" args)
        (push (if (string-match "/$" hd) (substring hd 0 -1) hd) args)))
    (lsdev-cd-internal args ignore-builds)))

;;mode string
(defvar lsdev-mode t)
(defvar lsdev-modestring nil)
(defun lsdev-update-modestring (&optional buffer)
  (unless buffer (setq buffer (current-buffer)))
  (let ((modeline (lsdev-name buffer)))
    (if modeline (setq modeline (concat " [" modeline "]")))
    (set (make-local-variable 'lsdev-modestring) modeline)))
;;(setq global-mode-string (append global-mode-string '(lsdev-modestring)))
(if (not (assoc 'lsdev-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(lsdev-mode lsdev-modestring) minor-mode-alist)))
(defadvice switch-to-buffer (after lsdev-update-modestring-adv)
  (lsdev-update-modestring))
(ad-activate 'switch-to-buffer)
(defadvice display-buffer (after lsdev-update-modestring-adv)
  (when ad-return-value (lsdev-update-modestring)))
(ad-activate 'display-buffer)


;;bs integration
(defun lsdev-bs-get-name(&rest ignored)
  (let ((name (lsdev-name (current-buffer))))
    (if name name "")))
(defun lsdev-bs-sort(b1 b2)
  (string< (lsdev-name b1) (lsdev-name b2)))
(add-to-list 'bs-configurations '("lsdev" nil nil nil nil lsdev-bs-sort) t)

;; compile stuff

(defvar lsdev-compile-args-by-dir (make-hash-table :test 'equal))
(defvar lsdev-compile-command nil)
(defvar lsdev-compile-last-directory nil)
(defvar lsdev-compile-last-args (getenv "MAKEFLAGS"))
(defvar lsdev-compile-args-history nil)

(defun lsdev-compile-directory(directory &optional auto)
  (interactive)
  (message "%s %s" directory (cond ((integerp auto) (int-to-string auto))
                                   (auto "t")
                                   (t "nil")))
  (unless (and directory auto)
    (setq directory (read-directory-name "Directory: " directory directory)))
  (if (> (length directory) 0)
      (let ((args (gethash directory lsdev-compile-args-by-dir lsdev-compile-last-args))
            (command (concat (if (> (length lsdev-compile-command) 0) lsdev-compile-command "make") " -C " directory)))
        (unless (and (integerp auto)
                     (= auto 1))
          (setq args (read-shell-command "Args: " args 'lsdev-compile-args-history)))
        (if args
            (setq command (concat command " " args)))
        (setq lsdev-compile-last-args args)
        (setq lsdev-compile-last-directory directory)
        (let ((default-directory directory))
          (compile command)
          (if args
              (puthash directory args lsdev-compile-args-by-dir)))))
  )


(defun lsdev-recompile-directory()
  (interactive)
  (if lsdev-compile-last-directory
      (lsdev-compile-directory lsdev-compile-last-directory)
    (lsdev-compile-directory nil)))

(defun lsdev-compile-pop(&optional name)
  (interactive)
  (unless name (setq name "*compilation*"))
  (if (buffer-pop name)
      (if (and (string= (buffer-name) name) (not (get-buffer-process (current-buffer))) (y-or-n-p "Recompile? ")) (recompile))
    (lsdev-compile-directory)))

(defun lsdev-shadows ()
  (lsdev-dirs-build (lsdev-root-dir (expand-file-name default-directory))))

(defun lsdev-compile-shadow(&optional auto)
  (interactive "P")
  (setq build-dir (expand-file-name default-directory))
  (setq src-root (lsdev-root-dir build-dir))
  (setq shadows (lsdev-dirs-build src-root))
  (if shadows
      (progn
        (setq shadow-directory nil)
        (if (= (length shadows) 1)
            (setq shadow-directory (nth 1 (car shadows)))
          (progn
            (setq shadow (ido-completing-read "Shadow: " shadows))
            (let ((s shadows))
              (while (and s (not shadow-directory))
                (progn
                  (setq n (car s) s (cdr s))
                  (if (string-equal (nth 0 n) shadow)
                      (setq shadow-directory (nth 1 n))))))))
        ;;        (message (format "%s %s %s" build-dir src-root shadow-directory))
        (setq parent-makefile (or (find-ancestor-file "Makefile" shadow-directory)
                                  (find-ancestor-file "build.ninja" shadow-directory)))
        (if parent-makefile (setq shadow-directory (file-name-directory parent-makefile)))
        (lsdev-compile-directory shadow-directory auto)
        t)
    nil))

(defun lsdev-file-path-in-project (src path) ;; src must be the actual root for stuff to work
  (if (file-exists-p (concat src path))
      (concat src path)
    (let ((out (shell-command-to-string (format "GTAGSROOT=%s global -Poa /%s" src (file-name-nondirectory path)))))
      (when (and (> (length out) 0)
                 (not (string-match "^global: " out)))
        (setq out (substring out 0 (- (length out) 1)))
        (if (file-exists-p out) out nil))))
  )

(defun lsdev-open-equivalent ()
  (interactive)
  (if (buffer-file-name)
      (let* ((default-directory "/")
             (current-root (lsdev-root-dir (buffer-file-name)))
             (relative (substring (buffer-file-name) (length current-root)))
             (all (lsdev-dirs-all "src"))
             (alternatives))
        (while all
          (let* ((cur (car all))
                 (name (substring (car cur) 4))
                 (path (car (cdr cur))))
            (if (and (not (string= path current-root))
                     (lsdev-file-path-in-project path relative))
                (setq alternatives (append (list name) alternatives))))
          (setq all (cdr all)))
        (if alternatives
            (let ((project (ido-completing-read (format "Open %s: " (file-name-nondirectory (buffer-file-name))) alternatives)))
              (if project
                  (find-file (lsdev-file-path-in-project (lsdev-dir-for-name (concat "src_" project)) relative))))

          )
        )
    )
  )
(provide 'lsdev)



