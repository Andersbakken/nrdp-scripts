(require 'bs)
(require 'ido)
(require 'nrdp-git)
(require 'buffer-pop)
(eval-when-compile (require 'cl))

(defgroup lsdev nil
  "Group for lsdev."
  :group 'tools
  :prefix "lsdev-")

(defcustom lsdev-open-equivalent-use-find nil
  "Whether find is used in lsdev-open-equivalent"
  :type 'boolean
  :group 'lsdev)

;;general lsdev utilities
(defun lsdev-dirs-internal (&rest match)
  (let ((result)
        (default-directory (if (file-directory-p default-directory) default-directory "/"))
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

(defvar lsdev/_name nil)

(defun lsdev-name (buffer-or-dir &rest match)
  (let ((name nil))
    (if (bufferp buffer-or-dir)
        (setq name (with-current-buffer buffer-or-dir lsdev/_name)))
    (unless name
      ;; (message "Calculating lsdev for %s...%s" (if (bufferp buffer-or-dir) (buffer-file-name buffer-or-dir) buffer-or-dir))
      (let ((dir (lsdev-get-dir buffer-or-dir)))
        (if dir
            (setq name (nth 0 (car (apply #'lsdev-dirs-internal "-c" dir "-p" match)))))
        (if (bufferp buffer-or-dir)
            (with-current-buffer buffer-or-dir
              (set (make-local-variable 'lsdev/_name)
                   (if name name t))))))
    (and (stringp name) name)))

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
      (setq retval (apply #'lsdev-dirs-internal "build" "-l" "-b" match))
      (unless (= (length retval) 1)
        (setq retval (apply #'lsdev-dirs-internal "build" "-l" match)))
      (if olddir
          (cd olddir))
      retval)))

(defun lsdev-is-build-dir (&optional dir)
  (let* ((root (lsdev-root-dir (or dir default-directory)))
         (builddirs (and root (lsdev-dirs-build root))))
    (and (= (length builddirs) 1)
         (string= (cadar builddirs) root))))

(defun lsdev-find-build-file (&optional file srcdir create)
  (unless file
    (setq file (read-from-minibuffer "File: ")))
  (unless srcdir
    (setq srcdir default-directory))
  (let ((root (lsdev-root-dir srcdir)))
    (when root
      (if (file-exists-p (concat root "/" file))
          (find-file (concat root "/" file))
        (let* ((dirs (lsdev-dirs-build srcdir "-r"))
               (dir (if (= (length dirs) 1)
                        (car dirs)
                      (assoc (completing-read "Build: " dirs) dirs)))
               (path (cadr dir)))
          (and (or create (file-exists-p (concat path "/" file)))
               (concat path "/" file)))))))

(defun lsdev-open-build-file (&optional file srcdir create)
  (interactive)
  (let ((path (lsdev-find-build-file file srcdir create)))
    (when path
      (find-file path))))

(defun lsdev-open-config (&optional srcdir)
  (interactive)
  (lsdev-open-build-file ".lsdev_config" srcdir t))

(defun config.status (&optional srcdir)
  (interactive)
  (lsdev-open-build-file "config.status" srcdir))

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
      (call-process (executable-find "lsdev.pl") nil (list t nil) nil "-a" "-l" "-tn" (if string string ""))
      (goto-char (point-min))
      (let ((pattern (if (equal "" string) "\\(.*\\)" (concat ".*\\(" string ".*\\)"))))
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

(defun lsdev-cd-directory-name-at-point(&optional point)
  (save-excursion
    (when point (goto-char point))
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

(defun lsdev-git-diff-all-at-point ()
  (interactive)
  (if (call-interactively 'git-diff-repo)
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

(defvar lsdev-mode-custom-bindings nil)
(defvar lsdev-cd-history nil)
(defun lsdev-cd-mode (args from-eshell listfunc)
  (unless listfunc (setq listfunc 'find-file))
  (let ((previous (current-buffer)))
    (push "-l" args)
    (if (get-buffer "*lsdev-complete*")
        (kill-buffer "*lsdev-complete*"))
    (switch-to-buffer (get-buffer-create "*lsdev-complete*"))
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (erase-buffer)
    (apply #'call-process (executable-find "lsdev.pl") nil (list t nil) nil args)
    (goto-char (point-min))
    (if (= (point-min) (point-max))
        (progn
          (lsdev-cd-bury-buffer)
          (switch-to-buffer previous))
      (save-excursion
        (let ((lines (count-lines (point-min) (point-max))))
          (goto-char (point-min))
          (if (= lines 1)
              (progn
                (funcall listfunc (lsdev-cd-directory-name-at-point))
                (kill-buffer "*lsdev-complete*"))
            (let ((firstbuild t))
              (while (and (< (count-lines (point-min) (point)) lines)
                          (not (looking-at "\*Builds\*")))
                (if (looking-at "^build_")
                    (save-excursion
                      (kill-line)
                      (kill-line)
                      (goto-char (point-max))
                      (when firstbuild
                        (setq firstbuild nil)
                        (insert "\n*Builds*\n"))
                      (insert (current-kill 1) "\n"))
                  (forward-line)))
              (setq buffer-read-only t)
              (local-set-key (kbd "q") 'lsdev-cd-bury-buffer)
              (local-set-key (kbd "/") 'lsdev-cd-subdir)
              (local-set-key (kbd "g") 'lsdev-cd-changedir)
              (local-set-key (kbd "d") 'lsdev-git-diff-all-at-point)
              (local-set-key (kbd "=") 'lsdev-git-diff-all-at-point)
              (local-set-key (kbd "D") 'git-diff-repo)
              (local-set-key (kbd "c") 'lsdev-compile-at-point)
              (local-set-key (kbd "b") 'lsdev-compile-at-point)
              (local-set-key (kbd "r") 'lsdev-recompile-at-point)
              (local-set-key (kbd "RET") 'lsdev-cd-path-at-point)
              (local-set-key [return] 'lsdev-cd-path-at-point)
              (if (functionp lsdev-mode-custom-bindings)
                  (funcall lsdev-mode-custom-bindings))
              (add-to-list 'mode-line-buffer-identification '(:eval (lsdev-cd-modeline-function))))))))))

(defun lsdev-cd (&optional prefix listfunc)
  (interactive "P")
  (let ((args (list "-a" "-r"))
         (from-eshell (and (eq major-mode 'eshell-mode) (current-buffer)))
         (project)
         (split "\f\t\n\r\v"))
    (if prefix
        (setq split (concat split "_-"))
      (push "-me" args))
    (setq project (completing-read "LSDEV Directory: " (with-temp-buffer
                                                         (call-process (executable-find "lsdev.pl") nil (list t nil) nil "-a" "-l" "-tn")
                                                         (cl-remove-duplicates (split-string (buffer-string) (concat "[" split "]+")) :test 'equal))
                                   nil t nil 'lsdev-cd-history))
    (unless (or (string= project "")
                (not project))
      (push (if (string-match "/$" project)
                (substring project 0 -1) project)
            args))
    (setq lsdev-cd-history (cl-remove-duplicates lsdev-cd-history :from-end t :test 'equal))
    (lsdev-cd-mode args from-eshell listfunc)))

;;mode string
(defvar lsdev-mode t)
(defvar lsdev-modestring nil)
(defvar lsdev-overlay-timeout 2)
(defvar lsdev-overlay nil)
(defface lsdev-overlay-default-face
  '((((class color) (background dark)) (:background "blue"))
    (((class color) (background light)) (:background "blue"))
    (t (:bold t)))
  "Face used lsdev overlays."
  :group 'lsdev)

(defvar lsdev-last-overlay-text nil)
(defun lsdev-format-message-default (text &optional other)
  (propertize text 'face 'lsdev-overlay-default-face))

(defvar lsdev-format-message 'lsdev-format-message-default)
(defun lsdev-update-modestring (&optional buffer)
  (unless lsdev-modestring
    (let ((modeline (and (buffer-file-name buffer)
                         (lsdev-name (or buffer (current-buffer))))))
      (when modeline
        (setq-local lsdev-modestring (concat " [" modeline "] ")))))
  (when (and lsdev-modestring
             (not (string= lsdev-modestring lsdev-last-overlay-text)))
    (setq lsdev-last-overlay-text lsdev-modestring)
    (let ((pos)
          (text (funcall lsdev-format-message (substring lsdev-modestring 2 -2)))
          (overlay))
      (save-excursion
        (goto-char (window-start))
        (setq pos (line-end-position)))
      (when lsdev-overlay
        (delete-overlay lsdev-overlay))
      (setq lsdev-overlay (make-overlay pos pos))
      (overlay-put lsdev-overlay 'after-string (concat (propertize " " 'display
                                                                   `(space :align-to (- right-fringe
                                                                                        ,(1+ (length text)))))
                                                       text))
      (run-with-idle-timer lsdev-overlay-timeout nil (lambda () (when lsdev-overlay
                                                                  (delete-overlay lsdev-overlay)
                                                                  (setq lsdev-overlay nil)))))))

(add-to-list 'global-mode-string '(:eval lsdev-modestring))

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

(defun lsdev-compile-directory(&optional directory auto)
  (interactive)
  ;; (message "%s %s" directory (cond ((integerp auto) (int-to-string auto))
  ;;                                  (auto "t")
  ;;                                  (t "nil")))
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
    (lsdev-compile-directory)))

(defun lsdev-compile-pop(&optional name)
  (interactive)
  (unless name (setq name "*compilation*"))
  (if (buffer-pop name)
      (if (and (string= (buffer-name) name)
               (not (get-buffer-process (current-buffer)))
               (y-or-n-p "Recompile? "))
          (recompile))
    (lsdev-compile-directory)))

(defun lsdev-shadows ()
  (lsdev-dirs-build (lsdev-root-dir (expand-file-name default-directory))))

(defun lsdev-compile-shadow(&optional auto)
  (interactive "P")
  (let* ((build-dir (expand-file-name default-directory))
         (src-root (lsdev-root-dir build-dir))
         (shadows (lsdev-dirs-build src-root))
         (shadow-directory))
    (when shadows
      (setq shadow-directory nil)
      (if (= (length shadows) 1)
          (setq shadow-directory (nth 1 (car shadows)))
        (let ((shadow (completing-read "Shadow: " shadows))
              (s shadows))
          (when (string= shadow "")
            (setq shadow (caar shadows)))
          (while (and s (not shadow-directory))
            (let ((n (car s)))
              (setq s (cdr s))
              (if (string-equal (nth 0 n) shadow)
                  (setq shadow-directory (nth 1 n))))))))
    ;;        (message (format "%s %s %s" build-dir src-root shadow-directory))
    (let ((parent-makefile (or (find-ancestor-file "Makefile" shadow-directory)
                               (find-ancestor-file "build.ninja" shadow-directory))))
      (if parent-makefile
          (setq shadow-directory (file-name-directory parent-makefile)))
      (lsdev-compile-directory shadow-directory auto)
      t)))

(defun lsdev-find-git-file (src filename) ; src always ends with /
  (with-temp-buffer
    (shell-command (format "git --git-dir=%s/.git ls-tree -r --name-only --full-name HEAD 2>/dev/null | grep \"\\<%s$\" | sed -e 's,^,%s,'" src filename src) t)
    (and (> (point-max) (point-min))
         (buffer-substring-no-properties (point-min) (1- (point-max))))))

(defun lsdev-file-path-in-project (src path) ;; src must be the actual root for stuff to work
  (unless (string-match "/$" src)
    (setq src (concat src "/")))
  (cond ((file-exists-p (concat src path))
         (concat src path))
        ((lsdev-find-git-file src (file-name-nondirectory path)))
        (lsdev-open-equivalent-use-find
         (call-process "find" nil (cons t nil) nil src "-type" "f" "-name" (file-name-nondirectory path))
         (and (> (point-max) (point-min))
              (buffer-substring-no-properties (point-min) (1- (point-max)))))
        (t nil)))

(defun lsdev-equivalent-file ()
  (when (buffer-file-name)
    (let* ((default-directory "/")
           (current-root (lsdev-root-dir (buffer-file-name)))
           (relative (substring (buffer-file-name) (length current-root)))
           (all (lsdev-dirs-all "src_"))
           (names)
           (alternatives))
      (while all
        (let* ((cur (car all))
               (name (substring (car cur) 4))
               (path (car (cdr cur)))
               (file (and (not (string= path current-root))
                          (lsdev-file-path-in-project path relative))))
          (when file
            (push name names)
            (push (cons name file) alternatives)))
        (setq all (cdr all)))
      (if alternatives
          (let* ((default (car names))
                 (project (completing-read
                           (format "Open %s (default %s): " (file-name-nondirectory (buffer-file-name)) default)
                           names nil nil (and (= (length names) 1) (car names)) nil default)))
            (when project
              (let* ((files (split-string (cdr (assoc project alternatives)) "\n"))
                     (file (car files)))
                (if (> (length files) 1)
                    (let* ((src-root (lsdev-dir-for-name (concat "src_" project)))
                           (src-root-len (1+ (length src-root))))
                      (setq file (completing-read "File: "
                                                  (mapcar (lambda (arg) (substring arg src-root-len)) files)))
                      (when file
                        (setq file (concat src-root "/" file)))))
                file)))))))

(defun lsdev-open-equivalent (&optional ediff)
  (interactive "P")
  (when (buffer-file-name)
    (let ((ctx (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          (file (lsdev-equivalent-file)))
      (cond ((not file) (message "No file found"))
            (ediff
             (let ((buf (find-file-noselect file)))
               (unless buf
                 (error "Can't open buffer: %s" file))
               (if (listp ediff)
                   (ediff-buffers buf (current-buffer))
                 (ediff-buffers (current-buffer) buf))))
            (t
             (find-file file)
             (let ((old (point)))
               (goto-char (point-min))
               (unless (search-forward ctx nil t)
                 (goto-char old))))))))

(defun lsdev-adddev (&optional name path)
  (interactive)
  (unless name
    (setq name (read-from-minibuffer "Name: ")))
  (unless path
    (setq path (read-directory-name "Path: ")))

  (if (and name path)
      (let ((dev-directories (expand-file-name "~/.dev_directories"))
            (home (expand-file-name "~/")))
        (save-excursion
          (if (string= home (substring path 0 (length home)))
              (setq path (concat "~/" (substring path (length home)))))
          (set-buffer (find-file-noselect dev-directories))
          (goto-char (point-max))
          (unless (= (point-at-bol) (point))
            (insert "\n"))
          (insert name "=" path "\n")
          (basic-save-buffer)
          (kill-buffer (current-buffer))))))

(defun lsdev-limited-buffers-file ()
  (locate-user-emacs-file "lsdev-limited-buffers"))

(defun lsdev-restore-limited-buffers ()
  (interactive)
  (let ((file (lsdev-limited-buffers-file)))
    (when (file-exists-p file)
      (let ((buffer (find-file-noselect file)))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (find-file-noselect (buffer-substring-no-properties (point) (point-at-eol)))
              (forward-line 1))
            (erase-buffer)
            (basic-save-buffer)))
        (kill-buffer buffer)))))

(defun lsdev-pretty-name (buffer)
  (let ((name (lsdev-name buffer)))
    (cond ((null name) nil)
          ((string-prefix-p "src_" name) (substring name 4))
          ((string-prefix-p "build_" name) (substring name 6))
          (t name))))

(defun lsdev-limit-to-project (&optional unlimit)
  (interactive "P")
  (lsdev-restore-limited-buffers)
  (when (not unlimit)
    (let ((all-buffers (buffer-list))
          (current (lsdev-pretty-name (current-buffer)))
          (old-current (current-buffer))
          (buffers (make-hash-table :test 'equal)))
      (while all-buffers
        (let ((name (and (buffer-file-name (car all-buffers)) (lsdev-pretty-name (car all-buffers)))))
          (when name
            (let ((cur (gethash name buffers)))
              (if cur
                  (add-to-list 'cur (car all-buffers))
                (setq cur (list (car all-buffers))))
              (puthash name cur buffers))))
        (setq all-buffers (cdr all-buffers)))
      (let ((project (completing-read (if current (format "Project (default %s): " current) "Project: ") buffers)))
        (unless (> (length project) 0)
          (setq project current))
        (let ((buffer (find-file-noselect (lsdev-limited-buffers-file)))
              (set-current-buffer nil))
          (maphash (lambda (key value)
                     ;; (message "%s" (cond ((null value) "null")
                     ;;                     ((stringp value) "string")
                     ;;                     ((listp value) "list")
                     ;;                     (t "other")))
                     (unless (string= key project)
                       (with-current-buffer buffer
                         (goto-char (point-max))
                         (while value
                           (insert (buffer-file-name (car value)) "\n")
                           ;; (message "shit [%s] [%s] [%s]" key project (buffer-file-name (car value)))
                           (when (eq (car value) old-current)
                             (setq set-current-buffer t))
                           (kill-buffer (car value))
                           ;; (kill-buffer-ask (car value))
                           (setq value (cdr value)))
                         (basic-save-buffer))))
                   buffers)
          (when set-current-buffer
            (switch-to-buffer (car (gethash project buffers))))
          (kill-buffer buffer))))))

;; (message "Got project [%s]" project))))

(defvar lsdev-git-sync-src nil)
(defun lsdev-git-sync-sentinel (process event)
  (when (eq (process-status process) 'exit)
    (with-current-buffer (process-buffer process)
      (if (= (process-exit-status process) 0)
          (message "Synced git repo %s" lsdev-git-sync-src)
        (message "git sync error: %s" (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer (current-buffer)))))

(defun lsdev-git-sync ()
  (interactive)
  (let ((dir (completing-read "git sync: " (lsdev-dirs-all "-build"))))
    (setq lsdev-git-sync-src dir)
    (set-process-sentinel (start-process (concat "lsdev git sync " dir) " *lsdev-git-sync*" "git" "lsdev" dir "--" "sync" "-r") 'lsdev-git-sync-sentinel)
    (message "Syncing %s..." dir)))

(provide 'lsdev)
