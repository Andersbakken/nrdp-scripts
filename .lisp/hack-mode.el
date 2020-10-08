(eval-when-compile
  (require 'cl))
(require 'ido)
(require 'nrdp-misc "misc")

(defconst hack-mode-templatize-nth 1)
(defconst hack-mode-c-mode-nth 2)
(defconst hack-mode-find-file-mode-nth 3)
(defconst hack-mode-find-log-nth 4)

(defun hack-mode-printf-format ()
  (let* ((fn (buffer-file-name))
         (name (if fn
                   (file-name-nondirectory fn)
                 (buffer-name))))

    (cons (concat "[" name ":%d]") ", __LINE__")))

(defvar hack-mode-templatize-header-format "%s_H_")
;;====================
;; Licensing fu
;;====================
(defvar hack-mode-license-rege-list
  '( ;;First one is my default license type
    ("without the permission of Netflix" "NETFLIX")
    ("Copyright.*Yahoo! Inc" "YAHOO")
    ("QT_BEGIN_LICENSE" "TROLL")
    ("Copyright.*Aventail Corporation" "AVENTAIL")
    ("Copyright.*The Apache Software Foundation" "APACHE")
    ;;  ("Copyright.*X Consortium" "X11")
    ("\\(Boston, MA[ \t]+02111-1307, USA\\|Cambridge, MA 02139, USA\\)" "GPL")
    ("SUCH DAMAGE\\.[     ]*" "BSD")
    ("\\\*\\* or implied warranty\\.[     ]*" "NCAR")
    ))
(defvar hack-mode-buffer-license-type nil)
(make-variable-buffer-local 'hack-mode-buffer-license-type)
(defun hack-mode-license-remove()
  "This will remove licences below and stick the license type in the modeline, hidecopyleft wasn't good enough"
  (interactive)
  (if (not hack-mode-buffer-license-type)
      (let ((found nil) (regexp-list hack-mode-license-rege-list) (lic nil))
        (widen)
        (goto-char (point-min))
        (push-mark)
        (while (and (not found) regexp-list)
          (if (setq found (re-search-forward (car (car regexp-list)) nil t))
              (let ((eoc (search-forward "*/")) (boc (search-backward "/*")))
                (setq lic (nth 1 (car regexp-list)))
                (narrow-to-region eoc (point-max))
                ;;(setq mode-line-buffer-identification (list "(" lic ") " mode-line-buffer-identification))
                ))
          (setq regexp-list (cdr regexp-list)))
        (setq hack-mode-buffer-license-type lic))
    (pop-mark)))
;;(mapcar '(lambda (hooksym) (add-hook hooksym 'hack-mode-license-remove)) '(c-mode-hook c++-mode-hook))

;;default stuff (my preferred hack mode)
(defun default-find-file-hook ()
  (hack-mode-no-tabs))
(setq hack-mode-default '("Default" nil nil default-find-file-hook))

;;troll stuff
(defun troll-templatize-file () "Insert a standard Troll template comment into the current buffer."
       (let ((f (expand-file-name "~/.troll.license")))
         (if (and (not (/= (point-min) (point-max))) (file-exists-p f))
             (insert-file-contents f) ;;insert the license
           )))
(defun troll-log-message (msg &optional nopercent) "Insert Troll logging message."
       (let ((result))
         (insert (concat "qWarning(\"" (car (hack-mode-printf-format)) ": " msg "\"" (cdr (hack-mode-printf-format))))
         (if (and (not nopercent) (string-match "%" msg)) (progn (insert ", ") (setq result (point))))
         (insert ");")
         result))
(defun troll-c-mode-hook ()
  (setq
   compile-command (format "make QTDIR=%s -kC %s/src" (getenv "QTDIR") (getenv "QTDIR"))
   ))
(defun hack-mode-no-tabs ()
  (setq indent-tabs-mode nil)
  (if (boundp 'insert-tab-mode)
      (setq insert-tab-mode nil)))

(defun troll-find-file-hook ()
  (hack-mode-no-tabs))

(setq hack-mode-troll '("Troll" troll-templatize-file troll-c-mode-hook troll-find-file-hook troll-log-message))

;;webkit stuff
(defun webkit-templatize-file () "Insert a standard WebKit template comment into the current buffer.")
(defun webkit-find-file-hook ()
  (hack-mode-no-tabs)
  (setq sam-auto-clean-whitespace nil))

(defun webkit-c-mode-hook ()
  (c-set-offset 'innamespace 0)
  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (if (boundp 'c-protection-key)
      (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                     "\\|protected slot\\|private\\|private slot"
                                     "\\)\\>")))
  (if (boundp 'c-c++-access-key)
      (setq c-c++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                     "\\|public slots\\|protected slots\\|private slots"
                                     "\\)\\>[ \t]*:")))
  ;; modify the colour of slots to match public, private, etc ...
  (font-lock-add-keywords 'c++-mode
                          '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
  ;; make new font for rest of qt keywords
  (make-face 'qt-keywords-face)
  (set-face-foreground 'qt-keywords-face "BlueViolet")
  ;; qt keywords
  (font-lock-add-keywords 'c++-mode
                          '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode
                          '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode
                          '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))
  )
(setq hack-mode-webkit '("WebKit" webkit-templatize-file webkit-c-mode-hook webkit-find-file-hook nil))

(defun netflix-has-NERROR ()
  (let ((root (--misc-find-deepest-ancestor-directory "configure")))
    (when root
      (with-temp-buffer
        (insert-file-contents (concat root "/src/base/Version.h"))
        (goto-char (point-min))
        (when (re-search-forward "^#define NRDP_VERSION_MAJOR \\([0-9]+\\)" nil t)
          (>= (string-to-number (match-string 1)) 2020))))))

;;netflix stuff
(defun netflix-templatize-file () "Insert a standard Netflix template comment into the current buffer."
       (let ((f (expand-file-name "~/.netflix.license")))
         (if (and (not (/= (point-min) (point-max))) (file-exists-p f))
             (insert-file-contents f) ;;insert the license
           )))
(defun netflix-log-message (msg nopercent)
  "Insert Netflix logging message."
  (let ((result))
    (cond ((or (eq major-mode 'js2-mode) (eq major-mode 'js-mode) (eq major-mode 'javascript-mode))
           (let ((quote (save-excursion
                          (save-restriction
                            (widen)
                            (goto-char (point-min))
                            (if (re-search-forward "\<let\>" nil t)
                                "`"
                              "\"")))))
             (insert (format "nrdp.l.error(%s%s%s" quote msg quote)))
           (setq result (point)))
          ((eq major-mode 'typescript-mode)
           (insert "nrdp.l.error(`" msg "`")
           (setq result (point)))
          (t
           (insert (if (netflix-has-NERROR) "NERROR" "Log::error") "(TRACE_LOG, \"" (car (hack-mode-printf-format)) ": " msg "\"" (cdr (hack-mode-printf-format)))
           (when (and (not nopercent) (string-match "%" msg))
             (insert ", ")
             (setq result (point)))))
    (insert ");")
    result))

(defun netflix-c-mode-hook ()
  (setq
   tab-width 4
   c-basic-offset 4
   cmake-tab-width 4
   indent-tabs-mode nil
   )
  ;; make new font for rest of qt keywords
  (make-face 'netflix-keywords-face)
  (set-face-foreground 'netflix-keywords-face "BlueViolet")
  (font-lock-add-keywords 'c++-mode
                          '(("\\<NF_[A-Z][_A-Za-z]*" . 'netflix-keywords-face)))
  (font-lock-add-keywords 'c++-mode
                          '(("\\<NRDP_[A-Z][_A-Za-z]*" . 'netflix-keywords-face)))
  (font-lock-add-keywords 'c++-mode
                          '(("\\<DEFINE_[A-Z][_A-Za-z]*" . 'netflix-keywords-face)))
  (font-lock-add-keywords 'c++-mode
                          '(("\\<DECLARE_[A-Z][_A-Za-z]*" . 'netflix-keywords-face)))
  )
(defun netflix-find-file-hook ()
  (setq
   cmake-tab-width 4
   indent-tabs-mode nil))
(setq hack-mode-netflix '("Netflix" netflix-templatize-file netflix-c-mode-hook netflix-find-file-hook netflix-log-message))

;;printf handling
(defun hack-mode-escape-arg (argument)
  (let ((result "")
        (start 0)
        end)
    (if (or (null (string-match "[^\"]" argument))
            (< (match-end 0) (length argument)))
        (while (string-match "[\"]" argument start)
          (setq end (match-beginning 0)
                result (concat result (substring argument start end)
                               "\\" (substring argument end (1+ end)))
                start (1+ end))))
    (concat result (substring argument start))))

(defun hack-mode-insert-debug-printf(&optional prefix msg nopercent)
  (interactive "P")
  (unless msg
    (setq msg (read-from-minibuffer "String: ")))
  (setq msg (hack-mode-escape-arg msg))
  (beginning-of-line)
  (indent-for-tab-command)
  (let ((msg-pos))
    (if (and (not prefix) (nth hack-mode-find-log-nth hack-mode))
        (setq msg-pos (funcall (nth hack-mode-find-log-nth hack-mode) msg nopercent))
      (progn
        (if (or (eq major-mode 'js2-mode) (eq major-mode 'js-mode))
            (insert "console.log('" msg "');")
          (progn (insert (concat "printf(\"" (car (hack-mode-printf-format)) ": " msg "\\n\"" (cdr (hack-mode-printf-format))))
                 (if (and (not nopercent) (string-match "%" msg))
                     (progn (insert ", ") (setq msg-pos (point))))
                 (insert "); fflush(stdout);")))))
    (unless (= (point) (point-at-eol)) (insert "\n"))
    (beginning-of-line)
    (indent-for-tab-command)
    (if msg-pos (goto-char msg-pos))))
(setq hack-mode-insert-debug-serial-value 0)

(defun hack-mode-insert-debug-serial(&optional prefix)
  (interactive "P")
  (hack-mode-insert-debug-printf prefix (format "%d" (setq hack-mode-insert-debug-serial-value (+ hack-mode-insert-debug-serial-value 1)))))

(defun hack-mode-insert-debug-code-line(prefix arg)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let* ((b (point))
           (text (progn
                   (end-of-line)
                   (buffer-substring-no-properties b (point)))))
      (if arg
          (progn
            (goto-char (1- (point-at-bol)))
            (insert "\n")
            (hack-mode-insert-debug-printf prefix text t))
        (forward-line 1)
        (skip-chars-forward " \t")
        (when (looking-at "{")
          (forward-line))
        (indent-for-tab-command)
        (hack-mode-insert-debug-printf prefix text t)))))

(defun hack-mode-insert-debug-code-line-pre(&optional prefix)
  (interactive "P")
  (hack-mode-insert-debug-code-line prefix t))

(defun hack-mode-insert-debug-code-line-post(&optional prefix)
  (interactive "P")
  (hack-mode-insert-debug-code-line prefix nil))

(defun hack-mode-templatize-file () "Maybe stick in a standard multiple-inclusion check for a header file"
       (if (and (buffer-file-name) (not buffer-read-only))
           (let ((name (file-name-nondirectory (buffer-file-name))) (empty-file (not (/= (point-min) (point-max)))))
             (if (nth hack-mode-templatize-nth hack-mode) (funcall (nth hack-mode-templatize-nth hack-mode)))
             (if (and empty-file name (string-match "\\.h$" name))
                 (let ((define (upcase (format hack-mode-templatize-header-format (file-name-sans-extension name)))))
                   (goto-char (point-max))
                   (insert "#ifndef " define "\n"
                           "#define " define "\n"
                           "\n\n\n"
                           "#endif /* " define " */\n")))
             (set-buffer-modified-p nil))))

;;generic stuff, this will allow me to toggle between my "known" styles.
(defvar hack-mode nil)
(setq hack-modes nil)
(make-variable-buffer-local 'hack-mode)
(put 'hack-mode 'permanent-local t)
(defun hack-mode-register (match mode) "Register mode as a new hack-mode"
       (setq hack-modes (append hack-modes (list (list match mode)))))
(defun hack-mode-set (&optional mode-name) "Sets up C hack mode"
       (interactive)
       (unless mode-name
         (setq mode-name (ido-completing-read "Mode: "
                                              (cl-remove-duplicates (let (result) (dolist (mode hack-modes result) (setq result (append result (list (car (nth 1 mode)))))))))))
       (if mode-name
           (dolist (mode hack-modes)
             (if (string= (car (nth 1 mode)) mode-name) (setq hack-mode (nth 1 mode))))))
(defun hack-mode-guess()
  (if hack-mode-buffer-license-type
      (hack-mode-set (downcase sam-buffer-license-type))
    (progn
      (setq hack-mode (block stop-guessing
                        (dolist (mode hack-modes)
                          (cond
                           ((functionp (car mode)) (if (funcall (car mode)) (return-from stop-guessing (nth 1 mode))))
                           ((stringp (car mode)) (if (and (buffer-file-name) (string-match (car mode) (buffer-file-name))) (return-from stop-guessing (nth 1 mode))))
                           (t nil)))))
      (unless hack-mode (hack-mode-set "Default")))))
(hack-mode-set "Default")

(add-hook 'find-file-hooks (lambda()
                             (hack-mode-guess)
                             (if (nth hack-mode-find-file-mode-nth hack-mode) (funcall (nth hack-mode-find-file-mode-nth hack-mode)))))
(add-hook 'c-mode-common-hook (lambda()
                                (hack-mode-guess)
                                (hack-mode-templatize-file)
                                ;;(hack-mode-license-remove) ;;find any license & remove it
                                (if (nth hack-mode-c-mode-nth hack-mode) (funcall (nth hack-mode-c-mode-nth hack-mode)))))

(provide 'hack-mode)
