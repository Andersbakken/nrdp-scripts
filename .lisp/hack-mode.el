(require 'cl)
(require 'ido)

(defconst hack-mode-templatize-nth 1)
(defconst hack-mode-c-mode-nth 2)
(defconst hack-mode-find-file-mode-nth 3)
(defconst hack-mode-find-log-nth 4)

(defvar hack-mode-printf-format (cons "[%s:%d]" ", __func__, __LINE__"))

;;default stuff (my preferred hack mode)
(defun default-find-file-hook ()
  (setq
   indent-tabs-mode nil
   insert-tab-mode nil
   ))
(setq hack-mode-default '("Default" nil nil default-find-file-hook))

;;troll stuff
(defun troll-templatize-file () "Insert a standard Troll template comment into the current buffer."
  (let ((f (expand-file-name "~/.troll.license")))
    (if (and (not (/= (point-min) (point-max))) (file-exists-p f))
        (insert-file f) ;;insert the license
      )))
(defun troll-log-message (msg &optional nopercent) "Insert Troll logging message."
  (let ((result))
    (insert (concat "qWarning(\"" (car hack-mode-printf-format) ": " msg "\"" (cdr hack-mode-printf-format)))
    (if (and (not nopercent) (string-match "%" msg)) (progn (insert ", ") (setq result (point))))
    (insert ");")
    result))
(defun troll-c-mode-hook ()
  (setq
   compile-command (format "make QTDIR=%s -kC %s/src" (getenv "QTDIR") (getenv "QTDIR"))
   ))
(defun troll-find-file-hook ()
  (setq
   indent-tabs-mode nil
   insert-tab-mode nil
   ))
(setq hack-mode-troll '("Troll" troll-templatize-file troll-c-mode-hook troll-find-file-hook troll-log-message))

;;webkit stuff
(defun webkit-templatize-file () "Insert a standard WebKit template comment into the current buffer."
  )
(defun webkit-find-file-hook ()
  (setq
   auto-clean-whitespace nil
   indent-tabs-mode nil
   insert-tab-mode nil
   ))
(defun webkit-c-mode-hook ()
  (c-set-offset 'innamespace 0)
  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                 "\\|protected slot\\|private\\|private slot"
                                 "\\)\\>")
        c-c++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                 "\\|public slots\\|protected slots\\|private slots"
                                 "\\)\\>[ \t]*:"))
  (progn
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
  )
(setq hack-mode-webkit '("WebKit" webkit-templatize-file webkit-c-mode-hook webkit-find-file-hook nil))

;;netflix stuff
(defun netflix-templatize-file () "Insert a standard Netflix template comment into the current buffer."
  (let ((f (expand-file-name "~/.netflix.license")))
    (if (and (not (/= (point-min) (point-max))) (file-exists-p f))
        (insert-file f) ;;insert the license
      )))
(defun netflix-log-message (msg nopercent) "Insert Netflix logging message."
  (let ((result))
    (if (or (eq major-mode 'js2-mode) (eq major-mode 'js-mode))
        (progn
          (insert "nrdp.log.error('" msg "'")
          (setq result (point)))
      (progn
        (insert (if (string-match "\.cpp$" (buffer-file-name))
                    "Log::error"
                  "netflix::base::Log::error")
                "(TRACE_LOG, \"" (car hack-mode-printf-format) ": " msg "\"" (cdr hack-mode-printf-format))
        (if (and (not nopercent) (string-match "%" msg)) (progn (insert ", ") (setq result (point))))))
    (insert ");")
    result))
(defun netflix-c-mode-hook () (setq
                               tab-width 4
                               c-basic-offset 4
                               ))
(defun netflix-find-file-hook ()
  (setq
   cmake-tab-width 4
   indent-tabs-mode nil
   insert-tab-mode nil
   ))
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
        (insert (concat "printf(\"" (car hack-mode-printf-format) ": " msg "\\n\"" (cdr hack-mode-printf-format)))
        (if (and (not nopercent) (string-match "%" msg))
            (progn (insert ", ") (setq msg-pos (point))))
        (insert "); fflush(stdout);")))
    (unless (= (point) (point-at-eol)) (insert "\n"))
    (beginning-of-line)
    (indent-for-tab-command)
    (if msg-pos (goto-char msg-pos))))
(setq hack-mode-insert-debug-serial-value 0)

(defun hack-mode-insert-debug-serial(&optional prefix)
  (interactive "P")
  (hack-mode-insert-debug-printf prefix (format "%d" (setq hack-mode-insert-debug-serial-value (+ hack-mode-insert-debug-serial-value 1)))))

(defun hack-mode-insert-debug-code-line(prefix arg)
  (beginning-of-line)
  (indent-for-tab-command)
  (let ((b (point-marker)))
    (end-of-line)
    (kill-ring-save b (point-marker))
    (next-line 1)
    (beginning-of-line)
    (indent-for-tab-command)
    (if (looking-at ": ")
        (progn
          (next-line 1)
          (beginning-of-line)))
    (indent-for-tab-command)
    (if (looking-at "{")
        (progn
          (next-line 1)
          (beginning-of-line)
          (indent-for-tab-command)))
    (hack-mode-insert-debug-printf prefix (current-kill 0) t)
    (previous-line 1)
    (beginning-of-line)
    (indent-for-tab-command)
    (next-line 1)
    (beginning-of-line)
    (indent-for-tab-command)
    (previous-line 1)
    (if arg (transpose-lines 1))))

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
            (let ((define (upcase (format "__%s_H__" (file-name-sans-extension name)))))
              (goto-char (point-max))
              (insert "#ifndef " define "\n"
                      "#define " define "\n"
                      "\n\\n\\n"
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
           (remove-duplicates (let (result) (dolist (mode hack-modes result) (setq result (append result (list (car (nth 1 mode)))))))))))
  (if mode-name
      (dolist (mode hack-modes)
        (if (string= (car (nth 1 mode)) mode-name) (setq hack-mode (nth 1 mode))))))
(defun hack-mode-guess()
  (setq hack-mode (block stop-guessing
                    (dolist (mode hack-modes)
                      (cond
                       ((functionp (car mode)) (if (funcall (car mode)) (return-from stop-guessing (nth 1 mode))))
                       ((stringp (car mode)) (if (and (buffer-file-name) (string-match (car mode) (buffer-file-name))) (return-from stop-guessing (nth 1 mode))))
                       (t nil)))))
  (unless hack-mode (hack-mode-set "Default")))
(hack-mode-set "Default")

(provide 'hack-mode)
