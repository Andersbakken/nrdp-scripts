(require 'delsel)
(require 'nrdp-git)
(require 'lsdev)
(require 'thingatpt)
(require 'bytecomp)
(require 's)

(defun --misc-find-deepest-ancestor-directory (file)
  (let ((ret)
        (dir default-directory))
    (while dir
      (let ((val (find-ancestor-file file dir)))
        (if (not val)
            (setq dir nil)
          (setq dir (file-name-directory val))
          (setq ret dir)
          (if (string= dir "/")
              (setq dir nil)
            (setq dir (expand-file-name (concat dir "/..")))))))
    ret))

(defun is-ancestor (root child)
  "Try to recursively go upwards from this directory and see if child is an ancestor of root"
  (let ((root-dir (cond (root ;; extrapolate from name
                         (if (equal (substring root -1) "/")
                             root
                           (concat root "/")))
                        (t default-directory) ;; hmm, use default
                        )))
    (while (not (or (string-equal child "/") (string-equal child root-dir)))
      (setq child (substring child 0 (string-match "[^/]*/?$" child))))
    ;; if we did found a file!
    (not (string= child "/"))))
(defalias 'sam-is-ancestor 'is-ancestor)

(defun find-ancestor-file (file-name &optional directory)
  (let ((dir (locate-dominating-file (or directory default-directory) file-name)))
    (and dir (expand-file-name (concat dir file-name)))))

(defalias 'sam-find-ancestor-file 'find-ancestor-file)

(defun misc-current-buffer()
  (if (string-match "^ ?\\*server\\*$" (buffer-name))
      (other-buffer)
    (current-buffer)))

(defun what-file (&optional name) ;;I use this function from emacsclient!
  (let* ((buffers (buffer-list)) (result))
    (while (and (not result) buffers)
      (let* ((buffer (car buffers)) (file-name (buffer-file-name buffer)))
        (when (and file-name (or (not name) (string-match name file-name)))
          (setq result file-name)
          (with-current-buffer buffer
            (save-restriction (widen) (setq result (format "%s:%d" result (line-number-at-pos)))))))
      (setq buffers (cdr buffers)))
    result))

(defalias 'sam-what-file 'what-file)

(defun what-directory (&optional name) ;;I use this function from emacsclient!
  (let* ((buffers (buffer-list))
         (result))
    (while (and (not result) buffers)
      (let* ((buffer (car buffers))
             (file-name (buffer-file-name buffer)))
        (if (and file-name (or (not name) (string-match name file-name)))
            (setq result (file-name-directory file-name))))
      (setq buffers (cdr buffers)))
    result))

(defalias 'sam-what-directory 'what-directory)

(defun rotate-windows (&optional toggle-split)
  "Rotate your windows or split the toggle"
  (interactive "P")
  (let (i numWindows)
    (if toggle-split
        (toggle-window-split)
      (cond ((not (> (count-windows) 1)) (message "You can't rotate a single window!"))
            (t
             (setq i 1)
             (setq numWindows (count-windows))
             (while (< i numWindows)
               (let* ((w1 (elt (window-list) i))
                      (w2 (elt (window-list) (+ (% i numWindows) 1)))
                      (b1 (window-buffer w1))
                      (b2 (window-buffer w2))
                      (s1 (window-start w1))
                      (s2 (window-start w2)))
                 (set-window-buffer w1 b2)
                 (set-window-buffer w2 b1)
                 (set-window-start w1 s2)
                 (set-window-start w2 s1)
                 (setq i (1+ i)))))))))

(defun toggle-window-split (&optional splitter)
  (interactive "P")
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges))))))
        (unless splitter (setq splitter
                               (if (= (car this-win-edges)
                                      (car (window-edges (next-window))))
                                   'split-window-horizontally
                                 'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; From Richard Mlynarik (mly@adoc.xerox.com)
;; Posting to comp.emacs 1995-01-02 21:06:39 PST
;; Subject Is there a "tail -f" mode
;; groups.google.com/groups?selm=MLY.95Jan2200227@fish.adoc.xerox.com
(defvar tailf-history nil)
(defun tailf-mark()
  (interactive) (setq buffer-read-only nil)
  (goto-char (point-max))
  (insert (format-time-string "\n\n============== MARK: %Y-%m-%d %H:%M:%S %Z =====================\n\n"))
  (setq buffer-read-only t))

(defun tailf-clear()
  (interactive) (setq buffer-read-only nil)
  (erase-buffer)
  (setq buffer-read-only t))

(defun tailf (file)
  (interactive "FFile: ")
  (setq file (expand-file-name file))
  ;; I sometimes get a timing screw
  (if (not (file-readable-p file))
      (error "%s doesn't exist" file))
  (let ((buffer (get-buffer-create (concat "*tail* " file))))
    (set-buffer buffer)
    (local-set-key "\C-\M-y" 'tailf-clear)
    (local-set-key "\C-\M-m" 'tailf-mark)
    (while (get-buffer-process buffer)
      (kill-process (get-buffer-process buffer)))
    (buffer-disable-undo (current-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq buffer-read-only t)
    (let ((process (start-process "tail -f" buffer "tail" "-100f" file)))
      (set-process-query-on-exit-flag process nil)
      (pop-to-buffer buffer) process)))

(defun --misc-replace-string-helper (from to &optional start end)
  (let ((count 0))
    (save-excursion
      (goto-char (or start (point-min)))
      (while (search-forward from end t)
        (incf count)
        (replace-match (or to "") nil t)))
    (and (> count 0) count)))

(defun --misc-replace-regexp-helper (from to &optional start end literal)
  (let ((count 0))
    (save-excursion
      (goto-char (or start (point-min)))
      (while (re-search-forward from end t)
        (incf count)
        (replace-match (or to "") nil literal)))
    (and (> count 0) count)))

;;====================
;; Carriage return bogusness
;;====================

(defun dos-to-unix ()
  "Replace \r\n with \n"
  (interactive)
  (--misc-replace-string-helper "\r\n" "\n"))

(defun unix-to-dos ()
  "Replace \n with \r\n"
  (interactive)
  (--misc-replace-string-helper "\n" "\r\n" ))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun ediff-cleanup-buffer-b-handler ()
  (if ediff-buffer-B
      (let ((b-window (get-buffer-window ediff-buffer-B)))
        (if b-window (delete-window b-window))
        (kill-buffer ediff-buffer-B))))

;;===================
;; Insert prints all over the board
;;===================
(require 'cc-mode)
(defvar litter-printf-function (lambda() (insert "\nprintf(\"%s:%d [%s]\\n\", __FILE__, __LINE__, __FUNCTION__);")))
(defun litter (trash &optional begin end)
  (interactive "sTrash: ")
  (unless begin
    (setq begin (or (if mark-active (region-beginning))
                    (save-excursion
                      (c-beginning-of-defun)
                      (while (not (or (eobp) (looking-at "{")))
                        (forward-char))
                      (if (eobp)
                          (point-max)
                        (+ (point) 1))))))
  (unless end
    (setq end (or (if mark-active (region-end))
                  (save-excursion
                    (c-end-of-defun)
                    (while (not (or (bobp) (looking-at "}")))
                      (backward-char))
                    (point)))))
  (goto-char begin)
  (let ((points (list (point))))
    (while (<= (point) end)
      (let ((old (point)))
        (c-end-of-statement)
        (if (= old (point))
            (goto-char end)
          (save-excursion
            (backward-char)
            (if (looking-at "; *$")
                (add-to-list 'points (+ (point) 1)))))))
    (while points
      (goto-char (car points))
      (if (functionp trash) (funcall trash) (insert trash))
      (setq points (cdr points))))
  (c-indent-defun)
  )

(defun litter-printf (&optional begin end)
  (interactive)
  (litter litter-printf-function))

(defun make-member-find-nested-classes ()
  (save-excursion
    (let (classes start)
      (goto-char (point-at-eol))
      (setq start (point))
      (while (and (re-search-backward "^\\([ \t]*\\)\\(class\\|struct\\)\\>[ \t]+\\([^:{]*\\)" nil t))
        (goto-char (match-beginning 1))
        (let ((match-start (match-beginning 3))
              (match-end (match-end 3)))
          (save-excursion
            (when (and (re-search-forward "[{;]" nil t)
                       (string= (match-string 0) "{"))
              (forward-char -1)
              (forward-sexp)
              (when (>= (point) start)
                (let* ((words (split-string (buffer-substring-no-properties match-start match-end) "[ \t\n]" t))
                       (len (1- (length words))))
                  (while (>= len 0)
                    (unless (string= (nth len words) "final")
                      (add-to-list 'classes (nth len words))
                      (setq len -1))
                    (decf len))))))))
      classes)))

(defun make-member-strip-default-arguments (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (search-forward "\"" nil t)
      (forward-char -1)
      (let ((start (point)))
        (forward-sexp)
        (delete-region start (point))))
    (goto-char (point-min))
    (while (search-forward "=" nil t)
      (forward-char -1)
      (skip-chars-backward "[\t ]")
      (let ((start (point)))
        (delete-region start (1- (or (and (search-forward "," nil t) (point))
                                     (point-max))))))
    (buffer-string)))

(defun misc-string-suffix-p (suffix string &optional ignore-case)
  "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (let ((start-pos (- (length string) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                string start-pos nil ignore-case)))))

(defun misc-string-prefix-p (str1 str2 &optional ignore-case)
  "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
  (eq t (compare-strings str1 nil nil
                         str2 0 (length str1) ignore-case)))

(defun misc-trim-string (string)
  (let ((ret (replace-regexp-in-string "\\`[ \t\n]*" ""
                                       (replace-regexp-in-string "[ \t\n]*\\'" "" string))))
    (and (> (length ret) 0) ret)))

(defun make-member-fixup-return-value (string)
  (let* ((inline)
         (ws "[ \t\n]")
         (words
          (with-temp-buffer
            (insert string)
            (setq inline (--misc-replace-regexp-helper "\\<inline\\>" nil))
            (--misc-replace-regexp-helper "\\<inline\\>" nil)
            (--misc-replace-string-helper "*" " * ")
            (--misc-replace-string-helper "&" " & ")
            (goto-char (point-min))
            (skip-chars-forward ws)
            (let ((ret)
                  (last (point)))
              (while (not (eobp))
                (cond ((looking-at "<")
                       (forward-char)
                       ;; (message "starting %s" (buffer-substring-no-properties (point) (point-max)))
                       (let ((count 1))
                         (while (cond ((eobp) nil)
                                      ((looking-at ">")
                                       (forward-char)
                                       (decf count)
                                       (> count 0))
                                      ((looking-at "<")
                                       (forward-char)
                                       (incf count)
                                       t)
                                      (t (forward-char) t))))
                       (push (buffer-substring-no-properties last (point)) ret)
                       (skip-chars-forward ws)
                       (setq last (point)))
                      ((looking-at ws)
                       (push (buffer-substring-no-properties last (point)) ret)
                       (skip-chars-forward ws)
                       (setq last (point)))
                      (t (forward-char))))
              (when (< last (point))
                (push (buffer-substring-no-properties last (point)) ret))
              (nreverse ret))))
         (ret)
         (seenvar)
         (len (1- (length words))))
    ;;    (message "[%s]" (combine-and-quote-strings words "|"))
    (when words
      (while (>= len 0)
        (let ((word (nth len words)))
          (if (not seenvar)
              (unless (or (string= word "*")
                          (string= word "&")
                          (string= word "const"))
                (setq seenvar t))
            (unless (cond ((string= word "const"))
                          ((string= word "*"))
                          ((string= word "&"))
                          ((string= word "unsigned"))
                          ((string= word "long")))
              (while (>= len 0)
                (decf len)
                (pop words)))))
        (decf len))
      (setq ret (replace-regexp-in-string "\\([\\*&]\\) +" "\\1" (mapconcat 'identity words " ")))
      (cons (cond ((misc-string-suffix-p "&" ret) ret)
                  ((misc-string-suffix-p "*" ret) ret)
                  ((> (length ret) 0) (concat ret " "))
                  (t nil)) inline))))

(defun make-member-create-function-definition ()
  (let ((classes (make-member-find-nested-classes)))
    (when classes
      (save-excursion
        (goto-char (point-at-bol))
        (let ((returnstart)
              (returnend)
              (return)
              (paramsstart)
              (functionnamestart)
              (functionnameend)
              (params)
              (const))
          (skip-chars-forward "[\t ]")
          (when (looking-at "virtual\\>")
            (forward-char 7)
            (skip-chars-forward "[\t ]"))
          (setq returnstart (point))
          (when (search-forward "(" (point-at-eol) t)
            (forward-char -1)
            (setq paramsstart (point))
            (skip-chars-backward "[\t ]")
            (setq functionnameend (point))
            (skip-chars-backward "[A-Za-z0-9_~+-*/%=!><!&^,]")
            (skip-chars-forward "[0-9+-*/%=!><!&^,]")
            (setq functionnamestart (point))
            (skip-chars-backward "[\t ]")
            (setq returnend (point))
            (goto-char paramsstart)
            (forward-sexp)
            (setq params (make-member-strip-default-arguments (buffer-substring-no-properties paramsstart (point))))
            (skip-chars-forward "[\t ]")
            (setq return (make-member-fixup-return-value (buffer-substring-no-properties returnstart returnend)))
            (when (or return
                      (let ((name (buffer-substring-no-properties functionnamestart functionnameend)))
                        (cond ((string= name (car (last classes))))
                              ((and (string-match "^~" name)
                                    (string= (car (last classes)) (substring name 1))))
                              (t nil))))
              (when (looking-at "\\<const\\>")
                (setq const " const")
                (goto-char (match-end 0)))
              (skip-chars-forward "[\t\n ]")
              (cond ((looking-at "//")
                     (goto-char (point-at-eol))
                     (skip-chars-forward "[\t\n ]"))
                    ((looking-at "/\\*")
                     (search-forward "\\*/")
                     (skip-chars-forward "[\t\n ]"))
                    (t))
              (if (not (looking-at "{"))
                  (cons (concat (if (cdr return)
                                    "inline ")
                                (car return)
                                (combine-and-quote-strings classes "::")
                                "::"
                                (buffer-substring-no-properties functionnamestart functionnameend)
                                params
                                const)
                        (point))
                (forward-sexp)
                (cons nil (point))))))))))

;;skeleton thingie
(defun make-member ()
  "make a skeleton member function in the .cpp file"
  (interactive)
  (let* ((insertion-data (make-member-create-function-definition))
         (insertion-string (car insertion-data))
         (end (cdr insertion-data))
         (inline (and insertion-string (misc-string-prefix-p "inline" insertion-string)))
         (old)
         (file (buffer-file-name))
         (other-file))
    (when end
      (goto-char end)
      (condition-case nil
          (forward-sexp)
        (error)))
    (when (and insertion-string file)
      (setq other-file (concat (file-name-sans-extension file) ".cpp"))
      (setq old (cons (point) (current-buffer)))
      (if inline
          (progn
            (if (search-forward insertion-string nil t)
                (setq insertion-string nil)
              (goto-char (point-max))
              (re-search-backward "^#endif" (- (point-max) 200) t)))
        (when (not (switch-cpp-h))
          (find-file other-file))
        (goto-char (point-min))
        (if (search-forward insertion-string nil t)
            (setq insertion-string nil)
          (goto-char (point-max))))
      (if (not insertion-string)
          (progn
            (message "It's already there!")
            (switch-to-buffer (cdr old))
            (goto-char (car old)))
        (unless (looking-back "\n\n" (point-at-bol))
          (insert "\n"))
        (insert insertion-string "\n{\n}\n")
        (unless inline
          (save-excursion
            (goto-char (point-min))
            (let ((include (concat "#include \"" (file-name-nondirectory file) "\""))
                  (includerx (concat "#include [\"<]" (file-name-nondirectory file) "[\">]")))
              (unless (search-forward-regexp includerx nil t)
                (when (looking-at "/\\*")
                  (search-forward "*/")
                  (insert "\n\n"))
                (insert include "\n")))))
        (re-search-backward "}")))))

(defalias 'agulbra-make-member 'make-member)

(defun keyboard-quit-kill-minibuffer ()
  (interactive)
  (if (and (not (window-minibuffer-p)) (active-minibuffer-window))
      (progn
        (select-window (active-minibuffer-window))
        (minibuffer-keyboard-quit))
    (progn
      (setq mark-active nil)
      (keyboard-quit)))
  )

(defun find-corresponding-cpp-h (&optional fileorbuffer)
  (save-excursion
    (let ((name (file-name-nondirectory (cond ((bufferp fileorbuffer) (buffer-file-name fileorbuffer))
                                              ((stringp fileorbuffer) fileorbuffer)
                                              (t (buffer-file-name)))))
          (candidates))
      (when (string-match "\\.ui.h$" name)
        (add-to-list 'candidates (replace-match ".ui" t t name) t))
      (when (string-match "\\.ui$" name)
        (add-to-list 'candidates (replace-match ".ui.h" t t name) t))
      (when (string-match "_p\\.h$" name)
        (add-to-list 'candidates (replace-match ".cpp" t t name) t))
      (when (string-match "\\.h$" name)
        (add-to-list 'candidates (replace-match ".cc" t t name) t))
      (when (string-match "\\.h$" name)
        (add-to-list 'candidates (replace-match ".C" t t name) t))
      (when (string-match "\\.h$" name)
        (add-to-list 'candidates (replace-match ".mm" t t name) t))
      (when (string-match "\\.h$" name)
        (add-to-list 'candidates (replace-match ".m" t t name) t))
      (when (string-match "\\.h$" name)
        (add-to-list 'candidates (replace-match ".c" t t name) t))
      (when (string-match "\\.h$" name)
        (add-to-list 'candidates (replace-match ".cpp" t t name) t))
      (when (string-match "_p\\.h$" name)
        (add-to-list 'candidates (replace-match "_p.cpp" t t name) t))
      (when (string-match "\\.h$" name)
        (add-to-list 'candidates (replace-match "_p.cpp" t t name) t))
      (when (string-match "\\.hpp$" name)
        (add-to-list 'candidates (replace-match ".cpp" t t name) t))
      (when (string-match "\\.hxx$" name)
        (add-to-list 'candidates (replace-match ".cxx" t t name) t))
      (when (string-match "_x11\\.cpp$" name)
        (add-to-list 'candidates (replace-match ".h" t t name) t))
      (when (string-match "_unix\\.cpp$" name)
        (add-to-list 'candidates (replace-match ".h" t t name) t))
      (when (string-match "_mac\\.cpp$" name)
        (add-to-list 'candidates (replace-match ".h" t t name) t))
      (when (string-match "_thn\\.cpp$" name)
        (add-to-list 'candidates (replace-match ".h" t t name) t))
      (when (string-match "_win\\.cpp$" name)
        (add-to-list 'candidates (replace-match ".h" t t name) t))
      (when (string-match "_qws\\.cpp$" name)
        (add-to-list 'candidates (replace-match ".h" t t name) t))
      (when (string-match ".cpp$" name)
        (add-to-list 'candidates (replace-match "_p.h" t t name) t))
      (when (string-match ".cpp$" name)
        (add-to-list 'candidates (replace-match ".h" t t name) t))
      (when (string-match ".cc$" name)
        (add-to-list 'candidates (replace-match ".h" t t name) t))
      (when (string-match ".c$" name)
        (add-to-list 'candidates (replace-match ".h" t t name) t))
      (when (string-match ".cpp$" name)
        (add-to-list 'candidates (replace-match ".hpp" t t name) t))
      (when (string-match ".cxx$" name)
        (add-to-list 'candidates (replace-match ".hxx" t t name) t))
      (when (string-match "\\.mm$" name)
        (add-to-list (replace-match ".h" t t name) candidates t))
      (when (string-match "\\.m$" name)
        (add-to-list 'candidates (replace-match ".h" t t name) t))
      (goto-char (point-min))
      (when (and (or (string-match "\\.cpp$" name)
                     (string-match "\\.c$" name)
                     (string-match "\\.cxx$" name)
                     (string-match "\\.cc$" name)
                     (string-match "\\.C$" name)
                     (string-match "\\.mm$" name)
                     (string-match "\\.m$" name))
                 (re-search-forward "^# *include *[\"<]\\([^\">]*\\)" nil t))
        (add-to-list 'candidates (buffer-substring-no-properties (match-beginning 1) (match-end 1)) t))

      (goto-char (point-min))
      (when (re-search-forward "\\(//\\|/\\*\\) *SWITCH_FILE: *\"" nil t)
        (let ((start (point)))
          (when (search-forward "\"" (point-at-eol) t)
            (add-to-list 'candidates (buffer-substring start (point))))))

      ;; (message "candidates are: %s" (combine-and-quote-strings candidates))

      (or (dolist (candidate candidates)
            (when (file-readable-p candidate)
              ;; (message "Found in simple search: %s" candidate)
              (return candidate)))
          (and (rtags-has-filemanager)
               (dolist (candidate candidates)
                 (with-temp-buffer
                   (rtags-call-rc "-K" "-A" "-P" candidate)
                   (when (eq (count-lines (point-min) (point-max)) 1)
                     ;; (message "Found in rtags search: %s" candidate)
                     (return (buffer-substring (point-min) (- (point-max) 1)))))))))))

(defun switch-cpp-h ()
  "Switch to the corresponding .cpp, .C, .cc or .h file."
  (interactive)
  (let ((found (find-corresponding-cpp-h)))
    (when found
      (find-file found))))

(defun point-is-at-eol (pos) (save-excursion (goto-char pos) (= pos (point-at-eol))))
(defun point-is-at-bol (pos) (save-excursion (goto-char pos) (= pos (point-at-bol))))
(defun smart-comment-or-uncomment-region (&optional prefix)
  (interactive "P")
  (let ((start (if (region-active-p) (point) (point-at-bol)))
        (end (if (region-active-p) (mark) (point-at-eol)))
        (needs-c-style-override nil))
    (if (< end start)
        (let ((tmp end))
          (setq end start)
          (setq start tmp)))
    (if (and
         (eq major-mode 'c++-mode)
         (not (and (or (point-is-at-eol start) (point-is-at-bol start))
                   (or (point-is-at-eol end) (point-is-at-bol end)))))
        (setq needs-c-style-override t))
    (if (and needs-c-style-override
             (= (point-at-bol) start)
             (= (point-at-eol) end))
        (setq needs-c-style-override nil))

    (if needs-c-style-override
        (progn
          (setq comment-start "/*")
          (setq comment-end "*/")))
    (if prefix
        (uncomment-region start end)
      (comment-or-uncomment-region start end prefix))
    (if needs-c-style-override
        (progn
          (setq comment-start "// ")
          (setq comment-end ""))))
  )

;; ================================================================================
;; Super keyboard-quit C-g
;; ================================================================================

(defun smart-keyboard-quit ()
  (interactive)
  (if (and (not (window-minibuffer-p)) (active-minibuffer-window))
      (progn
        (select-window (active-minibuffer-window))
        (minibuffer-keyboard-quit))
    (progn
      (setq mark-active nil)
      (keyboard-quit)))
  )

;; ================================================================================
;; strerror
;; ================================================================================

(defun strerror ()
  (interactive)
  (if (executable-find "strerror")
      (let* ((def (current-word))
             (query (read-from-minibuffer (concat "Strerror (default " def "): "))))
        (if (string= query "")
            (setq query def))
        (if (and query (> (length query) 0))
            (let ((res (shell-command-to-string (concat "strerror " query))))
              (message (substring res 0 (1- (length res))))))
        )
    (message "No strerror in $PATH"))
  )

;;====================
;; clean up white spaces hook
;;===================
(defun sam-fix-tabs (b e) (if indent-tabs-mode (tabify b e) (untabify b e)))
(defun sam-fix-tabs-region ()
  (interactive)
  (sam-fix-tabs  (region-beginning) (region-end)) (untabify  (region-beginning) (region-end)))
(defun sam-clean-out-spaces ()
  "Remove spaces at ends of lines"
  (interactive)
  (sam-fix-tabs (point-min) (point-max))
  (and (not buffer-read-only)
       (save-excursion
         (goto-char (point-min))
         (let ((count 0)
               (bmp (buffer-modified-p)))
           (while (re-search-forward "[ \t]+$" nil t)
             (setq count (1+ count))
             (replace-match "" t t))
           (and (> count 0)
                (progn
                  (set-buffer-modified-p bmp)
                  (message "Cleaned %d lines" count)))))))
(defvar-local sam-auto-clean-whitespace nil)
(defun sam-c-clean-out-spaces-hooked ()
  "Cleanup spaces, only in c mode"
  (interactive)
  (if (and sam-auto-clean-whitespace (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode)))
      (sam-clean-out-spaces))
  nil)
(add-hook 'write-file-hooks 'sam-c-clean-out-spaces-hooked)

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;;====================
;; insert loops
;;===================

(defun find> ()
  (let ((count 1) (ok t))
    (forward-char)
    (while (and (> count 0) (< (point) (point-max)))
      (let ((char (elt (buffer-string) (1- (point)))))
        ;; (message "Foobar %c %d %d" char (point) count)
        (cond ((= char 60) (incf count)) ;; <
              ((= char 62) (decf count)) ;; >
              ((= char 10) (setq count 0) (setq ok nil))
              (t)))
      (if (> count 0)
          (forward-char)))
    ok)
  )

(defun find-containers (&optional buffer-name)
  (unless buffer-name
    (setq buffer-name (buffer-file-name)))
  (let (containers typedefs)
    ;; (if buffer
    ;;     (let ((prev (current-buffer)))
    ;;       (set-buffer buffer)
    ;;       (setq contents (buffer-string))
    ;;       (set-buffer prev))
    ;;   (setq contents (buffer-string)))
    (with-temp-buffer
      (insert-file-contents-literally buffer-name)
      (let ((corresponding-file (find-corresponding-cpp-h buffer-name)))
        (when corresponding-file
          (insert-file-contents-literally corresponding-file)))
      ;; (message "string:\n%s" (buffer-substring-no-properties (point-min) (point-max)))

      ;; (message "buffer %s %d" buffer-name (point-max))
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (re-search-forward "<[A-Za-z_]" nil t))
        (let ((before (buffer-substring-no-properties (point-at-bol) (1- (point)))))
          ;; (message "got before [%s]" before)
          (when (not (string-match "^#include *<" before))
            (let (start end namestart name type pointer)
              ;; ((string-match "^ *typedef +" before)
              ;;  (let ((start (- (point) 2)) end)
              ;;    (when (find>)
              ;;      (setq end (1+ (point)))
              ;;      (goto-char start)
              ;;      (skip-chars-backward "A-Za-z0-9:_")
              ;;      (message "GOT SHIT [%s]" (buffer-substring-no-properties (point) end)))))
              ;; (t
              (backward-char 2)
              (setq start (point))
              (when (find>)
                (forward-char)
                (setq end (point))
                (skip-chars-forward " &")
                (setq pointer (> (skip-chars-forward "* ") 0))
                (setq namestart (point))
                (skip-chars-forward "A-Za-z0-9_")
                (setq name (buffer-substring-no-properties namestart (point)))
                (goto-char start)
                (skip-chars-backward " ")
                (skip-chars-backward "A-Za-z0-9:_")
                (setq type (buffer-substring-no-properties (point) end))
                ;; (message "type: [%s] name: [%s]" type name)
                (unless (or (string-match "^[^<]*\\(ptr\\|pointer\\|Ptr\\|Pointer\\)" type) (string= name ""))
                  (if (string-match "^ *typedef +" before)
                      (add-to-list 'typedefs name t)
                    (add-to-list 'containers (concat type (if pointer " *" " ") name))))
                (goto-char start)))))
        (if (< (point-at-eol) (point-max))
            (goto-char (1+ (point-at-eol)))
          (goto-char (point-max))))
      (while typedefs
        (goto-char (point-min))
        ;; (message "Searching for: [%s]" (concat (car typedefs) " *[&*]? *\\([A-Za-z0-9_]+\\)"))
        (while (and (< (point) (point-max))
                    (re-search-forward (concat "[A-Za-z0-9_:]*" (car typedefs) " *\\(&\\)?\\*? *[A-Za-z0-9_]+") nil t))
          (if (match-string 1)
              (add-to-list 'containers
                           (concat
                            (buffer-substring-no-properties (match-beginning 0) (match-beginning 1))
                            (buffer-substring-no-properties (match-end 1) (match-end 0))))
            (add-to-list 'containers (match-string 0)))
          ;; (message "match: [%s]" (match-string 1))
          (goto-char (point-at-eol)))
        (setq typedefs (cdr typedefs))))
    containers))

(defvar for-loop-space " ")
(defun insert-loop (&optional prefix erase)
  (interactive "P")
  (when (cond ((integerp prefix) (setq erase t))
              ((and prefix (listp prefix))
               (let ((start (point)))
                 (insert "for (int i=0; i<10; ++i) {\n")
                 (insert "}\n")
                 (indent-region start (point)))
               nil)
              (t))
    (indent-for-tab-command)
    (let (container it)
      (cond ((stringp prefix) (setq container prefix))
            ((region-active-p) (setq container (buffer-substring-no-properties (region-beginning) (region-end))))
            (t
             (save-excursion
               (let ((containers (or (find-containers)
                                     (list "std::map<std::string, int> map"))))
                 (add-to-list 'containers "")
                 (setq container (ido-completing-read "Container: " containers))
                 (if (string= container "")
                     (setq container (read-from-minibuffer "Container: ")))))))
      (when (not (string= container ""))
        (setq it (read-from-minibuffer "Iterator name (default 'it'): "))
        (let (name type (space (string-match " [^ ]*$" container)) (dot "."))
          (unless space
            (error "Invalid container %s" container))
          (setq type (substring container 0 space))
          (setq name (substring container (1+ space)))
          (when (= (elt name 0) 42)
            (setq name (substring name 1))
            (setq dot "->"))
          (if (string= it "")
              (setq it "it"))
          (when (region-active-p)
            (goto-char (point-at-eol))
            (insert "\n")
            (indent-according-to-mode))
          (let ((start (point)))
            (if erase
                (insert (format "%s::iterator %s = %s%sbegin();\nwhile%s(%s != %s%send()) {\nif%s(remove) {\n%s%serase(%s++);\n} else {\n++%s;\n}\n}"
                                type it name dot for-loop-space it name dot for-loop-space name dot it it))
              (insert (format "for%s(%s::const_iterator %s = %s%sbegin(); %s != %s%send(); ++%s) {\n\n}\n"
                              for-loop-space type it name dot it name dot it)))
            (indent-region start (point)))
          (forward-line (if erase -5 -2))
          (indent-according-to-mode))))))

;;=====================
;; compilation parsing
;;=====================
;;(setq compilation-error-regexp-alist '(absoft ada aix ant bash borland python-tracebacks-and-caml comma cucumber edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line gcc-include ruby-Test::Unit lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblitn))
(defvar compilation-note-regexp '("\\([A-Za-z_./+-][A-Za-z0-9_.+-]*/\\(?:/\\|\\.[CHMchm]\\)[A-Za-z0-9_./+-]*\\)\\(: \\)" 1 nil nil (nil . 2)))
(defvar compilation-error-warning-regexp '("\\([A-Za-z_./+-][A-Za-z0-9_.+-]*\\(?:/\\|\\.[CHMchm]\\)[A-Za-z0-9_./+-]*\\):\\([0-9]+\\):?\\([0-9]+\\)?:? ?\\([Ww]arning: \\)?\\([Nn]ote: \\)?" 1 2 3 (4 . 5)))
(defvar compilation-gnu '(
                          ;; The first line matches the program name for

                          ;;     PROGRAM:SOURCE-FILE-NAME:LINENO: MESSAGE

                          ;; format, which is used for non-interactive programs other than
                          ;; compilers (e.g. the "jade:" entry in compilation.txt).

                          ;; This first line makes things ambiguous with output such as
                          ;; "foo:344:50:blabla" since the "foo" part can match this first
                          ;; line (in which case the file name as "344").  To avoid this,
                          ;; the second line disallows filenames exclusively composed of
                          ;; digits.

                          ;; Similarly, we get lots of false positives with messages including
                          ;; times of the form "HH:MM:SS" where MM is taken as a line number, so
                          ;; the last line tries to rule out message where the info after the
                          ;; line number starts with "SS".  --Stef

                          ;; The core of the regexp is the one with *?.  It says that a file name
                          ;; can be composed of any non-newline char, but it also rules out some
                          ;; valid but unlikely cases, such as a trailing space or a space
                          ;; followed by a -, or a colon followed by a space.

                          ;; The "in \\|from " exception was added to handle messages from Ruby.
                          "^\\(?:[[:alpha:]][-[:alnum:].]+: ?\\|[ \t]+\\(?:in \\|from \\)\\)?\
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\): ?\
\\([0-9]+\\)\\(?:[.:]\\([0-9]+\\)\\)?\
\\(?:-\\([0-9]+\\)?\\(?:\\.\\([0-9]+\\)\\)?\\)?:\
\\(?:.*ICECC.*\\)?:\
\\(?: *\\(\\(?:Future\\|Runtime\\)?[Ww]arning\\|W:\\)\\|\
 *\\([Ii]nfo\\(?:\\>\\|rmationa?l?\\)\\|I:\\|instantiated from\\|[Nn]ote\\)\\|\
 *[Ee]rror\\|\[0-9]?\\(?:[^0-9\n]\\|$\\)\\|[0-9][0-9][0-9]\\)"
                          1 (2 . 4) (3 . 5) (6 . 7)))

                                        ; (setq compilation-error-regexp-alist nil)

(require 'compile)
;; (setq compilation-error-regexp-alist (delete 'gnu compilation-error-regexp-alist))
;; (add-to-list 'compilation-error-regexp-alist compilation-gnu)
;; (add-to-list 'compilation-error-regexp-alist compilation-error-warning-regexp)
;; (add-to-list 'compilation-error-regexp-alist compilation-note-regexp)

;; (defun compilation-parse-errors-filename (filename)
;;   (cond ((equal major-mode 'grep-mode) filename)
;;         ((null filename) filename)
;;         ((not (file-name-absolute-p filename)) filename)
;;         ((file-exists-p filename) filename)
;;         (t nil)))

(setq compilation-parse-errors-filename-function nil) ;; (function compilation-parse-errors-filename))

;; (define-key global-map [remap goto-line] 'goto-line-with-feedback)
(defun --misc-goto-line-helper (N)
  (goto-char (point-min))
  (forward-line (1- N)))

(defvar goto-line-with-feedback-history nil)
(defun goto-line-with-feedback (&optional nolinum)
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive "P")
  (setq nolinum (or nolinum (> (point-max) 524288)))
  (let ((mode (cond (nolinum nil)
                    ((bound-and-true-p git-gutter-mode) 'git-gutter-mode)
                    ((bound-and-true-p git-gutter+-mode) 'git-gutter+-mode)
                    (t nil)))
        (hadlinum (bound-and-true-p linum-mode)))
    (unwind-protect
        (progn
          (unless (or nolinum hadlinum)
            (linum-mode 1))
          (when mode
            (funcall mode 0))

          (let ((res (read-from-minibuffer "Goto line: " nil nil nil 'goto-line-with-feedback-history)))
            (cond ((string-match "^,\\([0-9]+\\)$" res)
                   (goto-char (1+ (string-to-number (match-string 1 res)))))
                  ((string-match "^\\([0-9]+\\)%$" res)
                   (goto-char (/ (* (point-max) (string-to-number (match-string 1 res))) 100)))
                  ((string-match "^:?\\([0-9]+\\):\\([0-9]+\\):?$" res)
                   (--misc-goto-line-helper (string-to-number (match-string 1 res)))
                   (forward-char (1- (string-to-number (match-string 2 res)))))
                  (t (--misc-goto-line-helper (string-to-number res))))))
      (unless (or nolinum hadlinum)
        (linum-mode -1))
      (when mode
        (funcall mode 1)))))

;; ================================================================================
;; misc-git-gutter+
;; ================================================================================
;; (define-key global-map (kbd "C-x g =") (function misc-git-gutter+-show-hunk))
;; (define-key global-map (kbd "C-x g d") (function misc-git-gutter+-show-hunk))
;; (define-key global-map (kbd "C-x g n") (function misc-git-gutter+-next-hunk))
;; (define-key global-map (kbd "C-x g p") (function misc-git-gutter+-previous-hunk))
;; (define-key global-map (kbd "C-x g r") (function misc-git-gutter+-revert-hunk))
(defvar misc-git-gutter+-transient-map (make-sparse-keymap))
(define-key misc-git-gutter+-transient-map (kbd "n") (function misc-git-gutter+-next-hunk))
(define-key misc-git-gutter+-transient-map (kbd "N") (function misc-git-gutter+-previous-hunk))
(define-key misc-git-gutter+-transient-map (kbd "p") (function misc-git-gutter+-previous-hunk))
(define-key misc-git-gutter+-transient-map (kbd "P") (function misc-git-gutter+-next-hunk))
(define-key misc-git-gutter+-transient-map (kbd "=") (function misc-git-gutter+-show-hunk))
(define-key misc-git-gutter+-transient-map (kbd "d") (function misc-git-gutter+-show-hunk))
(define-key misc-git-gutter+-transient-map (kbd "r") (function misc-git-gutter+-revert-hunk))

(defun misc-git-gutter+-next-hunk (arg)
  (interactive "p")
  (git-gutter+-next-hunk arg)
  (set-transient-map misc-git-gutter+-transient-map))

(defun misc-git-gutter+-previous-hunk (arg)
  (interactive "p")
  (git-gutter+-previous-hunk arg)
  (set-transient-map misc-git-gutter+-transient-map))

(defun misc-git-gutter+-show-hunk ()
  (interactive)
  (call-interactively 'git-gutter+-show-hunk)
  (set-transient-map misc-git-gutter+-transient-map))

(defun misc-git-gutter+-revert-hunk ()
  (interactive)
  (git-gutter+-revert-hunks)
  (set-transient-map misc-git-gutter+-transient-map))

;; ================================================================================
;; misc-occur
;; ================================================================================
;; (define-key global-map (kbd "C-M-o") (function misc-occur))

(defun misc-occur-data ()
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "^[0-9]+ match\\(es\\)\? for \"\\(.*\\)\" in buffer: \\(.*\\)")
        (cons (buffer-substring-no-properties (match-beginning 2) (match-end 2))
              (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
  )

(defun misc-is-occur-buffer () (string-match (buffer-name) "*Occur*"))

(defun misc-occur-read-from-minibuffer ()
  (let* ((word (current-word))
         (pattern
          (read-from-minibuffer
           (if (or (not word) (string= "" word))
               "List lines matching regexp: "
             (format "List lines matching regexp (default: %s): " word)))))
    (if (or (not pattern) (string= pattern ""))
        word
      pattern)))

(defun misc-occur (&optional dont-reoccur)
  (interactive "P")
  (if (and (not dont-reoccur) (misc-is-occur-buffer))
      (let* ((pattern (misc-occur-read-from-minibuffer))
             (data (and pattern (misc-occur-data)))
             (phrase (car data))
             (buffer (cdr data)))
        (delete-window)
        (with-current-buffer buffer
          (when (and pattern data)
            (if (string-match "^\\\\(\\(.*\\)\\\\)$" phrase)
                (occur (concat "\\(" (match-string 1 phrase) "\\|" pattern "\\)"))
              (occur (concat "\\(" phrase "\\|" pattern "\\)"))))))
    (call-interactively 'occur))
  )

(defun insert-namespace (&optional namespace)
  (interactive "P")
  (setq namespace (cond ((stringp namespace) namespace)
                        ((and namespace (listp namespace)) (read-from-minibuffer "Namespace: "))
                        (t "netflix")))
  (let ((ins (concat "using namespace " namespace ";")))
    (save-excursion
      (goto-char (point-min))
      (when (not (search-forward ins nil t))
        (goto-char (point-max))
        (if (cond ((re-search-backward "^ *using namespace" nil t) (goto-char (point-at-eol)))
                  ((re-search-backward "^ *# *include" nil t) (goto-char (point-at-eol)) (insert "\n"))
                  (t nil))
            (insert "\n" ins))))))

(defun nrdp-compiled-bridge (&optional prefix srcdir)
  (interactive "p")
  (lsdev-open-build-file (concat "src/platform/gibbon/data/resources/js/"
                                 (cond ((stringp prefix)
                                        prefix)
                                       ((and prefix (listp prefix))
                                        (read-from-minibuffer "File: " (file-name-nondirectory (buffer-file-name))))
                                       (t (file-name-nondirectory (buffer-file-name)))))))

(defun NetflixBridge.js (&optional srcdir)
  (interactive)
  (lsdev-open-build-file "src/platform/gibbon/data/resources/js/NetflixBridge.js" srcdir))

(defun PartnerBridge.js (&optional srcdir)
  (interactive)
  (lsdev-open-build-file "src/platform/gibbon/data/resources/js/PartnerBridge.js" srcdir))

(defun netflix.log (&optional srcdir)
  (interactive)
  (lsdev-open-build-file "src/platform/gibbon/data/netflix.log" srcdir))

(defun netflix-conf (&optional srcdir)
  (interactive)
  (lsdev-open-build-file (concat "src/platform/gibbon/data/etc/conf/"
                                 (completing-read "File: " (list "1080.xml" "common.xml" "config.xml" "gibbon.xml" "graphics.xml" "input.xml" "oem.xml" "rs-client.xml" "rs-server.xml" "stress.xml" "test.xml")))
                         srcdir))

(defun sam-commandline-arg (s)
  "Detect switch on commandline and interpret it."
  (let ((args command-line-args)
        (ret nil))
    (while args
      (let ((arg (downcase (car args)))(value nil))
    (setq args (cdr args))
    (cond
         ((equal arg (concat "-" s))
          (progn
            (setq value t)
            (if (and args (not (string-match "^-" (car args)))) (setq value (car args)))
        ))
         ((string-match (concat "--" s "=") arg) (setq value (replace-match "" t t arg)))
         )
    (if value (progn (setq ret value) (setq args nil)))))
    ret))

;; ================================================================================
;; misc-isearch
;; ================================================================================
;; (define-key isearch-mode-map (kbd "C-w") (function misc-isearch-yank-word-or-char-from-beginning-of-symbol))
;; (define-key isearch-mode-map (kbd "M-w") (function isearch-yank-word-or-char))

(defun misc-isearch-yank-word-or-char-from-beginning-of-symbol ()
  "Move to beginning of symbol before yanking word in isearch-mode."
  (interactive)
  (if (and (= 0 (length isearch-string)) (bounds-of-thing-at-point 'symbol))
      (let ((start (point)))
        (beginning-of-thing 'symbol)
        (isearch-yank-char (- start (point)))))
  (isearch-yank-word-or-char))

;; ================================================================================
;; mktest
;; ================================================================================

(defun misc-is-compiled (el)
  (let ((elc (byte-compile-dest-file el)))
    (and (file-exists-p elc)
         (not (file-newer-than-file-p el elc)))))

(defun misc-check-pattern (pattern file)
  (cond ((stringp pattern) (string-match pattern file))
        ((functionp pattern) (funcall pattern file))
        (t)))

(defun misc-find-files (directory &optional pattern norecurse cb exclude)
  (let ((files (and (file-directory-p directory)
                    (directory-files-and-attributes directory t nil t)))
        (ret (unless (null cb) 0)))
    (while files
      (let ((file (car files)))
        (when (or (not exclude)
                  (not (misc-check-pattern exclude (car file))))
          (cond ((misc-string-suffix-p "/." (car file)))
                ((misc-string-suffix-p "/.." (car file)))
                ((eq (nth 1 file) t)
                 (unless norecurse
                   (if cb
                       (incf ret (misc-find-files (car file) pattern norecurse cb exclude))
                     (setq ret (append ret (misc-find-files (car file) pattern norecurse cb exclude))))))
                ((and (not (nth 1 file)) ;; skip-symlinks
                      (misc-check-pattern pattern (car file)))
                 (if (not cb)
                     (push (car file) ret)
                   (funcall cb (car file))
                   (incf ret)))
                (t))))
      (setq files (cdr files)))
    ret))

(defun misc-byte-compile-all (directory &optional norecurse)
  (interactive "D")
  (let ((compiled 0)
        (considered 0))
    (misc-find-files directory
                     "\\.el$"
                     norecurse
                     (lambda (file)
                       (incf considered)
                       (unless (or (misc-is-compiled file) (not (file-writable-p file)))
                         (incf compiled)
                         (condition-case nil
                             (byte-compile-file file t)
                           (error))))
                     (lambda (file)
                       (or (string-match "/tests?[/$]" file)
                           (string-match "\\<demo\\>" file)
                           (string-match "\\<examples?\\>" file)
                           (string-match "\\.cask[/$]" file))))
    (cons compiled considered)))

(defun misc-byte-compile-all-loadpath ()
  (interactive)
  (let ((paths load-path)
        (considered 0)
        (compiled 0))
    (while paths
      (let ((result (misc-byte-compile-all (car paths))))
        (incf compiled (car result))
        (incf considered (cdr result)))
      (setq paths (cdr paths)))
    (message "Compiled %d/%d files" compiled considered)))

(defun misc-compare-files-by-modification-time (l r)
  (time-less-p (nth 6 l) (nth 6 r)))

(defun misc-directory-files-helper (dir match dirsonly sortbymodicationtime)
  (let ((all (directory-files-and-attributes dir nil match t))
        (ret))
    (when sortbymodicationtime
      (setq all (sort all 'misc-compare-files-by-modification-time)))
    (while all
      (let ((name (caar all)))
        (unless (or (string= name ".")
                    (string= name "..")
                    (and dirsonly (not (nth 1 (car all)))))
          (setq ret (cons name ret)))
        (setq all (cdr all))))
    ret))

(defvar mktest-cmake-args nil)
(defvar mktest-directory (getenv "TEST_DIRECTORY"))
(defun mktest (&optional name)
  (interactive)
  (unless mktest-directory
    (error "You have to set mktest-directory to something."))
  (unless name
    (setq name (read-from-minibuffer "Test name: ")))
  (let* ((dir (expand-file-name (concat mktest-directory "/" name "/")))
         (main.cpp (concat dir "main.cpp"))
         (cmake-arguments (list dir))
         (CMakeLists.txt (concat dir "CMakeLists.txt")))
    (if (file-exists-p main.cpp)
        (progn
          (message "Test already exists")
          (find-file main.cpp))
      (mkdir dir t)
      (with-temp-buffer
        (insert "cmake_minimum_required(VERSION 2.8)\n"
                "project(" name ")\n"
                "include_directories(${CMAKE_CURRENT_LIST_DIR})\n"
                "set(CMAKE_CXX_FLAGS \"${CMAKE_CXX_FLAGS} -std=c++17\")\n"
                "set(CMAKE_C_FLAGS \"${CMAKE_C_FLAGS}\")\n"
                "add_executable(" name " main.cpp)\n")
        (write-region (point-min) (point-max) CMakeLists.txt))
      (find-file main.cpp)
      (insert "int main(int argc, char **argv)\n"
              "{\n"
              "    \n"
              "}\n")
      (basic-save-buffer)
      (goto-char 35)
      (apply #'start-process
             "*mktest-cmake*"
             nil
             "cmake"
             (cond ((null mktest-cmake-args) cmake-arguments)
                   ((listp mktest-cmake-args) (append cmake-arguments mktest-cmake-args))
                   (t cmake-arguments))))))

(defun cdtest (&optional test)
  (interactive)
  (unless mktest-directory
    (error "You have to set mktest-directory to something."))
  (unless test
    (let ((dirs (misc-directory-files-helper mktest-directory nil t t)))
      (setq test (or (ido-completing-read (format "Test (default %s): " (car dirs)) dirs) (car dirs)))))
  (let* ((dir (expand-file-name (concat mktest-directory "/" test "/")))
         (main.cpp (concat dir "main.cpp")))
    (cond ((file-exists-p main.cpp) (find-file main.cpp))
          ((file-exists-p dir) (find-file dir))
          (t (message "No directory")))))

(defun getenvempty (variable)
  (or (getenv variable) ""))

(defvar mkgibbontest-directory (let* ((prefix (getenvempty "NF_HTTPD_PREFIX"))
                                      (index (string-match ":" prefix)))
                                 (and index (substring prefix 0 index))))

(defvar mkgibbontest-webprefix (let* ((prefix (getenvempty "NF_HTTPD_PREFIX"))
                                      (index (string-match ":" prefix)))
                                 (and index (substring prefix (1+ index)))))

(defvar mkgibbontest-template
  (concat "/*global nrdp*/\n"
          "function keyboardHandler(key)\n"
          "{\n"
          "    if (key.data.type == \"press\" && key.data.text == 'a') {\n"
          "        nrdp.gibbon.load({ url:\"http://en.wikipedia.org/wiki/Leif_Erikson\",\n"
          "                           headers: {\"X-Gibbon-Cache-Control\":\"key=foo,refresh,no-memory-cache\"}\n"
          "                         }, networkResponseHandler);\n"
          "    }\n"
          "    // nrdp.log.console(\"Got Key: \" + JSON.stringify(key));\n"
          "}\n"
          "function networkResponseHandler(event) {\n"
          "    nrdp.log.console(\"Got response \" + event.state + \" data: \" + event.size);\n"
          "}\n"
          "function onImageLoaded(event) {\n"
          "    nrdp.log.console(\"Got image loaded \" + JSON.stringify(event));\n"
          "}\n"
          "function onTimeout() {\n"
          "    nrdp.gibbon.load({ url:\"http://en.wikipedia.org/wiki/Leif_Erikson\",\n"
          "                       headers: {\"X-Gibbon-Cache-Control\":\"key=foo,refresh,no-memory-cache\"}\n"
          "                     }, networkResponseHandler);\n"
          "}\n"
          "\n"
          "var w;\n"
          "function main() {\n"
          "    nrdp.setServerTime(parseInt(new Date().valueOf() / 1000));\n"
          "    nrdp.gibbon.addEventListener(\"key\", keyboardHandler);\n"
          "    // nrdp.gibbon.load({url:\"http://en.wikipedia.org/wiki/Leif_Erikson\", headers: {\"X-Gibbon-Cache-Control\":\"max-age:1000\"}}, networkResponseHandler);\n"
          "    nrdp.gibbon.scene.widget = w = nrdp.gibbon.makeWidget({width:1280, height:720, color:\"#00ff00\"});\n"
          "    // w.image.url = {url:\"https://assets.nflxext.com/us/ffe/htmltvui/photon/mdx/background-streams.jpg\"};\n"
          "    // nrdp.gibbon.setTimeout(onTimeout, 0);\n"
          "}\n"
          "nrdp.gibbon.init(main);\n"))

(defun mkgibbontest-copy (name)
  (kill-new (concat mkgibbontest-webprefix "/" name))
  (message "Killed %s" (concat mkgibbontest-webprefix "/" name)))

(defun mkgibbontest-copy-current ()
  (interactive)
  (if (misc-string-prefix-p mkgibbontest-directory (buffer-file-name))
      (mkgibbontest-copy (substring (buffer-file-name) (length mkgibbontest-directory)))
    (kill-new (buffer-file-name))
    (message "Killed: %s" (buffer-file-name))))

(defalias 'cdgibbontest-copy-current 'mkgibbontest-copy-current)

(defun mkgibbontest (&optional name)
  (interactive)
  (unless mkgibbontest-directory
    (error "You have to set mkgibbontest-directory to something."))
  (unless name
    (setq name (read-from-minibuffer "Gibbon test name: ")))
  (let ((file (expand-file-name (concat mkgibbontest-directory "/gibbontest-" name ".js"))))
    (if (file-exists-p file)
        (progn
          (message "Test already exists")
          (find-file file))
      (find-file file)
      (mkgibbontest-copy (file-name-nondirectory file))
      (insert mkgibbontest-template)
      (basic-save-buffer))))

(defun cdgibbontest (&optional test)
  (interactive)
  (unless mkgibbontest-directory
    (error "You have to set mkgibbontest-directory to something."))
  (let* ((tests (misc-directory-files-helper mkgibbontest-directory "\.js$" nil t))
         (test (and tests (ido-completing-read (format "Gibbon test (default %s): " (car tests)) tests)))
         (abspath (concat mkgibbontest-directory "/" (or test (car tests)))))
    (when (file-exists-p abspath)
      (mkgibbontest-copy (file-name-nondirectory abspath))
      (find-file abspath))))

(defun include-file (&optional file)
  (interactive)
  (unless (> (length file) 0)
    (setq file (read-from-minibuffer "Include file: ")))
  (when (> (length file) 0)
    (unless (string-match "^[<\"]" file)
      (setq file (concat "<" file ">")))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (concat "^# *include *" (regexp-quote file)) nil t)
          (message "%s is already included" file)
        (goto-char (point-max))
        (let ((head "\n")
              (tail ""))
          (if (re-search-backward "^# *include\\>" nil t)
              (end-of-line)
            (setq head "")
            (setq tail "\n")
            (goto-char (point-min)))
          (insert head "#include " file tail))
        (message "Added #include %s" file)))))

;;===================
;; Prelude stuff
;;===================

(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))


(defun prelude-duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (column (- (point) (point-at-bol)))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ (point-at-bol) column))))

(defun shit (&optional args)
  (interactive)
  (when (executable-find "shit")
    (with-temp-buffer
      (apply #'call-process "shit" nil t t (cond ((null args) (split-string-and-unquote (read-from-minibuffer "Shit: ")))
                                                 ((stringp args) (split-string-and-unquote args))
                                                 ((listp args) args)
                                                 (t nil)))
      (magit-refresh-status-buffer)
      (message (buffer-string)))))

(defun nslookup (&optional ip)
  (interactive)
  (when (and (not ip) (region-active-p))
    (let ((sel (buffer-substring-no-properties (region-beginning) (region-end))))
      (if (string-match "[0-9][0-9]?[0-9]?\.[0-9][0-9]?[0-9]?\.[0-9][0-9]?[0-9]?\.[0-9][0-9]?[0-9]?" sel)
          (setq ip sel))))

  (message "%s" (shell-command-to-string (concat "nslookup " (or ip (read-from-minibuffer "nslookup: "))))))

(defun all-executables (&optional paths)
  (let ((all))
    (mapc (function (lambda (path)
                      ;; (message path)
                      (when (file-directory-p path)
                        (mapc
                         (function (lambda (file)
                                     (when (and (not (file-directory-p file))
                                                (file-executable-p file))
                                       (push file all))))
                         (directory-files path t nil t)))))
          (or paths exec-path))
    all))

(defun which-open()
  (interactive)
  (let ((file (completing-read "File: " (all-executables))))
    (if (and file (file-exists-p file))
        (find-file file)
      (if (and file (length file))
          (message "Can't find %s" file)))))

(defvar insert-c++-cast-alternatives (list "static_cast" "reinterpret_cast" "dynamic_cast" "static_pointer_cast" "std::static_pointer_cast"))
(defun insert-c++-cast (&optional cast type)
  (interactive)
  (unless cast
    (setq cast (completing-read "Cast type: " insert-c++-cast-alternatives  nil nil nil nil (car insert-c++-cast-alternatives)))
    (unless cast
      (error "Nothing to cast")))
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        pos)
    (if (not bounds)
        (progn
          (insert cast "<")
          (setq pos (point))
          (insert ">()"))
      (goto-char (cdr bounds))
      (insert ")")
      (goto-char (car bounds))
      (insert cast "<")
      (setq pos (point))
      (insert ">("))
    (goto-char pos)
    (when type
      (insert type))))

(defun insert-static-cast (&optional choose)
  (interactive "P")
  (insert-c++-cast (unless choose "static_cast")))

(defun misc-compilation-finish-function (buf str)
  (when misc-store-compiles
    (misc-store-last-compile))
  (with-current-buffer buf
    (let ((old buffer-read-only))
      (setq buffer-read-only nil)
      (ansi-color-filter-region (point-min) (point-max))
      (setq buffer-read-only old)))
  (cond ((string-match "exited abnormally" str)
         (message "compilation errors, press C-x ` to visit"))
        ((with-current-buffer buf
           (save-excursion
             (goto-char (point-min))
             (re-search-forward ":[0-9]+: warning:" nil t)))
         (message "compilation warnings, press C-x ` to visit"))
        ((string-match "*compilation*" (buffer-name buf))
         (if (> (length (window-list)) 1)
             (let ((windows (get-buffer-window-list buf)))
               (while windows
                 (delete-window (car windows))
                 (setq windows (cdr windows))))
           (switch-to-buffer buf)
           (bury-buffer))
         (message "Compilation successful!"))
        (t nil)))

(defun misc-cat-emacs ()
  (interactive)
  (with-current-buffer (misc-current-buffer)
    (save-restriction
      (widen)
      (let ((output (buffer-substring-no-properties (point-min) (point-max))))
        (find-file-literally "/tmp/misc-cat-emacs.tmp")
        (insert output)
        (basic-save-buffer)
        (kill-buffer (current-buffer))))))

(defun misc-find-first-non-ascii-char () ;; binary
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ascii characters."))))

(defun misc-isearch-occur ()
  "*Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "M-W") (function isearch-toggle-word))
(define-key isearch-mode-map (kbd "M-R") (function isearch-toggle-regexp))
(define-key isearch-mode-map (kbd "C-o") (function isearch-occur))

(defun misc-join-line (&optional arg)
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line)
             (if arg (delete-horizontal-space)))))
        (t (join-line 1)))
  (if (or (looking-at "}") (save-excursion (forward-char -1) (looking-at "{")))
      (insert " ")))

(defvar misc-compiles-dir "~/.emacs.d/compiles")
(defvar misc-store-compiles nil) ;; Set to integer to limit number of files kept
(defun misc-store-last-compile (&optional proc)
  (mkdir misc-compiles-dir t)
  (let* ((max 0)
         (count 0))
    (misc-find-files misc-compiles-dir "/compile_[0-9]+\\.txt$" t (lambda (file)
                                                                    (when (string-match "/compile_\\([0-9]+\\)\\.txt$" file)
                                                                      (incf count)
                                                                      (setq max (max max (string-to-number (match-string 1 file)))))))
    (when (and (numberp misc-store-compiles)
               (>= count misc-store-compiles))
      (let* ((compiles (nreverse (misc-directory-files-helper misc-compiles-dir "compile_[0-9]+.txt" nil t))))
        (while (>= count misc-store-compiles)
          (delete-file (concat misc-compiles-dir "/" (car compiles)))
          (decf count)
          (setq compiles (cdr compiles)))))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max)
                    (format "%s/compile_%06d.txt" misc-compiles-dir (1+ max))))))

(defun misc-grep-compiles ()
  (interactive)
  (grep-find
   (read-shell-command
    "Run find (like this): "
    (concat "find " misc-compiles-dir " -type f -print0 | xargs -0 -P 30 grep -n ")
    'grep-find-history)))

(defvar xclip-use-primary nil)
(defvar xclip-process nil)
(defvar xclip-location nil)
(defun xclip-sentinel (process event)
  (when (eq (process-status process) 'exit)
    (when (and (buffer-live-p (car xclip-location))
               (let ((visible))
                 (mapc (lambda (window)
                         (when (eq (window-buffer window) (car xclip-location))
                           (setq visible t)))
                       (window-list)))
               (with-current-buffer (car xclip-location)
                 (= (point) (cdr xclip-location))))
      (let ((output (with-current-buffer (process-buffer process)
                      (buffer-substring-no-properties (point-min) (point-max)))))
        (when (> (length output) 0)
          (switch-to-buffer (car xclip-location))
          (insert output)))))
  (unless (eq (process-status process) 'run)
    (kill-buffer (process-buffer xclip-process))
    (setq xclip-process nil
          xclip-location nil)))

(defun xclip-yank ()
  (interactive)
  (unless (getenv "DISPLAY")
    (error "No X11 connection"))
  (unless (executable-find "xclip")
    (error "No xclip"))
  (when buffer-read-only
    (error "Buffer is read only"))
  (when (and xclip-process
             (not (eq (process-status xclip-process) 'exit))
             (not (eq (process-status xclip-process) 'signal)))
    (error "Already pasting..."))

  (let ((buf (get-buffer-create " *XClip*")))
    (with-current-buffer buf
      (buffer-disable-undo buf)
      (erase-buffer))
    (setq xclip-process (start-process "XClip" buf "xclip" "-out" "-selection" (if xclip-use-primary "primary" "clipboard")))
    (setq xclip-location (cons (current-buffer) (point)))
    (set-process-sentinel xclip-process 'xclip-sentinel)))

;; man-page stuff
(defvar misc-man-preference (list "-a" "3" "3posix" "3.*" "2" "2posix" "2.*"))
(defun misc-man-idx (type)
  (let ((idx 0)
        (pref misc-man-preference))
    (while pref
      (if (string-match (concat "^" (car pref) "$") type)
          (setq pref nil)
        (setq pref (cdr pref))
        (incf idx)))
    idx))

(defvar misc-man-completion-cache nil)
(defun misc-man-completion-cache (&optional data)
  (let ((results)
        (last)
        (count 0))
    (mapc (lambda (line)
            (let ((dash (string-match " *-" line)))
              (when dash
                (mapc (lambda (entry)
                        (when (string-match "^\\([A-Za-z0-9_][^ (]*\\) *(\\([0-9]+\\)[^)]*)" entry)
                          (let* ((topic (match-string 1 entry))
                                 (section (match-string 2 entry))
                                 (formatted (concat topic "(" section ")")))
                            (if (string= topic last)
                                (incf count)
                              (setq last topic)
                              (setq count 1))
                            (push formatted results)
                            (when (= count 2)
                              (push (concat topic "(-a)") results)))))
                      (split-string (substring line 0 dash) ", *")))))
          (split-string (or data (shell-command-to-string "apropos .")) "\n" t))
    (sort results (lambda (a b)
                    (string-match "^\\([^(]+\\)(\\([^)]+\\))$" a)
                    (let ((atopic (match-string 1 a))
                          (asection (match-string 2 a)))
                      (string-match "^\\([^(]+\\)(\\([^)]+\\))$" b)
                      (let ((btopic (match-string 1 b))
                            (bsection (match-string 2 b)))
                        (cond ((string< atopic btopic) t)
                              ((string< btopic atopic) nil)
                              (t (< (misc-man-idx asection)
                                    (misc-man-idx bsection))))))))))

(defun misc-man (&optional word)
  (interactive "P")
  (let ((case-fold-search nil))
    (when (cond ((not misc-man-completion-cache))
                ((and word (listp word)) (setq word nil) t)
                (t (> (time-to-seconds (time-since (cdr misc-man-completion-cache))) (* 24 60 60))))
      (setq misc-man-completion-cache (cons (misc-man-completion-cache) (current-time))))

    (let* ((sym (thing-at-point 'symbol)) ;; completion
           (prompt (format "Man page%s: "
                           (or (and sym (format " (default \"%s\")" sym)) "")))
           (page (misc-trim-string (or word (completing-read-default prompt (car misc-man-completion-cache) nil nil nil nil sym)))))
      (unless (> (length page) 0)
        (setq page sym))
      (unless page
        (error "No page?"))
      (when (string-match "^[^(]*([0-9]+[^)]*$" page)
        (setq page (concat page ")")))
      (when (string-match "^\\(.*\\)($" page)
        (setq page (match-string 1 page)))
      (when (string-match "()$" page)
        (setq page (substring page 0 -2)))
      (unless (string-match "(" page)
        (let ((all (car misc-man-completion-cache))
              (rx (concat "^" page "(")))
          (while all
            (if (string-match rx (car all))
                (setq page (car all) all nil)
              (setq all (cdr all))))))
      (when (and (string-match "\\([^ ]*\\)(\\([^ ]*\\))" page)
                 (string= (match-string 2 page) "-a"))
        (setq page (concat "-a " (match-string 1 page))))
      (man page))))


(defun misc-liberal-file-exists (arg)
  (and arg (> (length arg) 0) (file-exists-p arg) arg))

(defun misc-find-outer-file-directory (name)
  (let ((dir default-directory)
        (best))
    (unless (string-match "/$" dir)
      (setq dir (concat dir "/")))
    (while (not (string= dir "/"))
      (when (file-exists-p (concat dir name))
        (setq best dir))
      (if (string-match "\\(^.*/\\)[^/]*/$" dir)
          (setq dir (match-string 1 dir))
        (setq dir "/")))
    best))

(defun misc-project-root()
  (s-chop-suffix "/" (cond ((misc-liberal-file-exists (lsdev-root-dir default-directory)))
                           ((misc-liberal-file-exists (nrdp-git-deepest-root)))
                           ((misc-find-outer-file-directory "configure"))
                           ((misc-find-outer-file-directory "Makefile"))
                           (t default-directory))))

(defun misc-save-current-macro-to-dot-emacs (name)
  "Save the current macro as named function definition inside your initialization file so you can reuse it anytime in the future."
  (interactive "SSave Macro as: ")
  (name-last-kbd-macro name)
  (save-excursion
    (find-file-literally user-init-file)
    (goto-char (point-max))
    (insert "\n\n;; Saved macro\n")
    (insert-kbd-macro name)
    (insert "\n")))

(defvar misc-grep-find-not
  " -not -name error.js -not -name xboxupsellpage.js -not -name '*.min.js*' -not -name 'string-unpack-code.js' -not -name ttrlibs.js -not -path '*/sunspider/*' -not -path '*/newt-docs/*'")
(defvar misc-grep-find-source-files
  "\\( -name \"*.cpp\" -o -name \"*.h\" -o -name \"*.c\" -o -name \"*.ts\" -o -name \"*.ts.d\" -o -name \"*.js\" -o -name \".mm\" -o -name \"*.m\" -o -name \"*.inc\" -o -name \"*.cc\" -o -iname \"*.hpp\" -o -name \"*.py\" -o -name \"*.el\" -o -name \"*.java\" -o -name \"*.html\" -o -name \"Makefile*\" \\)")
(defvar misc-grep-find-cmake
  "\\( -name \"CMakeLists.txt\" -o -name \"*.cmake\" \\)")


(defvar shell-quote-argument-in-ag nil)
(defadvice shell-quote-argument (around argument activate)
  (if shell-quote-argument-in-ag
      (setq ad-return-value
            (cond ((string= argument "") "''")
                  ((string-match "^\".*\"$" argument) argument)
                  (t (concat "'" (replace-regexp-in-string "'" "\\\\'" argument) "'"))))
    ad-do-it))

;; Use -w phrase to search for whole words
(defvar misc-grep-find-prefer-ag nil)
(defun misc-grep-find-helper (dir filterType) ;; filterType integerp: all files, filterType t: cmake, otherwise: sources
  (when (and (eq major-mode 'cmake-mode)
             (or (not (integerp filterType))))
    (setq filterType (not filterType)))
  (if (and misc-grep-find-prefer-ag
           (executable-find "ag")
           (fboundp 'ag))
      (let ((old-ag-arguments (copy-sequence ag-arguments)))
        (let* ((mode (cond ((integerp filterType) (setq current-prefix-arg nil) "*")
                           (filterType (setq current-prefix-arg nil) "cmake")
                           (t "sources")))
               (shell-quote-argument-in-ag t)
               (search-result
                (let ((result (read-from-minibuffer "Ag " (concat mode ": \"" dir (unless (string-match "/$" dir) "/") "\" "))))
                  (unless (string-match "^ *\\([^:]*\\): *\"\\([^\"]+\\)\" +\\(.*\\) *$" result)
                    (error "Invalid ag!"))
                  result))
               (default-directory (setq dir (match-string 2 search-result)))
               (search (match-string 3 search-result))
               (args (list "-w" "--word-regexp"
                           "-o" "--only-matching"
                           "-i" "--ignore-case"
                           "-c" "--count"
                           "-v" "--invert-match"
                           "-S" "--smart-case"
                           "-u" "--unrestricted"
                           "-a" "--all-types"
                           "-z" "--search-zip"
                           "-F" "--fixed-strings"
                           "-t" "--all-text"
                           "-U" "--skip-vcs-ignores"
                           "-f" "--follow"
                           "-l" "--files-with-matches"
                           "-L" "--files-without-matches"
                           "--hidden"
                           "--search-binary"))
               (appendargs (list "-s" "--case-sensitive"
                                 "-S" "--smart-case"
                                 "-i" "--ignore-case"))
               (argargs (list "-C" "--context"
                              "-A" "--after"
                              "-B" "--before"
                              "-G" "--file-search-regex"
                              "--ignore"
                              "--ignore-dir"
                              "--depth"))
               (split (split-string search)))
          (let ((m (or (match-string 1 search-result) mode)))
            (cond ((string= m "sources")
                   (setq ag-arguments (append
                                       (list "--shell" "--cc" "--cpp" "--js" "--objc" "--objcpp" "--java" "--python" "--elisp" "--xml" "--json" "--perl" "-G" ".*\.inc$")
                                       ag-arguments)))
                  ((string= m "cmake")
                   (setq ag-arguments (append
                                       (list "-G" "[Cc]make")
                                       ag-arguments)))
                  (t)))
          (while split
            (cond ((string= (car split) "--")
                   (when (string-match "-- *" search)
                     (setq search (replace-match "" t t search)))
                   (setq split nil))
                  ((member (car split) appendargs)
                   (nbutlast ag-arguments) ;; remove --
                   (setq ag-arguments (append ag-arguments (list (car split) "--")))
                   (when (string-match (concat (unless (cdr split) " *") (car split) "\\> *") search)
                     (setq search (replace-match "" t t search))))
                  ((member (car split) args)
                   (push (car split) ag-arguments)
                   (when (string-match (concat (unless (cdr split) " *") (car split) "\\> *") search)
                     (setq search (replace-match "" t t search))))
                  ((member (car split) argargs)
                   (push (cadr split) ag-arguments)
                   (push (car split) ag-arguments)
                   (when (string-match (concat (unless (cdr split) " *") (car split) "\\> *[^ ]* *") search)
                     (setq search (replace-match "" t t search))))
                  (t))
            (setq split (cdr split)))
          (ag-regexp search dir))
        (setq ag-arguments old-ag-arguments))
    (grep-find
     (read-shell-command
      "Run find (like this): "
      (concat "find " dir (concat " -type f " misc-grep-find-not " ")
              (cond ((integerp filterType) "")
                    (filterType misc-grep-find-cmake)
                    (t misc-grep-find-source-files))
              " -print0 | xargs -0 -P 30 grep -n ")
      'grep-find-history))))

(require 'minimal-session-saver nil t)
(defvar misc-loaded-session nil)
(defvar misc-mss-store-timer nil)
(defun misc-mss-store ()
  (setq misc-mss-store-timer nil)
  (and (fboundp 'minimal-session-saver-store) (minimal-session-saver-store)))

(defun misc-mss-schedule-store ()
  (when misc-mss-store-timer
    (cancel-timer misc-mss-store-timer))
  (setq misc-mss-store-timer (run-with-idle-timer 5 nil (function misc-mss-store))))

(defun misc-mss-kill-buffer-hook ()
  (when (buffer-file-name)
    (unless (file-directory-p default-directory)
      (cd "/"))
    (misc-mss-schedule-store))
  t)

(defun misc-mss-enable () ;;minimal-session-saver
  (unless misc-loaded-session
    (setq misc-loaded-session t)
    (condition-case nil
        (and (fboundp 'minimal-session-saver-load) (minimal-session-saver-load))
      (error)))
  (add-hook 'find-file-hook (function misc-mss-schedule-store))
  (add-hook 'kill-buffer-hook (function misc-mss-kill-buffer-hook))
  (add-hook 'kill-emacs-hook (function minimal-session-saver-store)))

(defun misc-grep-find-project (&optional filterType)
  (interactive "P")
  (setq current-prefix-arg nil)
  (misc-grep-find-helper (misc-project-root) filterType))

(defun misc-grep-find (&optional filterType)
  (interactive "P")
  (setq current-prefix-arg nil)
  (misc-grep-find-helper default-directory filterType))

(require 'asm-mode)
(add-hook 'asm-mode-hook (lambda () (local-set-key (vector asm-comment-char) 'self-insert-command)))

(defun unslack()
  (interactive)
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (--misc-replace-string-helper "“" "\"" (min start end) (max start end))
    (--misc-replace-string-helper "”" "\"" (min start end) (max start end))))

(defvar misc-other-window-transient-map (make-sparse-keymap))
(define-key misc-other-window-transient-map (kbd "o") (function misc-other-window))
(define-key misc-other-window-transient-map (kbd "O") (function misc-other-window-reverse))

(defun misc-other-window (count &optional all-frames)
  (interactive "p")
  (other-window count all-frames)
  (set-transient-map misc-other-window-transient-map))

(defun misc-other-window-reverse (count &optional all-frames)
  (interactive "p")
  (misc-other-window (or count -1) all-frames))

(defvar misc-next-error-map nil)
(setq misc-next-error-map (make-sparse-keymap))
(define-key misc-next-error-map (kbd "`") (function misc-next-error))
(define-key misc-next-error-map (kbd "~") (function misc-previous-error))

(defun misc-next-error (&optional arg reset)
  (interactive "P")
  (next-error arg reset)
  (set-transient-map misc-next-error-map))

(defun misc-previous-error (&optional n)
  (interactive "p")
  (previous-error n)
  (set-transient-map misc-next-error-map))

(defun misc-killall-buffers (&optional pattern)
  (interactive)
  (unless pattern
    (setq pattern (read-from-minibuffer "Kill buffers matching: ")))
  (let ((buffers (buffer-list))
        (count 0))
    (while buffers
      ;; (message "Checking %s against %s => %s"
      ;;          (or (buffer-file-name (car buffers))
      ;;              (buffer-name (car buffers)))
      ;;          pattern
      ;;          (if (string-match pattern (or (buffer-file-name (car buffers))
      ;;                                        (buffer-name (car buffers))))
      ;;              "yes" "no"))

      (when (string-match pattern (or (buffer-file-name (car buffers))
                                      (buffer-name (car buffers))))
        (incf count)
        (kill-buffer (car buffers)))
      (setq buffers (cdr buffers)))
    (message "Killed %d buffers matching %s" count pattern)))

(defun lsdev-scan-build-file (&optional -b)
  (interactive "P")
  (let ((filename (file-name-nondirectory (buffer-file-name)))
        (dir
         (or (let ((file (find-ancestor-file "build.ninja" default-directory)))
               (and file (file-name-directory file)))
             (let* ((choices (lsdev-dirs-build default-directory (and -b (listp -b) "-b")))
                    (choice (cond ((null choices) nil)
                                  ((= 1 (length choices)) (cadar choices))
                                  (t (cadr (assoc (completing-read "Build: " choices) choices))))))
               choice))))
    (unless (and dir (file-exists-p (concat dir "/build.ninja")))
      (error "Can't find a build.ninja for %s" (buffer-file-name)))
    (switch-to-buffer (get-buffer-create "*lsdev-scan-build*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (cd dir)
    (insert "Running scan-build...\n")
    (goto-char (point-min))
    (setq buffer-read-only t)
    (compilation-mode)
    (let ((proc (start-process "SCAN_BUILD" (current-buffer) "scan-build.sh" filename)))
      (set-process-filter proc (lambda (process output)
                                 (with-current-buffer (process-buffer process)
                                   (save-excursion
                                     (let ((buffer-read-only nil))
                                       (goto-char (point-max))
                                       (let ((start (point)))
                                         (insert output)
                                         (ansi-color-filter-region start (point-max)))))))))))

(defun lsdev-clang-check-file (&optional -b)
  (interactive "P")
  (let ((filename (file-name-nondirectory (buffer-file-name)))
        (dir
         (or (let ((file (find-ancestor-file "compile_commands.json" default-directory)))
               (and file (file-name-directory file)))
             (let* ((choices (lsdev-dirs-build default-directory (and -b (listp -b) "-b")))
                    (choice (cond ((null choices) nil)
                                  ((= 1 (length choices)) (cadar choices))
                                  (t (cadr (assoc (completing-read "Build: " choices) choices))))))
               choice))))
    (unless (and dir (file-exists-p (concat dir "/compile_commands.json")))
      (error "Can't find a build.ninja for %s" (buffer-file-name)))
    (switch-to-buffer (get-buffer-create "*lsdev-clang-check*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (cd dir)
    (insert "Running clang-check...\n")
    (goto-char (point-min))
    (setq buffer-read-only t)
    (compilation-mode)
    (let ((proc (start-process "CLANG_CHECK" (current-buffer) "clang-check.sh" filename)))
      (set-process-filter proc (lambda (process output)
                                 (with-current-buffer (process-buffer process)
                                   (save-excursion
                                     (let ((buffer-read-only nil))
                                       (goto-char (point-max))
                                       (let ((start (point)))
                                         (insert output)
                                         (ansi-color-filter-region start (point-max)))))))))))


(defun nrdp-misc-cc-forward-sexp (&optional arg)
  (interactive "^p")
  (let ((conditional)
        (include)
        (endif))
    (save-excursion
      (goto-char (point-at-bol))
      (when (and (search-forward-regexp "^ *# *\\([a-z]*\\)" (point-at-eol) t)
                 (or (string= (match-string 1) "ifdef")
                     (string= (match-string 1) "ifndef")
                     (string= (match-string 1) "if")
                     (string= (match-string 1) "else")
                     (string= (match-string 1) "elif")
                     (and (string= (match-string 1) "include")
                          (message "and here")
                          (setq include t))
                     (and (string= (match-string 1) "endif")
                          (setq endif t))))
        (forward-char 1)
        (let ((done nil)
              (extra 0))
          (while (not conditional)
            (if (not (search-forward-regexp "^ *# *\\([a-z]*\\)" nil t))
                (error "Couldn't find corresponding preprocessing directive")
              (cond ((eq include t)
                     (when (string= (match-string 1) "include")
                       (setq conditional (point-at-bol))))
                    ((or (string= (match-string 1) "endif")
                         (string= (match-string 1) "elif")
                         (string= (match-string 1) "else"))
                     (if (= extra 0)
                         (setq conditional (point-at-bol))
                       (when (string= (match-string 1) "endif")
                         (decf extra))))
                    ((or (string= (match-string 1) "ifdef")
                         (string= (match-string 1) "ifndef")
                         (string= (match-string 1) "if"))
                     (if endif
                         (setq conditional (point-at-bol))
                       (incf extra)))
                    (t)))
            (forward-char 1)))))
    (if conditional
        (goto-char conditional)
      (forward-sexp arg))))

(defun nrdp-misc-cc-backward-sexp (&optional arg)
  (interactive "^p")
  (let ((conditional)
        (include)
        (ifif))
    (save-excursion
      (goto-char (point-at-bol))
      (when (and (search-forward-regexp "^ *# *\\([a-z]*\\)" (point-at-eol) t)
                 (or (and (string= (match-string 1) "ifdef") (setq ifif t))
                     (and (string= (match-string 1) "if") (setq ifif t))
                     (and (string= (match-string 1) "ifndef") (setq ifif t))
                     (and (string= (match-string 1) "include") (setq include t))
                     (string= (match-string 1) "else")
                     (string= (match-string 1) "elif")
                     (match-string 1) "endif"))
        (backward-char 1)
        (let ((done nil)
              (extra 0))
          (while (not conditional)
            (if (not (search-backward-regexp "^ *# *\\([a-z]*\\)" nil t))
                (error "Couldn't find corresponding preprocessing directive")
              (cond ((eq include t)
                     (when (string= (match-string 1) "include")
                       (setq conditional (point-at-bol))))
                    ((or (string= (match-string 1) "ifdef")
                         (string= (match-string 1) "if")
                         (string= (match-string 1) "ifndef")
                         (string= (match-string 1) "elif")
                         (string= (match-string 1) "else"))
                     (if (= extra 0)
                         (setq conditional (point-at-bol))
                       (when (or (string= (match-string 1) "ifdef")
                                 (string= (match-string 1) "ifndef")
                                 (string= (match-string 1) "if"))
                         (decf extra))))
                    ((string= (match-string 1) "endif")
                     (if ifif
                         (setq conditional (point-at-bol))
                       (incf extra)))
                    (t)))
            (backward-char 1)))))
    (if conditional
        (goto-char conditional)
      (backward-sexp arg))))

(define-key c-mode-base-map (kbd "C-M-f") (function nrdp-misc-cc-forward-sexp))
(define-key c-mode-base-map (kbd "C-M-b") (function nrdp-misc-cc-backward-sexp))

(provide 'nrdp-misc)
