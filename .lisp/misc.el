
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
  "Try to recursively go upwards from this directory and see if a file with
the name of the value of file-name is present."
  (let ((check-dir (cond (directory ;; extrapolate from name
              (if (equal (substring directory -1) "/")
                  directory
                (concat directory "/")))
                         (t default-directory) ;; hmm, use default
                         )))
    (while (not (or (<= (length check-dir) 0) (string-equal check-dir "/") (file-exists-p (concat check-dir file-name))))
      (setq check-dir (substring check-dir 0 (string-match "[^/]*/?$" check-dir))))
    ;; if we did found a file!
    (if (<= (length check-dir) 1) nil  (concat check-dir file-name))))

(defalias 'sam-find-ancestor-file 'find-ancestor-file)

(defun what-file (&optional name) ;;I use this function from emacsclient!
  (setq result nil)
  (let* ((buffers (buffer-list)))
    (while (and (not result) buffers)
      (let* ((buffer (car buffers)))
        (setq file-name (buffer-file-name buffer))
        (message (concat "Looking at " file-name))
        (if (and file-name (or (not name) (string-match name file-name)))
            (progn
              (setq result file-name)
              (save-excursion (set-buffer buffer) (save-restriction (widen) (setq result (format "%s:%d" result (line-number-at-pos)))))))
        (message (concat "Done Looking at " file-name))
        )
        (setq buffers (cdr buffers))))
  result
  )

(defalias 'sam-what-file 'what-file)

(defun what-directory (&optional name) ;;I use this function from emacsclient!
  (setq result nil)
  (let* ((buffers (buffer-list)))
    (while (and (not result) buffers)
      (let* ((buffer (car buffers)))
        (setq file-name (buffer-file-name buffer))
        (if (and file-name (or (not name) (string-match name file-name)))
            (setq result (file-name-directory file-name))))
        (setq buffers (cdr buffers))))
  result
  )

(defalias 'sam-what-directory 'what-directory)

(defun rotate-windows (&optional toggle-split)
  "Rotate your windows or split the toggle"
  (interactive "P")
  (if toggle-split
      (toggle-window-split)
    (cond ((not (> (count-windows) 1)) (message "You can't rotate a single window!"))
          (t
           (setq i 1)
           (setq numWindows (count-windows))
           (while  (< i numWindows)
             (let* (
                    (w1 (elt (window-list) i))
                    (w2 (elt (window-list) (+ (% i numWindows) 1)))
                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))
                    (s1 (window-start w1))
                    (s2 (window-start w2))
                    )
               (set-window-buffer w1  b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i (1+ i))))))))

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
  (end-of-buffer)
  (insert-string (format-time-string "\n\n============== MARK: %Y-%m-%d %H:%M:%S %Z =====================\n\n"))
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
      (process-kill-without-query process)
      (pop-to-buffer buffer) process)))

;;====================
;; Carriage return bogusness
;;====================
(defun dos-to-unix ()
  "Replace \r\n with \n"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ( replace-string "\r\n" "\n" )))

(defun unix-to-dos ()
  "Replace \n with \r\n"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ( replace-string "\n" "\r\n" )))

(defun ediff-cleanup-buffer-b-handler ()
  (if ediff-buffer-B
      (let ((b-window (get-buffer-window ediff-buffer-B)))
        (if b-window (delete-window b-window))
        (kill-buffer ediff-buffer-B))))
(provide 'nrdp-misc)

;;===================
;; Insert prints all over the board
;;===================
(require 'cc-mode)
(defun litter-printf (&optional begin end)
  (interactive)
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
      (insert "\nprintf(\"%s:%d [%s]\\n\", __FILE__, __LINE__, __FUNCTION__);")
      (setq points (cdr points))))
  (c-indent-defun)
  )

;;skeleton thingie
(defun make-member ()
  "make a skeleton member function in the .cpp file"
  (interactive)
  (let ((class nil)
        (function nil)
        (file (buffer-file-name))
        (insertion-string nil)
        (start nil))
    (save-excursion
      (and (re-search-backward "^[ \t]*class[ \t\n]" nil t)
           (progn
             (forward-word 1)
             (while (looking-at "[ \t]*DLLEXPORT")
               (forward-word 1))
             (while (looking-at "[ \t]*Q_[a-zA-Z0-9]*_EXPORT")
               (forward-word 3))
             (while (looking-at "[ \t\n]")
               (forward-char 1))
             (setq start (point))
             (while (looking-at "[A-Za-z0-9_]")
               (forward-char 1))
             (setq class (buffer-substring start (point))))))
    (progn
      (and (looking-at "$")
           (progn
             (search-backward ")" nil t)
             (forward-char)
             (backward-sexp)))
      (and (stringp class)
           (re-search-backward "^[ \t]")
           (progn
             (while (looking-at "[ \t]")
               (forward-char 1))
             (setq start (point))
             (and (search-forward "(" nil t)
                  (progn
                    (forward-char -1)
                    (forward-sexp)))
             (and (looking-at "[ \t]+const")
                  (forward-word 1))
             (and (looking-at ";")
                  (setq function (buffer-substring start (point))))
             (re-search-forward "(" nil t))))
    (and (stringp function)
         (progn
           (and (string-match "[ \t]*\\<virtual\\>[ \t]*" function)
                (setq function (replace-match " " t t function)))
           (and (string-match "^\\(virtual\\>\\)?[ \t]*" function)
                (setq function (replace-match "" t t function)))
           (while (string-match "  +" function)
             (setq function (replace-match " " t t function)))
           (while (string-match "\t+" function)
             (setq function (replace-match " " t t function)))
           (while (string-match " ?=[^,)]+" function)
             (setq function (replace-match " " t t function)))
           (while (string-match " +," function)
             (setq function (replace-match "," t t function)))))
    (and (stringp function)
         (stringp class)
         (stringp file)
         (progn
           (cond ((string-match (concat "^ *" class "[ \\t]*(") function)
                  (progn
                    (setq insertion-string
                          (concat
                           "\n"
                           (replace-match
                            (concat class "::" class "(")
                            t t function)
                           "\n{\n    \n}\n"))))
                 ((string-match (concat "^ *~" class "[ \\t]*(") function)
                  (progn
                    (setq insertion-string
                          (concat
                           "\n"
                           (replace-match
                            (concat class "::~" class "(")
                            t t function)
                           "\n{\n    \n}\n"))))
                 ((string-match " *\\([a-z0-9_]+\\)[ \\t]*(" function)
                  (progn
                    (setq insertion-string
                          (concat
                           "\n"
                           (replace-match
                            (concat " " class "::" "\\1(")
                            t nil function)
                           "\n{\n    \n}\n"))))
                 (t
                  (error (concat "Can't parse declaration ``"
                                 function "'' in class ``" class
                                 "'', aborting"))))
           (stringp insertion-string))
         (condition-case nil (switch-cpp-h)
           (error (progn
                    (string-match "\\.h$" file)
                    (find-file (replace-match ".cpp" t t file)))))
         (progn
           (goto-char (point-max))
           (insert insertion-string)
           (forward-char -3)
           (save-excursion
             (and (string-match ".*/" file)
                  (setq file (replace-match "" t nil file)))
             (or (re-search-backward
                  (concat "^#include *\"" file "\"$") nil t)
                 (progn
                   (goto-char (point-min))
                   (re-search-forward "^$" nil t)
                   (insert "\n#include \"" file "\"\n"))))))))
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


(defun find-corresponding-cpp-h ()
  (let ((n (buffer-file-name)) (attempts) (all-attempts)(found nil))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "\\(//\\|/\\*\\) *SWITCH_FILE: *\"" nil t)
          (progn
            (setq start (point))
            (if (search-forward "\"") (progn (backward-char)
                                             (push (buffer-substring start (point)) all-attempts))))))
    (if (string-match "\\.ui.h$" n) (push (replace-match ".ui" t t n ) all-attempts))
    (if (string-match "\\.ui$" n) (push (replace-match ".ui.h" t t n ) all-attempts))
    (if (string-match "_p\\.h$" n) (push (replace-match ".cpp" t t n ) all-attempts))
    (if (string-match "\\.h$" n) (push (replace-match ".cc" t t n ) all-attempts))
    (if (string-match "\\.h$" n) (push (replace-match ".C" t t n ) all-attempts))
    (if (string-match "\\.h$" n) (push (replace-match ".mm" t t n ) all-attempts))
    (if (string-match "\\.h$" n) (push (replace-match ".m" t t n ) all-attempts))
    (if (string-match "\\.h$" n) (push (replace-match ".c" t t n ) all-attempts))
    (if (string-match "\\.h$" n) (push (replace-match ".cpp" t t n ) all-attempts))
    (if (string-match "_p\\.h$" n) (push (replace-match "_p.cpp" t t n ) all-attempts))
    (if (string-match "\\.h$" n) (push (replace-match "_p.cpp" t t n ) all-attempts))
    (if (string-match "\\.hpp$" n) (push (replace-match ".cpp" t t n ) all-attempts))
    (if (string-match "\\.hxx$" n) (push (replace-match ".cxx" t t n ) all-attempts))

    (if (string-match "_x11\\.cpp$" n) (push (replace-match ".h" t t n ) all-attempts))
    (if (string-match "_unix\\.cpp$" n) (push (replace-match ".h" t t n ) all-attempts))
    (if (string-match "_mac\\.cpp$" n) (push (replace-match ".h" t t n ) all-attempts))
    (if (string-match "_thn\\.cpp$" n) (push (replace-match ".h" t t n ) all-attempts))
    (if (string-match "_win\\.cpp$" n) (push (replace-match ".h" t t n ) all-attempts))
    (if (string-match "_qws\\.cpp$" n) (push (replace-match ".h" t t n ) all-attempts))
    (if (string-match ".cpp$" n) (push (replace-match "_p.h" t t n ) all-attempts))
    (if (string-match ".cpp$" n) (push (replace-match ".h" t t n ) all-attempts))
    (if (string-match ".cc$" n) (push (replace-match ".h" t t n ) all-attempts))
    (if (string-match ".c$" n) (push (replace-match ".h" t t n ) all-attempts))
    (if (string-match ".cpp$" n) (push (replace-match ".hpp" t t n ) all-attempts))
    (if (string-match ".cxx$" n) (push (replace-match ".hxx" t t n ) all-attempts))

    (if (string-match "\\.mm$" n) (push (replace-match ".h" t t n ) all-attempts))
    (if (string-match "\\.m$" n) (push (replace-match ".h" t t n ) all-attempts))

    (setq attempts all-attempts)
    (while (and (not found) attempts)  ;;simple search first
      (let ((attempt (car attempts)))
        ;;Does the file exist?
        (if (file-readable-p attempt) (setq found attempt))
        (setq attempts (cdr attempts))))

    (if (and (not found) (gtags-get-rootpath))
        (dolist (attempt all-attempts)
          (unless found
            (with-temp-buffer
              (call-process (executable-find "global") nil t nil "-Pa" (concat "/" (file-name-nondirectory attempt) "$"))
              (if (eq (count-lines (point-min) (point-max)) 1)
                  (setq found (buffer-substring (point-min) (- (point-max) 1))))))))
    found))


(defun switch-cpp-h ()
  "Switch to the corresponding .cpp, .C, .cc or .h file."
  (interactive)
  (let ((found (find-corresponding-cpp-h)))
    (if found
        (find-file found)
      (ff-find-other-file))))

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


;;===================
;; Magit stuff
;;===================
(defun magit-show-revision-at-current-line()
  (interactive)
  (let ((file)
        (sha (buffer-substring (point-at-bol) (+ (point-at-bol) 7))))
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "Commits for file \\(.*\\) in [^ ]+$")
          (setq file (match-string 1))
        (error "Not in approriate magit-log buffer it seems")))
    (magit-git-command (format "show %s:%s" sha file))))

(defun buffer-is-visible (buffer)
  (let ((windows (window-list)) (ret))
    (while windows
      (when (eq buffer (window-buffer (car windows)))
        (setq windows nil)
        (setq ret t))
      (setq windows (cdr windows)))
    ret)
  )

(defun magit-refresh-status-buffer()
  (let ((topdir (magit-get-top-dir default-directory)))
    (if topdir
        (let ((buf (get-buffer (concat "*magit: " (file-name-nondirectory (directory-file-name topdir)) "*"))))
          (if (and buf (buffer-is-visible buf))
              (with-current-buffer buf
                (magit-refresh)))))
    )
  )

(defun magit-diff-current-section ()
  (interactive)
  (let ((file (magit-diff-item-file (magit-current-section))))
    (when file
      (setq git-default-directory default-directory)
      (git-diff-against (concat default-directory file) "HEAD")))
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
(defvar sam-auto-clean-whitespace nil)
(make-variable-buffer-local 'sam-auto-clean-whitespace)
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
  (let ((count 1))
    (forward-char)
    (while (and (> count 0) (< (point) (point-max)))
      (let ((char (elt (buffer-string) (1- (point)))))
        ;; (message "Foobar %c %d %d" char (point) count)
        (cond ((= char 60) (incf count)) ;; <
              ((= char 62) (decf count)) ;; >
              (t)))
      (if (> count 0)
          (forward-char))))
  )

(defun find-containers ()
  (let (containers (contents (buffer-string)))
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (re-search-forward "<[A-Za-z_]" nil t))
        (unless (string= (buffer-substring-no-properties (point-at-bol) (1- (point))) "#include <")
          (let (start end namestart name type)
            (backward-char 2)
            (setq start (point))
            (find>)
            (setq end (point))
            (forward-char)
            (skip-chars-forward " &")
            (setq namestart (point))
            (skip-chars-forward "A-Za-z0-9_")
            (setq name (buffer-substring-no-properties namestart (point)))
            (goto-char start)
            (skip-chars-backward " ")
            (skip-chars-backward "A-Za-z0-9:_")
            (setq type (buffer-substring-no-properties (point) end))
            (unless (or (string-match "^[^<]*\\(ptr\\|pointer\\|Ptr\\|Pointer\\)" type) (string= name ""))
              (add-to-list 'containers (concat type " " name)))))
        (if (< (point-at-eol) (point-max))
            (goto-char (1+ (point-at-eol)))
          (goto-char (point-max)))))
    containers)
  )

(defvar for-loop-space " ")
(defun insert-loop (&optional prefix erase)
  (interactive "P")
  (if (integerp prefix)
      (setq erase t))
  (if (and prefix (not (stringp prefix)) (not erase))
      (insert-for-loop)
    (let (container it)
      (cond ((stringp prefix) (setq container prefix))
            ((region-active-p) (setq container (buffer-substring-no-properties (region-beginning) (region-end))))
            (t
             (let ((containers (find-containers)))
               (unless containers (setq containers (list "std::map<std::string, int> map")))
               (add-to-list 'containers "")
               (setq container (ido-completing-read "Container: " containers))
               (if (string= container "")
                   (setq container (read-from-minibuffer "Container: "))))))
      (when (not (string= container "" ))
        (setq it (read-from-minibuffer "Iterator name (default 'it'): "))
        (let (name type (space (string-match " [^ ]*$" container)))
          (unless space
            (error "Invalid container %s" container))
          (setq type (substring container 0 space))
          (setq name (substring container (1+ space)))
          (if (string= it "")
              (setq it "it"))
          (when (region-active-p)
            (goto-char (point-at-eol))
            (insert "\n")
            (indent-according-to-mode))
          (if erase
              (insert (format "%s::iterator %s = %s.begin();\nwhile%s(%s != %s.end()) {\nif%s(remove) {\n%s.erase(%s++);\n} else {\n++%s;\n}\n}"
                              type it name for-loop-space it name for-loop-space name it it))
            (insert (format "for (%s::const_iterator %s = %s.begin(); %s != %s.end(); ++%s) {\n\n}\n"
                            type it name it name it)))
          (indent-prev-lines (if erase 6 3))
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
(setq compilation-error-regexp-alist (delete 'gnu compilation-error-regexp-alist))
(add-to-list 'compilation-error-regexp-alist compilation-gnu)
(add-to-list 'compilation-error-regexp-alist compilation-error-warning-regexp)
(add-to-list 'compilation-error-regexp-alist compilation-note-regexp)

(defun compilation-parse-errors-filename (filename)
  (if (or (equal major-mode 'grep-mode) (and filename (file-exists-p filename))) filename))
(setq compilation-parse-errors-filename-function (function compilation-parse-errors-filename))

;; (define-key global-map [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (let ((had-git-gutter git-gutter-mode))
    (unwind-protect
        (progn
          (linum-mode 1)
          (if had-git-gutter
              (git-gutter-mode 0))
          (let ((res (read-from-minibuffer "Goto line: ")))
            (cond ((string-match "^,\\([0-9]+\\)$" res)
                   (goto-char (1+ (string-to-int (match-string 1 res)))))
                  ((string-match "^\\([0-9]+\\)%$" res)
                   (goto-char (/ (* (point-max) (string-to-int (match-string 1 res))) 100)))
                  ((string-match "^:?\\([0-9]+\\):\\([0-9]+\\):?$" res)
                   (goto-line (string-to-int (match-string 1 res)))
                   (forward-char (1- (string-to-int (match-string 1 res)))))
                  (t (goto-line (string-to-int res))))))
      (linum-mode -1)
      (if had-git-gutter
          (git-gutter-mode 1)))
    )
  )
