(require 'delsel)
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
  (let* ((buffers (buffer-list))
         (result))
    (while (and (not result) buffers)
      (let* ((buffer (car buffers))
             (file-name (buffer-file-name buffer)))
        (message (concat "Looking at " file-name))
        (when (and file-name (or (not name) (string-match name file-name)))
          (setq result file-name)
          (with-current-buffer buffer
            (save-restriction (widen) (setq result (format "%s:%d" result (line-number-at-pos))))))
        (message (concat "Done Looking at " file-name)))
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

;;====================
;; Carriage return bogusness
;;====================
(defun --misc-replace-string-helper (from to)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward from nil t)
      (replace-match to nil t))))

(defun dos-to-unix ()
  "Replace \r\n with \n"
  (interactive)
  (--misc-replace-string-helper "\r\n" "\n"))

(defun unix-to-dos ()
  "Replace \n with \r\n"
  (interactive)
  (--misc-replace-string-helper "\n" "\r\n" ))

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

(defun find-corresponding-cpp-h (&optional filename)
  (let ((n (or filename (buffer-file-name)))
        (attempts)
        (all-attempts)
        (start)
        (found))
    (goto-char (point-min))
    (if (re-search-forward "\\(//\\|/\\*\\) *SWITCH_FILE: *\"" nil t)
        (progn
          (setq start (point))
          (when (search-forward "\"")
            (backward-char)
            (push (buffer-substring start (point)) all-attempts))))
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
  (let (found)
    (save-excursion
      (setq found (find-corresponding-cpp-h)))
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

(defun git-gitify-path (file)
  (if (string-match "^/" file)
      (let ((root (magit-get-top-dir (file-name-directory file))))
        (if (string-match (concat "^" root) file)
            (setq file (substring file (length root))))))
  file)

(defun git-show-revision (file sha)
  (let ((line (and (string= file (buffer-file-name)) (count-lines 1 (point)))))
    (setq file (git-gitify-path file))
    (let ((buffer (get-buffer-create (format "%s - %s" file sha))))
      (switch-to-buffer buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (call-process "git" nil t t "show" (format "%s:%s" sha file))
      (setq buffer-file-name file)
      (set-auto-mode)
      (setq buffer-file-name nil)
      (goto-char (point-min))
      (if line
          (forward-line line))
      (font-lock-fontify-buffer)
      (setq buffer-read-only t))))

(defun git-show-head (&optional file)
  (interactive)
  (unless (or file (buffer-file-name))
    (error "Not a real file"))
  (git-show-revision (or file (buffer-file-name)) "HEAD"))

(defun magit-log-mode-current-file ()
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "Commits for file \\(.*\\) in [^ ]+$")
        (match-string 1)
      (error "Not in approriate magit-log buffer it seems")
      nil)))

(defun magit-show-revision-at-current-line()
  (interactive)
  (let ((file (magit-log-mode-current-file))
        (sha (save-excursion
               (goto-char (point-at-bol))
               (skip-chars-forward "[A-Fa-f0-9]")
               (buffer-substring-no-properties (point-at-bol) (point)))))
    (git-show-revision file sha)))

(defun magit-sync ()
  "Run git sync."
  (interactive)
  (magit-run-git-async "sync"))

(define-key magit-status-mode-map (kbd "-") 'magit-ediff)
(define-key magit-status-mode-map (kbd "U") 'magit-discard-item)
(define-key magit-status-mode-map (kbd "_") 'magit-diff-smaller-hunks)
(define-key magit-status-mode-map (kbd "=") 'magit-diff-current-section)
(define-key magit-status-mode-map (kbd "l") 'magit-log-current-section)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
(if (fboundp 'magit-show-file-revision)
    (define-key magit-log-mode-map (kbd "#") (function magit-show-file-revision))
  (define-key magit-log-mode-map (kbd "#") (function magit-show-revision-at-current-line)))

(defun misc-magit-add-action (group key name func)
  (interactive)
  (let ((group-actions (assoc-default 'actions (assoc-default group magit-key-mode-groups))))
    (add-to-list 'group-actions (list key name func))
    (push 'actions group-actions)
    (setf (second (assoc-default group magit-key-mode-groups)) group-actions)
    (setq magit-key-mode-keymaps 'nil)))

(defun magit-blame-for-current-revision ()
  (interactive)
  (let ((file (magit-log-mode-current-file))
        (sha (save-excursion
               (goto-char (point-at-bol))
               (skip-chars-forward "[A-Fa-f0-9]")
               (buffer-substring-no-properties (point-at-bol) (point)))))
    (when (and file sha)
      (agb-git-blame sha file))))

(defun magit-choose-push ()
  (interactive)
  (let* ((remote
          (with-temp-buffer
            (call-process "git" nil t nil "remote")
            (let ((remotes (split-string (buffer-string))))
              (if (= (length remotes) 1)
                  (car remotes)
                (ido-completing-read "Remote: " remotes)))))
         (branch
          (with-temp-buffer
           (call-process "git" nil t nil "branch" "-r")
           (goto-char (point-min))
           (let ((match (concat "^ *" remote "/\\(.*\\)$"))
                 (branches))
             (while (not (eobp))
               (when (looking-at match)
                 (push (match-string 1) branches))
               (forward-line 1))
             (if branches
                 (ido-completing-read "Branch: " branches)
               (read-from-minibuffer "Branch: "))))))
    (when (and branch remote)
      (magit-run-git-async "push" remote (concat "HEAD:" branch) magit-custom-options))))

(misc-magit-add-action 'pulling "S" "Sync" 'magit-sync)
(misc-magit-add-action 'pushing "J" "Jira" 'magit-jira)
(misc-magit-add-action 'pushing "R" "Jira (Don't resolve)" 'magit-jira-no-resolve)
(misc-magit-add-action 'pushing "S" "Submit" 'magit-submit)
(misc-magit-add-action 'pushing "C" "Choose" 'magit-choose-push)
(misc-magit-add-action 'pushing "A" "Submit All" 'magit-submit-all)
(misc-magit-add-action 'pushing "I" "Ignore" 'magit-ignore)
(misc-magit-add-action 'logging "b" "Blame" 'magit-blame-for-current-revision)

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
    (when topdir
      (let ((buf (get-buffer (concat "*magit: " (file-name-nondirectory (directory-file-name topdir)) "*"))))
        (when (and buf (buffer-is-visible buf))
          (with-current-buffer buf
            (magit-refresh)))))))

(defun magit-current-section-string ()
  (let* ((section (magit-current-section))
         (info (and section (magit-section-info (magit-current-section)))))
    (cond ((and (listp info) (stringp (nth 1 info))) (nth 1 info))
          ((stringp info) info)
          (t nil))))

(defun magit-current-section-file ()
  (let ((section (magit-current-section-string)))
    (and section (file-exists-p section) section)))

(defun magit-current-section-sha ()
  (let ((string (magit-current-section-string)))
    (cond ((not string) nil)
          ((file-exists-p string) nil)
          ((string-match "[^A-Fa-f0-9]" string) nil)
          (t string))))

(defun magit-diff-current-section ()
  (interactive)
  (let ((file (magit-current-section-file)))
    (when file
      (setq git-default-directory default-directory)
      (git-diff-against (concat default-directory file) "HEAD")))
  )

(defun magit-log-current-section ()
  (interactive)
  (let ((file (magit-current-section-file)))
    (if file
        (magit-file-log file)
      (magit-key-mode-popup-logging)))
  )

(defun magit-run-on-multiple (commands &optional commit)
  (let (args)
    (cond (commit (push commit args))
          (mark-active
           (let ((lines (split-string (buffer-substring-no-properties
                                       (save-excursion
                                         (goto-char (min (region-beginning) (region-end)))
                                         (point-at-bol))
                                       (save-excursion
                                         (goto-char (1- (max (region-beginning) (region-end))))
                                         (point-at-eol))) "\n")))
             (while lines
               (let ((line (car lines)))
                 (if (string-match "^[A-Fa-f0-9]+" line)
                     (progn
                       (push (match-string 0 line) args)
                       (setq lines (cdr lines)))
                   (setq lines nil args nil))))))
          ((magit-current-section-sha) (push (magit-current-section-sha) args))
          (t
           (push (let ((val (read-from-minibuffer "Sha (default HEAD): " nil nil nil "HEAD")))
                   (cond ((string= "" val) "HEAD")
                         (t val)))
                 args)))
    (when (> (length args) 0)
      (cond ((listp commands)
             (let ((rev (reverse commands)))
               (while rev
                 (push (car rev) args)
                 (setq rev (cdr rev)))))
            (t
             (push commands args)))
      ;; (message "running: [%s]" (combine-and-quote-strings args))
      (apply #'magit-run-git-async args))))

(defun magit-jira (&optional commit noresolve)
  (interactive)
  (magit-run-on-multiple (if noresolve
                             (list "jira" "--no-interactive" "--comment")
                           (list "jira" "--resolve" "--no-interactive" "--comment")) commit))

(defun magit-jira-no-resolve (&optional commit)
  (interactive)
  (magit-jira commit t))

(defun magit-submit (&optional commit)
  (interactive)
  (magit-run-on-multiple "submit" commit))

(defun magit-ignore (&optional commit)
  (interactive)
  (magit-run-on-multiple "ignore" commit))

(defun magit-submit-all (&optional commit)
  (interactive)
  (magit-run-git-async "submit" "-a"))

(defun magit-ediff-buffers (a b)
  (setq magit-ediff-buffers (list b a))
  (setq magit-ediff-windows (current-window-configuration))
  (ediff-buffers b a '(magit-ediff-add-cleanup)))

;; ================================================================================
;; git-jira
;; ================================================================================

(defun git-jira (&optional commit)
  (interactive)
  (unless commit
    (setq commit (magit-read-rev-with-default "Jira commit: ")))
  (if commit
      (call-process "git-jira" nil nil nil "--resolve" "--no-interactive" commit)))

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
                      (add-to-list 'typedefs name)
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
(setq compilation-error-regexp-alist (delete 'gnu compilation-error-regexp-alist))
(add-to-list 'compilation-error-regexp-alist compilation-gnu)
(add-to-list 'compilation-error-regexp-alist compilation-error-warning-regexp)
(add-to-list 'compilation-error-regexp-alist compilation-note-regexp)

(defun compilation-parse-errors-filename (filename)
  (if (or (equal major-mode 'grep-mode) (and filename (file-exists-p filename))) filename))
(setq compilation-parse-errors-filename-function (function compilation-parse-errors-filename))

;; (define-key global-map [remap goto-line] 'goto-line-with-feedback)
(defun --misc-goto-line-helper (N)
  (goto-char (point-min))
  (forward-line (1- N)))

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
                   (goto-char (1+ (string-to-number (match-string 1 res)))))
                  ((string-match "^\\([0-9]+\\)%$" res)
                   (goto-char (/ (* (point-max) (string-to-number (match-string 1 res))) 100)))
                  ((string-match "^:?\\([0-9]+\\):\\([0-9]+\\):?$" res)
                   (--misc-goto-line-helper (string-to-number (match-string 1 res)))
                   (forward-char (1- (string-to-number (match-string 1 res)))))
                  (t (--misc-goto-line-helper (string-to-number res))))))
      (linum-mode -1)
      (if had-git-gutter
          (git-gutter-mode 1)))
    )
  )

;; ================================================================================
;; agb-occur
;; ================================================================================

(defun agb-occur-data ()
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "^[0-9]+ match\\(es\\)\? for \"\\(.*\\)\" in buffer: \\(.*\\)")
        (cons (buffer-substring-no-properties (match-beginning 2) (match-end 2))
              (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
  )

(defun agb-is-occur-buffer () (string-match (buffer-name) "*Occur*"))

(defun agb-occur-read-from-minibuffer ()
  (let* ((word (current-word))
         (pattern
          (read-from-minibuffer
           (if (or (not word) (string= "" word))
               "List lines matching regexp: "
             (format "List lines matching regexp (default: %s): " word)))))
    (if (or (not pattern) (string= pattern ""))
        word
      pattern)))

(defun agb-occur (&optional dont-reoccur)
  (interactive "P")
  (if (and (not dont-reoccur) (agb-is-occur-buffer))
      (let* ((pattern (agb-occur-read-from-minibuffer))
             (data (and pattern (agb-occur-data)))
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

;; ================================================================================
;; agb-isearch
;; ================================================================================
;; (define-key isearch-mode-map (kbd "C-w") (function agb-isearch-yank-word-or-char-from-beginning-of-symbol))
;; (define-key isearch-mode-map (kbd "M-w") (function isearch-yank-word-or-char))

(defun agb-isearch-yank-word-or-char-from-beginning-of-symbol ()
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


(defun misc-compare-files-by-modification-time (l r)
  (time-less-p (nth 6 l) (nth 6 r)))

(defun misc-directory-files-helper (dir match dirsonly sortbymodicationtime)
  (let ((all (directory-files-and-attributes dir nil match t))
        (ret))
    (if sortbymodicationtime
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
                "include_directories(${CMAKE_CURRENT_LIST_DIR})\n"
                "set(CMAKE_CXX_FLAGS \"${CMAKE_CXX_FLAGS} -std=c++0x\")\n"
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
          "    // nrdp.gibbon.load({url:\"http://en.wikipedia.org/wiki/Leif_Erikson\", headers: {\"X-Gibbon-Cache-Control:max-age:1000}}, networkResponseHandler);\n"
          "    nrdp.gibbon.scene.widget = w = nrdp.gibbon.makeWidget({width:1280, height:720, color:\"#00ff00\"});\n"
          "    // w.image.url = {url:\"http://cdn-1.nflximg.com/images/7516/817516.jpg\"};\n"
          "    // nrdp.gibbon.setTimeout(onTimeout, 0);\n"
          "}\n"
          "nrdp.gibbon.init(main);\n"))

(defun mkgibbontest-copy (name)
  (kill-new (concat mkgibbontest-webprefix "/" name)))

(defun mkgibbontest-copy-current ()
  (interactive)
  (mkgibbontest-copy (file-name-nondirectory (buffer-file-name))))

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
  (let* ((tests (misc-directory-files-helper mkgibbontest-directory "gibbontest-" nil t))
         (test (and tests (ido-completing-read (format "Gibbon test (default %s): " (car tests)) tests)))
         (abspath (concat mkgibbontest-directory "/" (or test (car tests)))))
    (when (file-exists-p abspath)
      (mkgibbontest-copy (file-name-nondirectory abspath))
      (find-file abspath))))


;;

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

(provide 'nrdp-misc)
