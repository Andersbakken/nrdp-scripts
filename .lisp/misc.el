
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
    (if (not (string-equal child "/"))
        t nil)))
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

(defun rotate-windows ()
  "Rotate your windows" 
  (interactive) 
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
             (setq i (1+ i)))))))

(defun toggle-window-split (&optional splitter)
  (interactive)
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

(defun magit-refresh-status-buffer()
  (let ((topdir (magit-get-top-dir default-directory)))
    (if topdir
        (let ((buf (concat "*magit: " (file-name-nondirectory (directory-file-name topdir)) "*")))
          (if (get-buffer buf)
              (with-current-buffer buf
                (magit-refresh)))))
    )
  )
