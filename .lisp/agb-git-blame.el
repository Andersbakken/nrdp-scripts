(require 'diff-mode)
(defvar agb-git-blame-mode-hook nil)
(defvar agb-git-blame-last-temp-buffer nil)
(defvar agb-git-blame-last-file nil)
(defvar agb-git-blame-commit-chain nil)
(defvar agb-git-blame-last-blame-buffer nil)
(defvar agb-git-blame-showing-smaller nil)
(defvar agb-git-blame-use-relative-date nil)
(defcustom agb-git-blame-reuse-buffers
  t
  "Whether to reuse temp buffers for agb-git-blame"
  :group 'rtags
  :type 'boolean)

(defvar agb-git-blame-mode-map nil)
;; assign command to keys
(setq agb-git-blame-mode-map (make-sparse-keymap))

(define-key agb-git-blame-mode-map (kbd "q") (function bury-buffer))
(define-key agb-git-blame-mode-map (kbd "r") (function agb-git-reblame-for-revision))
(define-key agb-git-blame-mode-map (kbd "p") (function agb-git-reblame-for-previous-revision-~))
(define-key agb-git-blame-mode-map (kbd "^") (function agb-git-reblame-for-previous-revision-^))
(define-key agb-git-blame-mode-map (kbd "~") (function agb-git-reblame-for-previous-revision-~))
(define-key agb-git-blame-mode-map (kbd "n") (function agb-git-reblame-pop))
(define-key agb-git-blame-mode-map (kbd "=") (function agb-git-blame-show-diff))
(define-key agb-git-blame-mode-map (kbd "+") (function agb-git-blame-show-diff-other-window))
(define-key agb-git-blame-mode-map (kbd "SPC") (function agb-git-blame-show-diff-other-window))
(define-key agb-git-blame-mode-map (kbd "DEL") (function agb-git-blame-show-diff-other-window-back))
(define-key agb-git-blame-mode-map (kbd "s") (function agb-git-blame-toggle-smaller))
(define-key agb-git-blame-mode-map (kbd "t") (function agb-git-blame-toggle-use-relative-date))
(define-key agb-git-blame-mode-map (kbd "o") (function agb-git-blame-show-revision))
(define-key agb-git-blame-mode-map (kbd "f") (function agb-git-blame-show-revision))
(define-key agb-git-blame-mode-map (kbd "#") (function agb-git-blame-show-revision))
(define-key agb-git-blame-mode-map (kbd "<RET>") (function agb-git-blame-show-diff))
(define-key agb-git-blame-mode-map (kbd "<ENTER>") (function agb-git-blame-show-diff))

(define-derived-mode agb-git-blame-mode fundamental-mode
  ;; (setq font-lock-defaults '(agb-git-blame-faces))
  (setq mode-name "agb-git-blame")
  (use-local-map agb-git-blame-mode-map)
  (run-hooks 'agb-git-blame-mode-hook)
  (setq buffer-read-only t))

(defun agb-git-blame-post-command-hook ()
  (when (eq major-mode 'agb-git-blame-mode)
    (let ((wlist (window-list))
          (cur (get-buffer-window (current-buffer)))
          (blame-commit (agb-git-blame-current-commit))
          (commit-length)
          (blame-file (agb-git-blame-current-file)))
      (while wlist
        (let ((buf (window-buffer (car wlist))))
          (when (and (string= (buffer-name buf) "*magit-log*")
                     (with-current-buffer buf
                       (save-excursion
                         (goto-char (point-min))
                         (when (and (looking-at "Commits for file \\(.*\\) in [^ ]+$")
                                    (string= blame-file
                                             (file-truename (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
                           (forward-line 1)
                           (skip-chars-forward "[A-Fa-f0-9]")
                           (setq commit-length (- (point) (point-at-bol)))))))
            (select-window (get-buffer-window buf))
            (goto-char (point-min))
            (when (re-search-forward (concat "^" (substring blame-commit 0 commit-length) " ") nil t)
              (goto-char (point-at-bol)))
            (select-window cur)
            (setq wlist nil)))
        (setq wlist (cdr wlist))))))

(add-hook 'post-command-hook (function agb-git-blame-post-command-hook))

(defun agb-git-blame (&optional revision file)
  (interactive)
  (let* ((buffer-name (cond (file)
                            ((buffer-file-name))
                            ((agb-git-blame-filename))))
         (buf (get-buffer-create (format "*%s - Blame - %s*" buffer-name (or revision "HEAD"))))
         ;; (line (buffer-substring (point-at-bol) (point-at-eol)))
         (lineno (line-number-at-pos)))
    (unless buffer-name
      (error "Can't blame this file"))
    (unless revision (setq revision "HEAD"))
    (when (and agb-git-blame-reuse-buffers
               agb-git-blame-last-blame-buffer
               (not (eq buf agb-git-blame-last-blame-buffer)))
      (kill-buffer agb-git-blame-last-blame-buffer))
    (setq agb-git-blame-last-blame-buffer buf)
    ;; (when (string-match "[0-9][0-9]:[0-9][0-9]:[0-9] [0-9- ]* [0-9]+) \\(.*\\)$" line)
    ;;   (setq line (match-string 1 line)))
    (when (or (string= revision "HEAD") (not (string= buffer-name agb-git-blame-last-file)))
      (setq agb-git-blame-last-file buffer-name
            agb-git-blame-commit-chain nil))
    (when (not (member revision agb-git-blame-commit-chain))
      (push revision agb-git-blame-commit-chain))
    (set-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)

    (let ((args (list revision "--" buffer-name)))
      (push (if agb-git-blame-use-relative-date "--date=relative" "--date=local") args)
      (when agb-git-blame-showing-smaller
        (push "-s" args))
      (push "blame" args)
      (apply #'call-process "git" nil (current-buffer) nil args)
      (goto-char (point-min))
      (if (looking-at "fatal")
          (progn
            (if (> (length agb-git-blame-commit-chain) 1)
                (agb-git-blame (nth 1 agb-git-blame-commit-chain)))
            (message "No such commit [git %s]" (combine-and-quote-strings args)))
        (goto-char (- (point-max) 1))
        (if (re-search-backward "^[a-f0-9^]\\{8\\}[^)]*\\( [0-9]+\\)) ")
            (let* ((count (length (match-string 1)))
                   (column (- (match-beginning 1) (point-at-bol)))
                   (from (format "^\\(.\\{%d\\}\\).\\{%d\\}\\(.*\\)$" column count))
                   (to "\\1\\2"))
              (goto-char (point-min))
              (while (re-search-forward from nil t)
                (replace-match to nil nil))))
          (goto-char (point-min))
          (agb-git-blame-mode)
          (setq buffer-read-only t)
          (switch-to-buffer buf)
          (goto-char (point-min))
          (if (> (count-lines (point-min) (point-max)) lineno)
              (forward-line (1- lineno)))))))

(defun agb-git-reblame-for-revision (&optional suffix)
  (interactive)
  (let ((commit (agb-git-blame-current-commit)))
    (when commit
      (agb-git-blame (concat commit suffix) (agb-git-blame-current-file)))))

(defun agb-git-reblame-for-previous-revision-~ ()
  (interactive)
  (agb-git-reblame-for-revision "~"))

(defun agb-git-reblame-for-previous-revision-^ ()
  (interactive)
  (agb-git-reblame-for-revision "^"))

(defun agb-git-reblame-pop ()
  (interactive)
  (when (and (agb-git-blame-filename)
             (> (length agb-git-blame-commit-chain) 1))
    (setq agb-git-blame-commit-chain (cdr agb-git-blame-commit-chain))
    (agb-git-blame (car agb-git-blame-commit-chain))))

(defun agb-git-blame-filename ()
  (if (string-match "\\*\\(.*\\) - Blame - [A-Za-z0-9~]*\\*" (buffer-name))
      (match-string 1 (buffer-name))))

(defun agb-git-blame-current-commit()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\\([0-9a-f]\\{8,12\\}\\)[ )]")
        (match-string 1))))

(defun agb-git-blame-current-file()
  (let ((file (nth 1 (split-string (buffer-substring-no-properties (point-at-bol) (point-at-eol)) " "))))
    (cond ((not file) agb-git-blame-last-file)
          ((string= file "") agb-git-blame-last-file)
          ((string-match "(" file) agb-git-blame-last-file)
          (t (concat (agb-git-blame-root-dir) file)))))

(defun agb-git-blame-buffer-visible (buffer)
  (car (cl-member-if (lambda (arg) (eq buffer (window-buffer arg))) (window-list))))

(defun agb-git-blame-show-diff (&optional otherwindow)
  (interactive "P")
  (let ((commit (agb-git-blame-current-commit)))
    (if commit
        (let* ((bufname (format "*%s - %s*" (agb-git-blame-filename) commit))
               (buffer (get-buffer bufname))
               (visible (and buffer (agb-git-blame-buffer-visible buffer))))
          (if buffer
              (if otherwindow
                  (progn
                    (if visible
                        (if (= otherwindow 1)
                            (scroll-other-window)
                          (scroll-other-window-down))
                      (progn
                        (if (= 1 (length (window-list)))
                            (split-window))
                        (switch-to-buffer-other-window buffer)
                        (other-window 1))))
                (switch-to-buffer buffer))
            (progn
              (if (and agb-git-blame-reuse-buffers agb-git-blame-last-temp-buffer)
                  (kill-buffer agb-git-blame-last-temp-buffer))
              (if (not otherwindow)
                  (switch-to-buffer (get-buffer-create bufname))
                (progn
                  (if (= 1 (length (window-list)))
                      (split-window-vertically))
                  (switch-to-buffer-other-window (get-buffer-create bufname))
                  (other-window 1)))
              (setq agb-git-blame-last-temp-buffer (current-buffer))
              (erase-buffer)
              (call-process "git" nil (current-buffer) nil "show" commit)
              (goto-char (point-min))
              (diff-mode)
              (setq buffer-read-only t)))))))

(defun agb-git-blame-show-diff-other-window()
  (interactive)
  (agb-git-blame-show-diff 1))

(defun agb-git-blame-show-diff-other-window-back()
  (interactive)
  (agb-git-blame-show-diff -1))


(defvar agb-git-blame-show-revision-keymap (make-sparse-keymap))
(define-key agb-git-blame-show-revision-keymap (kbd "q") 'bury-buffer)

(defun agb-git-blame-root-dir (&optional directory) ;; stolen from git-mode
  (unless directory (setq directory default-directory))
  (let ((check-dir (cond (directory ;; extrapolate from name
                          (if (equal (substring directory -1) "/")
                              directory
                            (concat directory "/")))
                         (t default-directory))))
    (while (not (or (string-equal check-dir "/")
                    (file-exists-p (concat check-dir ".git"))))
      (setq check-dir (substring check-dir 0 (string-match "[^/]*/?$" check-dir))))
    (if (not (string-equal check-dir "/")) check-dir nil)))


(defun agb-git-blame-show-revision ()
  (interactive)
  (let ((commit (agb-git-blame-current-commit))
        (gitroot (agb-git-blame-root-dir)))
    (if (and commit gitroot)
        (let* ((fn (agb-git-blame-filename))
               (bufname (format "*%s@%s*" fn commit)))
          (if (get-buffer bufname)
              (kill-buffer bufname))
          (if (and agb-git-blame-reuse-buffers agb-git-blame-last-temp-buffer)
              (kill-buffer agb-git-blame-last-temp-buffer))
          (switch-to-buffer (get-buffer-create bufname))
          (setq agb-git-blame-last-temp-buffer (current-buffer))
          (let ((gitfn fn))
            (if (string= gitroot (substring gitfn 0 (length gitroot)))
                (setq gitfn (substring gitfn (length gitroot))))
            (call-process "git" nil (current-buffer) nil "show" (concat commit ":" gitfn)))
          (setq buffer-file-name fn)
          (set-auto-mode)
          (use-local-map agb-git-blame-show-revision-keymap)
          (setq buffer-file-name nil)
          (goto-char (point-min))
          (font-lock-fontify-buffer)
          (setq buffer-read-only t)))))

(defun agb-git-blame-toggle-smaller ()
  (interactive)
  (setq agb-git-blame-showing-smaller (not agb-git-blame-showing-smaller))
  (if (and (agb-git-blame-filename)
           (>= (length agb-git-blame-commit-chain) 1))
      (agb-git-blame (car agb-git-blame-commit-chain))))

(defun agb-git-blame-toggle-use-relative-date ()
  (interactive)
  (setq agb-git-blame-use-relative-date (not agb-git-blame-use-relative-date))
  (if (and (agb-git-blame-filename)
           (>= (length agb-git-blame-commit-chain) 1))
      (agb-git-blame (car agb-git-blame-commit-chain))))


(provide 'agb-git-blame)
