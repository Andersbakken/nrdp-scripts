;;===================
;; Magit stuff
;;===================

;; 1.4.2 compatibility
(require 'magit)
(require 's)

(defun buffer-local-set-key (key func)
  (interactive "KSet key on this buffer: \naCommand: ")
  (let ((name (format "%s-magic" (buffer-name))))
    (eval
      `(define-minor-mode ,(intern name)
                          "Automagically built minor mode to define buffer-local keys."))
    (let* ((mapname (format "%s-map" name))
           (map (intern mapname)))
      (unless (boundp (intern mapname))
        (set map (make-sparse-keymap)))
      (eval
        `(define-key ,map ,key func)))
    (funcall (intern name) t)))

(define-key magit-file-section-map [C-return] 'magit-diff-visit-file)
(define-key magit-file-section-map "\r" 'magit-diff-visit-file-worktree)
(define-key magit-hunk-section-map [C-return] 'magit-diff-visit-file)
(define-key magit-hunk-section-map "\r" 'magit-diff-visit-file-worktree)

(define-key magit-file-mode-map "\C-xg" nil)

(defun nrdp-magit-section-type (object)
    (if (fboundp 'magit-section-type)
        (magit-section-type object)
      (oref object type)))

(defun nrdp-magit-section-value (object)
  (if (fboundp 'magit-section-value)
      (magit-section-value object)
    (oref object value)))

(define-key magit-mode-map (kbd "M-w") 'kill-ring-save)
(defun nrdp-git-magit-buffer-file-name ()
  (and (stringp header-line-format)
       (string-match "Commits in [^ ]+ touching \\([^ ]+\\)" header-line-format)
       (concat default-directory (match-string 1 header-line-format))))

(defun nrdp-git-magit-log-args (prefix &rest other)
  (let* ((n (cond ((null prefix) (list (concat "-n" (number-to-string (* (window-height) 3)))))
                  ((numberp prefix) (list (concat "-n" (number-to-string prefix))))
                  (t nil))))
    (if other
        (append other n)
      n)))

(defun nrdp-git-magit-current-things ()
  (let* ((types (list 'commit 'file 'stash))
         (type types)
         (matchedtype)
         (ret))
    (while type
      (setq ret (magit-region-sections (car type)))
      (if ret
          (setq matchedtype (car type)
                type nil)
        (setq type (cdr type))))
    (unless ret
      (let ((current (magit-current-section)))
        (when current
          (setq matchedtype (car (member (nrdp-magit-section-type current) types))
                ret (list current)))))
    (cond ((eq matchedtype 'file)
           (let ((root (magit-toplevel)))
             (cons (mapcar (lambda (item) (concat root (nrdp-magit-section-value item))) ret) 'file)))
          (matchedtype (cons (mapcar #'nrdp-magit-section-value ret) matchedtype))
          (t nil))))

(defun nrdp-git-magit-current-things-filtered (filter)
  (let ((ret (nrdp-git-magit-current-things)))
    (and (eq (cdr ret) filter) (car ret))))

(defun nrdp-git-magit-file-log (&optional prefix bufferorfilename)
  (interactive "P")
  (let* ((file (if (stringp bufferorfilename)
                   bufferorfilename
                 (nrdp-git-magit-buffer-file-name)))
         (buf (cond ((bufferp bufferorfilename) bufferorfilename)
                    (file (or (find-buffer-visiting file)
                              (find-file-noselect file)))
                    ((buffer-file-name) (current-buffer))
                    (t nil))))
    (if buf
        (with-current-buffer buf
          (let* ((magit-buffer-file-name (file-truename (buffer-file-name buf)))
                 (default-directory (file-name-directory magit-buffer-file-name)))
            (magit-log-head (nrdp-git-magit-log-args prefix "--follow") (list magit-buffer-file-name))))
      (message "Can't log this buffer"))))

(defun nrdp-git-merge-conflicts (&optional file)
  (interactive "P")
  (cond ((stringp file))
        ((bufferp file) (setq file (buffer-file-name)))
        ((integerp file)) ;; all files
        ((null file) (setq file (buffer-file-name)))
        (t (setq file (read-file-name "File: "))))
  (when file
    (setq file (file-truename file)))
  (let ((default-directory (nrdp-git-dir-for-file (or file default-directory)))
        (buffer (get-buffer-create (if file (format "*%s - git merge-conflicts*" file) "*git merge-conflicts*"))))
    (switch-to-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (if (not (= (call-process "git" nil t nil "merge-conflicts" "--no-color" "-p" (or file "")) 0))
        (progn
          (message "%s" (buffer-substring-no-properties (point-min) (1- (point-max))))
          (kill-buffer buffer))
      (goto-char (point-min))
      (diff-mode)
      (setq buffer-read-only t)
      (use-local-map (copy-keymap diff-mode-map))
      (local-set-key (kbd "q") 'bury-buffer))))

(defun nrdp-git-magit-log (&optional prefix)
  (interactive "P")
  (magit-log-other (list (magit-get-current-branch)) (nrdp-git-magit-log-args prefix)))

(fset 'magit-toggle-whitespace
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("D-ws" 0 "%d")) arg)))

(magit-define-popup-action 'magit-pull-popup ?S "Sync" 'nrdp-magit-sync)
(magit-define-popup-action 'magit-push-popup ?S "Submit" 'magit-submit)
(magit-define-popup-action 'magit-push-popup ?s "Submit pull-request" 'magit-submit-pull-request)
(magit-define-popup-action 'magit-push-popup ?J "Jira" 'magit-jira)
(magit-define-popup-action 'magit-push-popup ?R "Jira (Don't resolve)" 'magit-jira-no-resolve)
(magit-define-popup-action 'magit-push-popup ?I "Ignore" 'magit-ignore-commit)
(magit-define-popup-action 'magit-stash-popup ?b "Buffer" 'nrdp-git-stash-buffer)

(magit-define-popup-switch 'magit-push-popup ?D "Delete" "-d")
(magit-define-popup-switch 'magit-push-popup ?a "All commits" "-a")

(when (string< "20151209.731" (magit-version))
  (magit-define-popup-action 'magit-pull-popup ?F "Pull from tracking" 'magit-pull-from-upstream)
  (magit-define-popup-action 'magit-push-popup ?P "Push to tracking" 'magit-push-current-to-upstream))
(magit-define-popup-action 'magit-log-popup ?b "Blame" 'magit-blame-for-current-revision)

(unless (fboundp 'magit-current-section-string)
  (defun magit-current-section-string ()))

(defun nrdp-magit-visit-thing-advice (orig-fun &rest args)
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (if (looking-at "^\\([A-Fa-f0-9]+\\) .*ago$")
        (magit-show-commit (match-string 1) t)
      (call-interactively orig-fun))))

(advice-add 'magit-visit-thing :around #'nrdp-magit-visit-thing-advice)

(defun nrdp-git-gitify-path (file sha &optional follow)
  (when (string-match "^/" file)
    (let ((root (magit-toplevel (file-name-directory file))))
      (when (string-match (concat "^" root) file)
        (setq file (substring file (length root))))))
  (when (and file follow)
    (with-temp-buffer
      (let ((fullrev (shell-command-to-string (concat "git rev-parse " sha))))
        (cd (magit-toplevel (file-name-directory file)))
        (call-process "git" nil t nil "log" "--stat" "--stat-name-width=10000" "--follow" "--pretty=%H" file)
        (goto-char (point-min))
        (when (and (search-forward fullrev nil t)
                   (re-search-forward "[A-Za-z0-9]" nil t))
          (let ((start (1- (point))))
            (when (search-forward " " nil t)
              (setq file (buffer-substring-no-properties start (1- (point))))))))))
  file)

(defun nrdp-git-grep-shell-quote-argument (arg)
  (cond ((string-match "^\"" arg) arg)
        ((not (string-match " " arg)) (concat "\"" arg "\""))
        ((not (string-match "\"" arg)) (concat "\"" arg "\""))
        ((not (string-match "'" arg)) (concat "'" arg "'"))
        (t (shell-quote-argument arg))))

(defun nrdp-git-grep-prompt (dir)
  (let* ((default (current-word))
         (prompt (if default
                     (format "git grep: (default %s) " default)
                   "git grep: "))
         (search (read-from-minibuffer prompt (format "pwd: %s " dir) nil nil nil default)))
    (if (> (length search) 0)
        search
      (or default ""))))

(defun nrdp-git-grep-split-arguments (string)
  (let ((i 0) result current quotep escapedp word)
      (while (< i (length string))
        (setq current (aref string i))
        (cond
         ((and (char-equal current ?\ )
               (not quotep))
          (when word
            (push word result))
          (setq word nil escapedp nil))
         ((and (or (char-equal current ?\')
                   (char-equal current ?\"))
               (not escapedp)
               (not quotep))
          (setq quotep current
                escapedp nil))
         ((and (or (char-equal current ?\')
                   (char-equal current ?\"))
               (char-equal current quotep)
               (not escapedp))
          (push word result)
          (setq quotep nil
                word nil
                escapedp nil))
         ((char-equal current ?\\)
          (when escapedp (push current word))
          (setq escapedp (not escapedp)))
         (t
          (when escapedp
            (push ?\\ word))
          (setq escapedp nil)
          (push current word)))
        (incf i))
      (when quotep
        (error (format "Unbalanced quotes at %d"
                       (- (length string) (length word)))))
      (when word
        (push word result))

      (mapcar (lambda (x) (coerce (reverse x) 'string))
              (reverse result))))

(defun nrdp-git-grep (&optional dir)
  "git-grep the entire current repo"
  (interactive)
  (unless dir
    (setq dir (magit-toplevel)))
  (unless dir
    (error "No git dir"))
  (let* ((default (current-word))
         (pipe)
         (prompt (if default
                     (format "git grep: (default %s) pwd: " default)
                   "git grep: pwd: "))
         (result (read-from-minibuffer prompt (concat dir " ") nil nil nil default))
         (search))
    (unless (string-match "^\\(/[^ ]+\\) \\(.*\\)$" result)
      (error "Bad string!"))
    (setq dir (match-string 1 result))
    (setq search (if (> (match-end 2) (match-beginning 2))
                     (match-string 2 result)
                   default))

    (unless search
      (error "Nothing to search for"))
    (let ((pipeidx (string-match "|" search)))
      (when pipeidx
        (setq pipe (substring search pipeidx))
        (setq search (substring search 0 pipeidx))))
    (setq search (s-chop-suffix " " search))
    (when (string-match "^[^\"'-]* [^\"'-]*$" search)
      (setq search (concat "\"" search "\"")))

    (let* ((hasdashdash)
           (quote)
           (hasarg)
           (default-directory (magit-toplevel dir))
           (args (mapcar (lambda (arg)
                           (when (string= arg "--")
                             (setq hasdashdash t))
                           (when (and (not hasdashdash)
                                      (not hasarg)
                                      (not (string= "-" (substring arg 0 1))))
                             (setq hasarg t))
                           (nrdp-git-grep-shell-quote-argument arg))
                         (nrdp-git-grep-split-arguments search))))
      (when (and (not hasdashdash)
                 (not hasarg))
        (push "--" args))

      ;; (message "%s" (concat "git --no-pager grep -I -n "
      ;;                  (mapconcat 'identity args " ")
      ;;                  (if hasdashdash
      ;;                      " "
      ;;                    " -- ")
      ;;                  dir
      ;;                  " ':!*/error.js' ':!*/xboxupsellpage.js' ':!*/boot.js' ':!*min.js'"
      ;;                  pipe)))))

      (grep-find (concat "git --no-pager grep --recurse-submodules -I -n "
                         (mapconcat 'identity args " ")
                         (if hasdashdash
                             " "
                           " -- ")
                         dir
                         " ':!*/sunspider/*' ':!*/error-text/*' ':!*/xboxupsellpage.js' ':!*/mkdocs-material*' ':!*min.js' ':!*/jquery*.js'"
                         pipe)))))

(defun nrdp-git-grep-dwim (&optional prefix)
  (interactive "P")
  (let* ((devs (lsdev-dirs-internal default-directory "src"))
         (dir (magit-toplevel (and (= (length devs) 1) (cadar devs)))))
    (unless dir
      (error "No git dir"))
    (nrdp-git-grep (cond ((and prefix (file-directory-p (concat dir "src")))
                          (concat dir "src"))
                         ((null prefix) dir)
                         ((stringp prefix) (concat dir prefix))
                         (t default-directory)))))

(defun nrdp-git-config-value (conf)
  (let ((ret (shell-command-to-string (concat "git config " conf))))
    (if (and ret (string-match "\n$" ret))
        (substring ret 0 (1- (length ret)))
      ret)))

(defun nrdp-git-dir-for-file (&optional file)
  (magit-toplevel (file-name-directory (file-truename (cond ((stringp file) file)
                                                            ((buffer-file-name file))
                                                            (t default-directory))))))

(defun nrdp-git-revert (&optional buffer)
  (interactive)
  (let* ((default-directory (nrdp-git-dir-for-file buffer))
         (file (and (buffer-file-name buffer)
                    (file-truename (buffer-file-name buffer)))))
    (cond ((not file) (message "This buffer is not visiting a file"))
          ((not default-directory) (message "Dude... where's the repo?"))
          (t (and (y-or-n-p (concat "Are you sure you want to revert " (file-name-nondirectory file)))
                  (= (call-process "git" nil nil nil "reset" "--" file) 0)
                  (= (call-process "git" nil nil nil "checkout" "HEAD" "--" file) 0)
                  (with-current-buffer (or buffer (current-buffer))
                    (revert-buffer t t t)))))))


(defun nrdp-git-deepest-root ()
  (let ((path default-directory) best)
    (while (> (length path) 1)
      (setq best (or (magit-toplevel path) best))
      (setq path (and (string-match "\\(.*/\\).*/" path) (match-string 1 path))))
    best))

(defun nrdp-git-show-revision (&optional file sha)
  (interactive "P")
  (setq file (expand-file-name (cond ((stringp file) file)
                                     ((bufferp file) (buffer-file-name file))
                                     ((null file) (buffer-file-name))
                                     (t (setq file (read-file-name "File: "))))))
  (unless (file-exists-p file)
    (error "You have to select a file! (%s)" file))
  (setq file (file-truename file))
  (let ((default-directory (nrdp-git-dir-for-file file)))
    (unless sha
      (setq sha (completing-read "Sha: " (with-temp-buffer
                                           (call-process "git" nil t nil "branch" "-a")
                                           (goto-char (point-min))
                                           (let ((branches)
                                                 (current))
                                             (while (not (eobp))
                                               (when (cond ((looking-at "^\\* \\(.*\\)$") (setq current (match-string 1)) nil)
                                                           ((looking-at "^  remotes/\\(.*\\)$"))
                                                           ((looking-at "^.*\.git-submit$") nil)
                                                           ((looking-at "^  \\(.*\\)$"))
                                                           (t nil))
                                                 (push (match-string 1) branches))
                                               (forward-line 1))
                                             (when current
                                               (push current branches))
                                             branches)))))
    (unless sha
      (error "You have to pick a SHA!"))
    (let* ((line (and (string= file (buffer-file-name)) (count-lines 1 (point))))
           (git-file (nrdp-git-gitify-path file sha))
           (buffer (get-buffer-create (format "*%s - %s*" git-file sha))))
      (switch-to-buffer buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (unless (= (call-process "git" nil t nil "show" (format "%s:%s" sha git-file)) 0)
        (erase-buffer)
        (setq git-file (nrdp-git-gitify-path file sha t))
        (rename-buffer (format "*%s - %s*" git-file sha))
        (call-process "git" nil t nil "show" (format "%s:%s" sha git-file)))
      (goto-char (point-min))
      (if line
          (forward-line line))
      (setq buffer-file-name git-file)
      (set-auto-mode)
      (setq buffer-file-name nil)
      (font-lock-flush)
      (setq buffer-read-only t)
      (buffer-local-set-key (kbd "q") 'bury-buffer))))

(defvar nrdp-git-diff-reuse-diff-buffer nil)
(defun nrdp-git-diff (&optional -w target no-split-window norestorefocus against word)
  (interactive "P")
  (let* ((file (cond ((null target)
                      (if (buffer-file-name)
                          (file-truename (buffer-file-name))
                        (error "nrdp-git-diff: Not a file buffer")))
                     ((bufferp target)
                      (if (buffer-file-name target)
                          (file-truename (buffer-file-name target))
                        (error "nrdp-git-diff: Not a file buffer")))
                     ((stringp target) target)
                     (target (magit-toplevel))
                     (t (error "nrdp-git-diff: What to do here?"))))
         (dir (nrdp-git-dir-for-file file))
         (old (and (not norestorefocus) (get-buffer-window)))
         (numwindows (length (window-list)))
         (args (list (or against "HEAD") "--" file))
         (buffer (get-buffer-create (if (or nrdp-git-diff-reuse-diff-buffer (not args))
                                        "*git-diff*"
                                      (concat "*git-diff: " (car args) "*")))))
    (when dir
      (when -w
        (push "-w" args))
      (when word
        (push "--color=always" args)
        (push "--word-diff=plain" args)
        (push "--word-diff-regex=." args))
      (if no-split-window
          (switch-to-buffer buffer)
        (set-buffer (switch-to-buffer-other-window buffer)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq default-directory dir)
      (if (and (= (apply #'call-process "git" nil t t "diff" args) 0)
               (not (= (point-min) (point-max))))
          (progn
            (goto-char (point-min))
            (when word
              (ansi-color-apply-on-region (point-min) (point-max)))
            (insert "$ git diff " (combine-and-quote-strings args) "\n")
            (search-forward-regexp "^@@ ")
            (goto-char (point-at-bol))
            (diff-mode)
            (setq buffer-read-only t)
            (when old
              (select-window old)))
        (message "No differences")
        (kill-buffer (current-buffer))
        (if (= numwindows 1)
            (delete-window)
          (other-window 1))
        nil))))

(defun nrdp-git-diff-tracking (&optional -w target no-split-window norestorefocus word)
  (interactive "P")
  (let ((tracking (magit-get-upstream-branch)))
    (if tracking
        (nrdp-git-diff -w target no-split-window norestorefocus tracking word)
      (message "No tracking branch for branch"))))

(defun nrdp-git-word-diff (&optional -w target no-split-window norestorefocus against)
  (interactive "P")
  (nrdp-git-diff -w target no-split-window norestorefocus against t))

(defun nrdp-git-diff-other (&optional -w target)
  (interactive "P")
  (nrdp-git-diff -w target))

(defun nrdp-git-diff-repo (&optional -w)
  (interactive "P")
  (nrdp-git-diff -w))

(defun nrdp-git-diff-directory (&optional -w)
  (interactive "P")
  (nrdp-git-diff -w default-directory))

(defun nrdp-git-show-head (&optional file)
  (interactive)
  (unless (or file (buffer-file-name))
    (error "Not a real file"))
  (nrdp-git-show-revision (or file (buffer-file-name)) "HEAD"))

(defun nrdp-git-show-tracking (&optional file)
  (interactive)
  (unless (or file (buffer-file-name))
    (error "Not a real file"))
  (let* ((default-directory (nrdp-git-dir-for-file file))
         (tracking (magit-get-upstream-branch)))
    (if tracking
        (nrdp-git-show-revision (file-truename (or file (buffer-file-name))) tracking)
      (message "No tracking branch for branch"))))

(defun magit-log-mode-current-file ()
  (save-excursion
    (goto-char (point-min))
    (let ((file (cond ((looking-at "Commits for file \\(.*\\) in [^ ]+$")
                       (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                      ((string-match "Commits in [^ ]* touching \\(.*[^ ]\\) *$" header-line-format)
                       (match-string 1 header-line-format))
                      (t
                       (error "Not in approriate magit-log buffer it seems")))))
      (if (and file (not (file-name-absolute-p file))) (concat (magit-toplevel) file) file))))

(defun magit-show-revision-at-current-line()
  (interactive)
  (let ((file (magit-log-mode-current-file))
        (sha (save-excursion
               (goto-char (point-at-bol))
               (skip-chars-forward "[A-Fa-f0-9]")
               (buffer-substring-no-properties (point-at-bol) (point)))))
    (nrdp-git-show-revision file sha)))

(defun nrdp-magit-sync ()
  "Run git sync."
  (interactive)
  (magit-run-git-async "sync" "--no-color"))

;; Prevent *magit-process* from stealing focus when it pops up.
(defadvice pop-to-buffer (around return-focus activate)
  (let ((prev (selected-window)))
    ad-do-it
    (when (and prev
               (not (eq prev (selected-window)))
               (or (and (fboundp 'magit-process-buffer-name)
                        (string= (buffer-name) (magit-process-buffer-name)))
                   (string-match "^\\*magit-process: " (buffer-name))))
      (select-window prev))))

(defun nrdp-magit-status (&optional prefix)
  (interactive "P")
  (when (or prefix (not (let ((buffers (buffer-list))
                              (top (magit-toplevel))
                              (found))
                          (unless top
                            (error "Not a git repo"))
                          (while (and buffers (not found))
                            (let ((buf (car buffers)))
                              (if (and (string-match "^\*magit: " (buffer-name buf))
                                       (string= (with-current-buffer buf default-directory) top))
                                  (setq found buf buffers nil)
                                (setq buffers (cdr buffers)))))
                          (when found
                            (switch-to-buffer found)
                            (magit-refresh)
                            t))))
        (call-interactively 'magit-status)))

(defadvice magit-status-internal (around fixdir activate)
  (let ((dir (ad-get-arg 0)))
    (unless (file-name-absolute-p dir)
      (ad-set-arg 0 (expand-file-name dir))))
  ad-do-it)

(define-key magit-status-mode-map (kbd "-") (lambda (arg) (interactive "p") (nrdp-git-ediff-file (find-file-noselect (magit-current-section-file)))))
(define-key magit-status-mode-map (kbd "U") 'magit-discard-item)
(define-key magit-status-mode-map (kbd "_") 'magit-diff-less-context)
(define-key magit-status-mode-map (kbd "=") 'magit-diff-current-section)
(define-key magit-status-mode-map (kbd "l") 'magit-log-current-section)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
(if (fboundp 'magit-show-file-revision)
    (define-key magit-log-mode-map (kbd "#") (function magit-show-file-revision))
  (define-key magit-log-mode-map (kbd "#") (function magit-show-revision-at-current-line)))
(define-key magit-log-mode-map (kbd "=") (function nrdp-magit-log-show-diff))
(define-key magit-log-mode-map (kbd "@") (function magit-blame-for-current-revision))

(defun magit-blame-for-current-revision ()
  (interactive)
  (let ((file (magit-log-mode-current-file))
        (sha (save-excursion
               (goto-char (point-at-bol))
               (skip-chars-forward "[A-Fa-f0-9]")
               (buffer-substring-no-properties (point-at-bol) (point)))))
    (when (and file sha)
      (agb-git-blame sha (file-name-nondirectory file)))))

(defun buffer-is-visible (buffer)
  (let ((windows (window-list)) (ret))
    (while windows
      (when (eq buffer (window-buffer (car windows)))
        (setq windows nil)
        (setq ret t))
      (setq windows (cdr windows)))
    ret))

(defun magit-find-current-status-buffer ()
  (let ((topdir (magit-toplevel default-directory)))
    (when topdir
      (get-buffer (concat "*magit: " (file-name-nondirectory (directory-file-name topdir)) "*")))))

(defun magit-refresh-status-buffer()
  (interactive)
  (let ((buf (magit-find-current-status-buffer)))
    (when (and buf (buffer-is-visible buf))
      (with-current-buffer buf
        (magit-refresh)))))

(add-hook 'after-save-hook 'magit-refresh-status-buffer)

(defun magit-current-section-file ()
  (if (fboundp 'magit-file-at-point)
      (let ((res (magit-file-at-point)))
        (and res (concat default-directory res)))
    (if (fboundp 'magit-current-section-string)
        (let ((section (magit-current-section-string)))
          (and section (file-exists-p section) section)))))

(defun magit-current-section-sha ()
  (if (fboundp 'magit-branch-or-commit-at-point)
      (magit-branch-or-commit-at-point)
    (let ((string (magit-current-section-string)))
      (cond ((not string) nil)
            ((file-exists-p string) nil)
            ((string-match "[^A-Fa-f0-9]" string) nil)
            (t string)))))

(defun magit-diff-current-section (&optional -w)
  (interactive "P")
  (let ((file (magit-current-section-file)))
    (when file
      (nrdp-git-diff -w file))))

(defun nrdp-magit-log-show-diff (&optional -w)
  (interactive "P")
  (let ((file (magit-current-section-file)))
    (when file
      (nrdp-git-diff -w file))))

(defun nrdp-git-stash-buffer ()
  (interactive)
  (let ((files (nrdp-git-magit-current-things-filtered 'file)))
    (when (and (not files) (buffer-file-name))
      (setq files (list (buffer-file-name))))
    (if files
        (apply #'magit-run-git-async "fstash" "--quiet" files)
      (message "Nothing to fstash"))))

(defun magit-log-current-section (&optional prefix)
  (interactive "P")
  (let ((file (magit-current-section-file)))
    (if file
        (nrdp-git-magit-file-log prefix file)
      (call-interactively 'magit-log-popup))))

(defun magit-run-on-multiple-commits (commands &optional commit)
  (let ((args (cond (commit (list commit))
                    ((nrdp-git-magit-current-things-filtered 'commit))
                    (t
                     (let ((val (read-from-minibuffer "Sha (default HEAD): " nil nil nil "HEAD")))
                       (cond ((string= "" val) (list "HEAD"))
                             (t (list val))))))))
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
  (magit-run-on-multiple-commits (if noresolve
                                     (list "jira" "--no-interactive" "--comment")
                                   (list "jira" "--resolve" "--no-interactive" "--comment")) commit))

(defun magit-submit (&optional commit pr args)
  (interactive)
  (let ((prev (getenv "GIT_POST_SUBMIT_FLAGS")))
    (setenv "GIT_POST_SUBMIT_FLAGS" (concat "--no-interactive --resolve"))
    (setq args (append args (magit-push-arguments)))
    (when pr
      (push (read-from-minibuffer "Name: ") args)
      (push "--pull-request" args))
    (push "submit" args)
    (if (member "-a" args)
        (magit-run-git-async args)
      (magit-run-on-multiple-commits args commit))
    (setenv "GIT_POST_SUBMIT_FLAGS" prev)))

(defun magit-submit-pull-request (&optional commit)
  (interactive)
  (magit-submit commit t))

(defun magit-ignore-commit (&optional commit)
  (interactive)
  (magit-run-on-multiple-commits "ignore-commit" commit))

(defun nrdp-git-ediff-file (&optional buffer)
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (let ((default-directory (nrdp-git-dir-for-file buffer)))
    (ediff-buffers buffer (progn (nrdp-git-show-head (buffer-file-name buffer)) (current-buffer)))))

;; ================================================================================
;; git-jira
;; ================================================================================

(defun git-jira (&optional commit)
  (interactive)
  (unless commit
    (setq commit (let ((result (completing-read "Jira commit: " (split-string (shell-command-to-string "git log --pretty=\"%h %s\" -n 10") "\n"))))
                   (and result (string-match "^\\([^ ]+\\)" result) (match-string 1 result)))))
  (if commit
      (call-process "git-jira" nil nil nil "--resolve" "--no-interactive" commit)))
(provide 'nrdp-git)
