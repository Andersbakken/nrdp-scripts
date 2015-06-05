;;===================
;; Magit stuff
;;===================

(defun magit-cherry-pick (&optional commit)
  (interactive)
  (let ((branches))
    (with-temp-buffer
      (when (eq 0 (call-process "git" nil t nil "branch"))
        (goto-char (point-min))
        (while (not (eobp))
          ;; (message (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          (skip-chars-forward " *")
          (push (buffer-substring-no-properties (point) (point-at-eol)) branches)
          (forward-line 1))))
    (with-temp-buffer
      (when (eq 0 (call-process "git" nil t nil "branch" "-r"))
        (goto-char (point-min))
        (while (not (eobp))
          ;; (message (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          (skip-chars-forward " ")
          (push (buffer-substring-no-properties (point) (point-at-eol)) branches)
          (forward-line 1))))
    ;; (message (combine-and-quote-strings branches))
    (unless commit
      (setq commit (ido-completing-read "Commit: " branches)))
    (if (> (length commit) 0)
        (magit-cherry-pick-commit commit)
      (error "Nothing to cherry-pick"))))

(defun git-gitify-path (file)
  (if (string-match "^/" file)
      (let ((root (magit-get-top-dir (file-name-directory file))))
        (if (string-match (concat "^" root) file)
            (setq file (substring file (length root))))))
  file)

(defun git-grep-prompt ()
  (let* ((default (current-word))
         (prompt (if default
                     (concat "Search for: (default " default ") ")
                   "Search for: "))
         (search (read-from-minibuffer prompt nil nil nil nil default)))
    (if (> (length search) 0)
        search
      (or default ""))))

(defun git-grep (search)
  "git-grep the entire current repo"
  (interactive (list (git-grep-prompt)))
  (grep-find (concat "git --no-pager grep -I -n "
                     (shell-quote-argument search)
                     " -- " (magit-get-top-dir) " ':!*/error.js' ':!*/xboxupsellpage.js' ':!*/boot.js' ':!*min.js'")))

(defun git-config-value (conf)
  (let ((ret (shell-command-to-string (concat "git config " conf))))
    (if (and ret (string-match "\n$" ret))
        (substring ret 0 (1- (length ret)))
      ret)))

(defun git-revert (&optional buffer)
  (interactive)
  (let ((file (buffer-file-name (or buffer (current-buffer)))))
    (if (not file)
        (message "This buffer is not visiting a file")
      (and (y-or-n-p (concat "Are you sure you want to revert " (file-name-nondirectory file)))
           (= (call-process "git" nil nil nil "reset" "--" file) 0)
           (= (call-process "git" nil nil nil "checkout" "HEAD" "--" file) 0)
           (with-current-buffer (or buffer (current-buffer))
             (revert-buffer t t t))))))

(defun git-show-revision (file sha)
  (let ((line (and (string= file (buffer-file-name)) (count-lines 1 (point)))))
    (setq file (git-gitify-path file))
    (let ((dir default-directory)
          (buffer (get-buffer-create (format "%s - %s" file sha))))
      (switch-to-buffer buffer)
      (setq default-directory dir)
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

(defvar git-diff-reuse-diff-buffer nil)
(defun git-diff (&optional -w target no-split-window norestorefocus against)
  (interactive "P")
  (let* ((dir default-directory)
         (window-count (length (window-list)))
         (old (get-buffer-window))
         (args (list (or against "HEAD") "--" (cond ((null target)
                                                     (if (buffer-file-name)
                                                         (buffer-file-name)
                                                       (error "git-diff: Not a file buffer")))
                                                    ((bufferp target)
                                                     (if (buffer-file-name target)
                                                         (buffer-file-name target)
                                                       (error "git-diff: Not a file buffer")))
                                                    ((stringp target) target)
                                                    (target (magit-get-top-dir))
                                                    (t (error "git-diff: What to do here?")))))
         (buffer (get-buffer-create (if (or git-diff-reuse-diff-buffer (not args))
                                        "*git-diff*"
                                      (concat "*git-diff: " (car args) "*")))))
    (when -w
      (push "-w" args))
    (if no-split-window
        (switch-to-buffer buffer)
      (switch-to-buffer-other-window buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir)
    (if (and (= (apply #'call-process "git" nil t nil "diff" args) 0)
             (not (= (point-min) (point-max))))
        (progn
          (goto-char (point-min))
          (insert "$ git diff " (combine-and-quote-strings args) "\n")
          (search-forward-regexp "^@@ ")
          (goto-char (point-at-bol))
          (setq buffer-read-only t)
          (diff-mode)
          (unless norestorefocus
            (select-window old)))
      (message "No differences")
      (kill-buffer (current-buffer))
      (unless no-split-window
        (when (> window-count 1)
          (delete-window))
        (select-window old))
      nil)))

(defun git-diff-other (&optional -w target)
  (interactive "P")
  (git-diff -w target))

(defun git-diff-repo (&optional -w)
  (interactive "P")
  (git-diff -w))

(defun git-diff-directory (&optional -w)
  (interactive "P")
  (git-diff -w default-directory))

(defun git-show-head (&optional file)
  (interactive)
  (unless (or file (buffer-file-name))
    (error "Not a real file"))
  (git-show-revision (or file (buffer-file-name)) "HEAD"))

(defun git-show-tracking (&optional file)
  (interactive)
  (unless (or file (buffer-file-name))
    (error "Not a real file"))
  (let ((tracking (magit-get-remote/branch)))
    (if tracking
        (git-show-revision (or file (buffer-file-name)) tracking)
      (message "No tracking branch for branch"))))

(defun magit-log-mode-current-file ()
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "Commits for file \\(.*\\) in [^ ]+$")
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))
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

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

;; Prevent *magit-process* from stealing focus when it pops up.
(defadvice pop-to-buffer (around return-focus activate)
  (let ((prev (selected-window)))
    ad-do-it
    (when (and prev
               (not (eq prev (selected-window)))
               (string= (buffer-name) magit-process-buffer-name))
      (select-window prev))))

(define-key magit-status-mode-map (kbd "-") 'magit-ediff)
(define-key magit-status-mode-map (kbd "U") 'magit-discard-item)
(define-key magit-status-mode-map (kbd "_") 'magit-diff-smaller-hunks)
(define-key magit-status-mode-map (kbd "=") 'magit-diff-current-section)
(define-key magit-status-mode-map (kbd "l") 'magit-log-current-section)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
(if (fboundp 'magit-show-file-revision)
    (define-key magit-log-mode-map (kbd "#") (function magit-show-file-revision))
  (define-key magit-log-mode-map (kbd "#") (function magit-show-revision-at-current-line)))
(define-key magit-log-mode-map (kbd "@") (function magit-blame-for-current-revision))

(defvar-local magit-hidden-stash-overlay nil)
(defvar-local magit-hide-stashes t)
(defcustom magit-fullscreen-status nil "Whether magit status always becomes fullscreen" :type 'boolean :group 'magit)
(defcustom magit-max-stashes 0 "How many stashes to display" :type 'number :group 'magit)

(defun magit-limit-stashes-hook ()
  (when (> magit-max-stashes 0)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "^Stashes:$" nil t)
        (let ((stashes-beginning (match-beginning 0))
              (buffer-read-only nil))
          (if (and (buffer-local-value 'magit-hide-stashes (current-buffer))
                   (search-forward-regexp (format "^%d: " magit-max-stashes) nil t))
              (let ((start (point-at-bol))
                    (end (or (search-forward-regexp "^$" nil t)
                             (point-max))))
                (setq-local magit-hidden-stash-overlay (make-overlay start end))
                (overlay-put magit-hidden-stash-overlay 'invisible t)))
          (goto-char stashes-beginning)
          (delete-char 8)
          (insert-button "Stashes:" 'action (lambda (x)
                                              (setq magit-hide-stashes (not magit-hide-stashes))
                                              (if magit-hide-stashes
                                                  (magit-limit-stashes-hook)
                                                (when (buffer-local-value 'magit-hidden-stash-overlay (current-buffer))
                                                  (delete-overlay magit-hidden-stash-overlay)
                                                  (setq magit-hidden-stash-overlay nil))))))))))

(add-hook 'magit-refresh-status-hook 'magit-limit-stashes-hook)

(defadvice magit-status (around magit-fullscreen activate)
  (when magit-fullscreen-status
    (window-configuration-to-register :magit-fullscreen))
  ad-do-it
  (when magit-fullscreen-status
    (delete-other-windows)))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (bury-buffer)
  (jump-to-register :magit-fullscreen))

(when magit-fullscreen-status
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

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
      (agb-git-blame sha (file-name-nondirectory file)))))

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

(defun magit-find-current-status-buffer ()
  (let ((topdir (magit-get-top-dir default-directory)))
    (when topdir
      (get-buffer (concat "*magit: " (file-name-nondirectory (directory-file-name topdir)) "*")))))

(defun magit-refresh-status-buffer()
  (interactive)
  (let ((buf (magit-find-current-status-buffer)))
    (when (and buf (buffer-is-visible buf))
      (with-current-buffer buf
        (magit-refresh)))))

(add-hook 'after-save-hook 'magit-refresh-status-buffer)

(defun magit-current-section-string ()
  (let* ((section (magit-current-section))
         (info (and section (magit-section-info section))))
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

(defun magit-diff-current-section (&optional -w)
  (interactive "P")
  (let ((file (magit-current-section-file)))
    (when file
      (git-diff -w file))))

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

(defun magit-ediff-file ()
  (interactive)
  (let ((a (current-buffer))
        (b (progn (git-show-head (buffer-file-name)) (current-buffer))))
    (magit-ediff-buffers b a)))

;; ================================================================================
;; git-jira
;; ================================================================================

(defun git-jira (&optional commit)
  (interactive)
  (unless commit
    (setq commit (magit-read-rev-with-default "Jira commit: ")))
  (if commit
      (call-process "git-jira" nil nil nil "--resolve" "--no-interactive" commit)))
(provide 'nrdp-git)
