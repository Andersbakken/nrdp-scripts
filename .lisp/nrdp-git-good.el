;; new-ass
(define-key magit-file-section-map [C-return] 'magit-diff-visit-file)
(define-key magit-file-section-map "\r" 'magit-diff-visit-file-worktree)
(define-key magit-hunk-section-map [C-return] 'magit-diff-visit-file)
(define-key magit-hunk-section-map "\r" 'magit-diff-visit-file-worktree)

(define-key magit-mode-map (kbd "M-w") 'kill-ring-save)
(defun nrdp-git-magit-buffer-file-name ()
  (and (stringp header-line-format)
       (string-match "Commits in [^ ]+ touching \\([^ ]+\\)" header-line-format)
       (concat default-directory (match-string 1 header-line-format))))

(defun nrdp-git-magit-file-log (&optional bufferorfilename)
  (interactive)
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
            (magit-log-head (list "--follow") (list magit-buffer-file-name))))
      (message "Can't log this buffer"))))

(defun nrdp-git-magit-log ()
  (interactive)
  (magit-log (list (magit-get-current-branch))))

(fset 'magit-toggle-whitespace
      [?D ?- ?w ?\C-\M-l ?\C-x ?1 ?\M-x ?e ?v ?a ?l ?- ?b ?u tab return ?\C-\M-l ?q ?\C-x ?\( ?\C-x ?\( ?D ?- ?w ?g])

(magit-define-popup-action 'magit-pull-popup ?S "Sync" 'nrdp-magit-sync)
(magit-define-popup-action 'magit-push-popup ?S "Submit" 'magit-submit)
(magit-define-popup-action 'magit-push-popup ?A "Submit all" 'magit-submit-all)
(magit-define-popup-action 'magit-push-popup ?J "Jira" 'magit-jira)
(magit-define-popup-action 'magit-push-popup ?R "Jira (Don't resolve)" 'magit-jira-no-resolve)
(magit-define-popup-action 'magit-push-popup ?I "Ignore" 'magit-ignore)
(magit-define-popup-action 'magit-log-popup ?b "Blame" 'magit-blame-for-current-revision)

(defun magit-current-section-string ())

(provide 'nrdp-git-good)
