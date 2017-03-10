(defgroup nrdp-automerge nil "Minor mode for nrdp-automerge" :prefix "nrdp-automerge-" :group 'tools)

(defvar nrdp-automerge-pending-shell-commands nil)
(defvar nrdp-automerge-current-shell-process nil)
(defvar nrdp-automerge-dry nil)

(defcustom nrdp-automerge-tools-path nil
  "Path to nrdp-tools checkout"
  :group 'nrdp-automerge
  :type '(choice (const :tag "Unset" nil) directory))

(defcustom nrdp-automerge-nrdp-repo "./nrdp"
  "Path to nrdp repo."
  :group 'nrdp-automerge
  :type '(choice (const :tag "Unset" nil) directory))

(defcustom nrdp-automerge-branches "./nrdp.branches"
  "Path to nrdp repo."
  :group 'nrdp-automerge
  :type '(choice (const :tag "Unset" nil) file))

(defcustom nrdp-automerge-git-status nil
  "What function to call for git status."
  :group 'nrdp-automerge
  :type 'function)

(defun nrdp-automerge-nrdp-repo-path ()
  (if (string-match "^/" nrdp-automerge-nrdp-repo)
      nrdp-automerge-nrdp-repo
    (concat nrdp-automerge-tools-path "/automerge/" nrdp-automerge-nrdp-repo)))

(defun nrdp-automerge-branches ()
  (if (string-match "^/" nrdp-automerge-branches)
      nrdp-automerge-branches
    (concat nrdp-automerge-tools-path "/automerge/" nrdp-automerge-branches)))

(defconst nrdp-automerge-buffer-name "*nrdp-automerge*")

(defvar nrdp-automerge-mode-map nil)
(setq nrdp-automerge-mode-map (make-sparse-keymap))

(defvar nrdp-automerge-actions nil)
(defun nrdp-automerge-define-action (key description func)
  (define-key nrdp-automerge-mode-map key func)
  (add-to-list 'nrdp-automerge-actions (cons key description) t))

(defun nrdp-automerge-clear ()
  (interactive)
  (with-current-buffer (nrdp-automerge-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (nrdp-automerge-update-header)
    (setq buffer-read-only t)))

(defun nrdp-automerge-toggle-dry (&optional prefix)
  (interactive "P")
  (setq nrdp-automerge-dry (if (integerp prefix)
                               (not (= prefix 0))
                             (not nrdp-automerge-dry)))
  (nrdp-automerge-update-header))

(defun nrdp-automerge-status()
  (let ((ret (list (if (file-exists-p (concat (nrdp-automerge-nrdp-repo-path) "/.git/AUTOMERGE"))
                       "automerge underway"
                     "no automerge underway"))))
    (when nrdp-automerge-dry
      (push "dry" ret))
    (mapconcat 'identity (nreverse ret) ", ")))

(defun nrdp-automerge-update-header ()
  (save-excursion
    (with-current-buffer (nrdp-automerge-buffer)
      (let ((old buffer-read-only))
        (setq buffer-read-only nil)
        (goto-char (point-min))
        (forward-line 1)
        (when (looking-at "--------------")
          (delete-region (point-min) (point-at-eol)))
        (let ((string (mapconcat 'identity (append (mapcar (lambda (action)
                                                             (format "%s: %s" (car action) (cdr action)))
                                                           nrdp-automerge-actions)
                                                   (list (nrdp-automerge-status))) "  ")))
          (insert string "\n" (make-string (length string) ?-) "\n"))))))

(setq nrdp-automerge-actions nil)
(nrdp-automerge-define-action (kbd "m") "Merge" 'nrdp-automerge-merge)
(nrdp-automerge-define-action (kbd "a") "Abort" 'nrdp-automerge-abort)
(nrdp-automerge-define-action (kbd "c") "Continue" 'nrdp-automerge-continue)
(nrdp-automerge-define-action (kbd "s") "git status" 'nrdp-automerge-git-status)
(nrdp-automerge-define-action (kbd "S") "git status inline" 'nrdp-automerge-git-status-inline)
(nrdp-automerge-define-action (kbd "l") "clear" 'nrdp-automerge-clear)
(nrdp-automerge-define-action (kbd "d") "Toggle dry" 'nrdp-automerge-toggle-dry)

(define-derived-mode nrdp-automerge-mode fundamental-mode "nrdp-automerge-mode"
  (nrdp-automerge-clear))

(defun nrdp-automerge (&optional prefix)
  (interactive "P")
  (let ((buf (nrdp-automerge-buffer)))
    (if (and buf prefix)
        (switch-to-buffer buf)
      (when buf
        (kill-buffer buf))
      (setq buf (get-buffer-create nrdp-automerge-buffer-name))
      (switch-to-buffer buf)
      (when nrdp-automerge-current-shell-process
        (kill-process nrdp-automerge-current-shell-process))
      (setq nrdp-automerge-current-shell-process nil
            nrdp-automerge-pending-shell-commands nil)
      (nrdp-automerge-mode))))

(defun nrdp-automerge-buffer ()
  (get-buffer nrdp-automerge-buffer-name))

(defun nrdp-automerge-process-sentinel (process state)
  (when (eq process nrdp-automerge-current-shell-process)
    (let ((status (process-status process)))
      (when (memq status '(exit signal closed failed))
        (nrdp-automerge-update-header)
        (setq nrdp-automerge-current-shell-process nil)
        (nrdp-automerge-run-next-command)))))

(defun nrdp-automerge-process-filter (process output)
  (when (eq process nrdp-automerge-current-shell-process)
    (with-current-buffer (nrdp-automerge-buffer)
      ;; (message "got output")
      (save-excursion
        (goto-char (point-max))
        (setq buffer-read-only nil)
        (insert output)
        (while (< (point) (point-max))
          (when (looking-at "")
            (delete-char 1)
            (insert "\n"))
          (forward-char))
        (setq buffer-read-only t)))))

(defun nrdp-automerge-run-next-command ()
  ;; (message "nrdp-automerge-run-next-command %d" (length nrdp-automerge-pending-shell-commands))
  (when (> (length nrdp-automerge-pending-shell-commands) 0)
    (let ((cmd (car nrdp-automerge-pending-shell-commands)))
      (setq nrdp-automerge-pending-shell-commands (cdr nrdp-automerge-pending-shell-commands))
      (message "nrdp-automerge: %s" (mapconcat 'identity cmd " "))
      (let ((proc (apply #'start-process (format "*nrdp-automerge <%s>*" (mapconcat 'identity cmd " ")) nil (car cmd) (cdr cmd))))
        (setq nrdp-automerge-current-shell-process proc)
        (set-process-sentinel proc 'nrdp-automerge-process-sentinel)
        (set-process-filter proc 'nrdp-automerge-process-filter)))))

(defun nrdp-automerge-push-command (cmd)
  ;; (message "pushed command %s -> %d" (mapconcat 'identity cmd " ") (length nrdp-automerge-pending-shell-commands))
  (setq nrdp-automerge-pending-shell-commands (append nrdp-automerge-pending-shell-commands (list cmd)))
  (unless nrdp-automerge-current-shell-process
    (nrdp-automerge-run-next-command)))

(defun nrdp-automerge-push-git-automerge-command (type)
  (nrdp-automerge-push-command (list (concat nrdp-automerge-tools-path "/automerge/git-automerge")
                                     "--repo" "ssh://git@stash.corp.netflix.com:7999/nrdp/nrdp.git"
                                     "--dir" (nrdp-automerge-nrdp-repo-path)
                                     (nrdp-automerge-branches)
                                     (cond ((eq type 'start) "--merge")
                                           ((eq type 'abort) "--abort")
                                           ((eq type 'continue) "--continue")
                                           (t (error "bad!")))
                                     (if nrdp-automerge-dry "--dry" "--no-dry"))))

(defun nrdp-automerge-git-status-inline ()
  (interactive)
  (nrdp-automerge-push-command (list "git" "-C" (nrdp-automerge-nrdp-repo-path) "status")))

(defun nrdp-automerge-update-repo ()
  (if (executable-find "git-sync")
      (nrdp-automerge-push-command (list "git" "-C" nrdp-automerge-tools-path "sync" "--no-color"))
    (nrdp-automerge-push-command (list "git" "-C" nrdp-automerge-tools-path "pull"))))

(defun nrdp-automerge-abort ()
  (interactive)
  (nrdp-automerge-push-git-automerge-command 'abort)
  (nrdp-automerge-git-status-inline))

(defun nrdp-automerge-merge ()
  (interactive)
  (nrdp-automerge-update-repo)
  (nrdp-automerge-push-git-automerge-command 'start)
  (nrdp-automerge-git-status-inline))

(defun nrdp-automerge-continue ()
  (interactive)
  (nrdp-automerge-push-git-automerge-command 'continue)
  (nrdp-automerge-git-status-inline))

(defun nrdp-automerge-git-status ()
  (interactive)
  (when (functionp nrdp-automerge-git-status)
    (funcall nrdp-automerge-git-status (nrdp-automerge-nrdp-repo-path))))


(provide 'nrdp-automerge)
