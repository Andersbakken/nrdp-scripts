(defun pvm-cd (&optional prefix func)
  (interactive "p")
  (let ((opts nil))
    (with-temp-buffer
      (call-process "pvm" nil t nil "--no-color" "--no-autoupdate" "ls")
      (goto-char (point-min))
      (while (re-search-forward "\\([^ ]+@[0-9]+\\) *-" nil t)
        ;; (message "Got line %s" (buffer-substring-no-properties (point-at-bol) (match-end 1)))
        ;; (message "GOT THING [%s]" (match-string 1))
        (push (match-string 1) opts)
        (forward-char)))
    (if (null opts)
        (error "No pvms found")
      (let ((result (completing-read "PVM: " opts nil t)))
        (when result
          (let ((dir (shell-command-to-string (concat "pvm --no-color --no-autoupdate show --installed --installpath " result))))
            (when dir
              (funcall (or func 'find-file) (if (string-match "\n$" dir) (substring dir 0 -1) dir)))))))))

(provide 'pvm)
