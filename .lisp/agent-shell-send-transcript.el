;;; agent-shell-send-transcript.el --- Send previous transcripts into agent-shell -*- lexical-binding: t; -*-

;; Send a previous agent-shell transcript into the current session,
;; allowing any ACP agent (Claude Code, OpenCode, etc.) to continue
;; from a prior conversation regardless of which agent originally
;; produced it.

;;; Code:

(require 'agent-shell)

(defun agent-shell-send-transcript--transcript-dir ()
  "Return the transcript directory for the current session.
Uses `agent-shell-transcript-file-path-function' to derive it."
  (when-let ((path-fn agent-shell-transcript-file-path-function))
    (condition-case nil
        (let ((path (funcall path-fn)))
          (when path
            (file-name-directory path)))
      (error nil))))

(defun agent-shell-send-transcript--matching-cwd-p (file cwd)
  "Return non-nil if transcript FILE has a Working Directory matching CWD."
  (with-temp-buffer
    (insert-file-contents file nil 0 500)
    (goto-char (point-min))
    (when (re-search-forward "^\\*\\*Working Directory:\\*\\*\\s-*\\(.+\\)" nil t)
      (let ((dir (string-trim (match-string 1))))
        (string= (file-name-as-directory (expand-file-name dir))
                 (file-name-as-directory (expand-file-name cwd)))))))

(defun agent-shell-send-transcript--find-files ()
  "Find transcript files whose Working Directory matches the current cwd.
Returns an alist of (LABEL . FILE-PATH) sorted newest first."
  (let* ((cwd (agent-shell-cwd))
         (dir (agent-shell-send-transcript--transcript-dir))
         (files nil))
    (when (and dir (file-directory-p dir))
      (dolist (file (directory-files dir t "\\.md\\'"))
        (when (and (file-regular-p file)
                   (agent-shell-send-transcript--matching-cwd-p file cwd))
          (let* ((mtime (file-attribute-modification-time (file-attributes file)))
                 (first-line (agent-shell-send-transcript--first-user-line file))
                 (label (format "%s  %s"
                                (format-time-string "%F %T" mtime)
                                (truncate-string-to-width (or first-line "") 70))))
            (push (list label file mtime) files)))))
    ;; Sort newest first
    (mapcar (lambda (entry)
              (cons (nth 0 entry) (nth 1 entry)))
            (sort files (lambda (a b)
                          (time-less-p (nth 2 b) (nth 2 a)))))))

(defun agent-shell-send-transcript--first-user-line (file)
  "Extract the first user message from transcript FILE for display."
  (with-temp-buffer
    (insert-file-contents file nil 0 2000)
    (goto-char (point-min))
    (when (re-search-forward "^## User ([^)]+)\n\n\\(.+\\)" nil t)
      (string-trim (match-string 1)))))

;;;###autoload
(defun agent-shell-send-transcript (&optional pick-shell)
  "Send a previous transcript into `agent-shell'.

Shows all transcripts for the current working directory and lets
you pick one.  The transcript content is inserted into the shell
input so you can add your own continuation prompt before submitting.

When PICK-SHELL is non-nil, prompt for which shell buffer to use."
  (interactive)
  (let* ((transcripts (agent-shell-send-transcript--find-files))
         (_ (unless transcripts
              (user-error "No transcripts found")))
         (labels (mapcar #'car transcripts))
         (selection (completing-read "Send transcript: " labels nil t))
         (file-path (cdr (seq-find (lambda (entry)
                                     (string= (car entry) selection))
                                   transcripts)))
         (_ (unless file-path
              (user-error "No transcript selected")))
         (content (with-temp-buffer
                    (insert-file-contents file-path)
                    (buffer-string)))
         (shell-buffer (when pick-shell
                         (completing-read "Send transcript to shell: "
                                          (mapcar #'buffer-name
                                                  (or (agent-shell-buffers)
                                                      (user-error "No shells available")))
                                          nil t))))
    (agent-shell-insert
     :text (format "Here is a transcript of a previous conversation session. Please use it as context and continue from where it left off:\n\n%s\n\n" content)
     :shell-buffer shell-buffer)))

;;;###autoload
(defun agent-shell-send-transcript-to ()
  "Like `agent-shell-send-transcript' but prompt for which shell to use."
  (interactive)
  (agent-shell-send-transcript t))

(provide 'agent-shell-send-transcript)
;;; agent-shell-send-transcript.el ends here
