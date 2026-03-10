;;; agent-shell-file-links.el --- Clickable file:line links in agent-shell -*- lexical-binding: t; -*-

;; Make file:line references in agent-shell output clickable.
;; Clicking (or pressing RET) opens the file at the given line.
;; Relative paths are resolved against the agent-shell project root.

;;; Code:

(require 'agent-shell)

(defun agent-shell-file-links--resolve (file)
  "Resolve FILE to an absolute path.
If FILE is already absolute, return it as-is.
Otherwise expand it relative to `agent-shell-cwd'."
  (if (file-name-absolute-p file)
      file
    (expand-file-name file (agent-shell-cwd))))

(defun agent-shell-file-links--open (file line &optional end-line)
  "Open FILE at LINE in Emacs.
If END-LINE is non-nil, highlight from LINE to END-LINE."
  (let ((resolved (agent-shell-file-links--resolve file)))
    (if (file-exists-p resolved)
        (progn
          (find-file resolved)
          (goto-char (point-min))
          (forward-line (1- line))
          (when end-line
            (let ((beg (line-beginning-position))
                  (end (save-excursion
                         (forward-line (- end-line line))
                         (line-end-position))))
              (set-mark beg)
              (goto-char end)
              (activate-mark))))
      (message "File not found: %s" resolved))))

(defun agent-shell-file-links--make-keymap (file line &optional end-line)
  "Return a keymap that opens FILE at LINE on click or RET.
If END-LINE is non-nil, pass it through to highlight the range."
  (let ((map (make-sparse-keymap))
        (action (lambda () (interactive)
                  (agent-shell-file-links--open file line end-line))))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    map))

(defconst agent-shell-file-links--path-chars
  '(not (any " \t\n:*`\"'[](){}<>|;"))
  "Character class for valid path components, excluding whitespace,
colon, and common markdown/shell punctuation.")

(defconst agent-shell-file-links--re
  (rx-to-string
   `(seq (group (or
                 ;; absolute path
                 (seq "/" (one-or-more ,agent-shell-file-links--path-chars))
                 ;; relative path with slash or leading dot
                 (seq (optional "./")
                      (one-or-more ,agent-shell-file-links--path-chars)
                      "/"
                      (one-or-more ,agent-shell-file-links--path-chars))))
         (optional ":" (group (one-or-more digit))
                   (optional "-" (group (one-or-more digit))))))
  "Regexp matching file, file:line, or file:line-endline references.")

(defun agent-shell-file-links-buttonize (range)
  "Make file references clickable in RANGE.
Handles path, path:line, and path:line-endline forms.
Intended for use in `agent-shell-section-functions'."
  (when-let ((body (alist-get :body range))
             (body-start (alist-get :start body))
             (body-end (alist-get :end body)))
    (save-excursion
      (goto-char body-start)
      (while (re-search-forward agent-shell-file-links--re body-end t)
        (let* ((file (match-string 1))
               (line-str (match-string 2))
               (end-line-str (match-string 3))
               (line (if line-str (string-to-number line-str) 1))
               (end-line (and end-line-str (string-to-number end-line-str)))
               (beg (match-beginning 0))
               (end (match-end 0))
               (resolved (agent-shell-file-links--resolve file)))
          (when (file-exists-p resolved)
            (let ((inhibit-read-only t))
              (add-text-properties
               beg end
               `(keymap ,(agent-shell-file-links--make-keymap file line end-line)
                 mouse-face highlight
                 help-echo ,(concat resolved
                                    (if line-str
                                        (concat ":" line-str
                                                (if end-line-str
                                                    (concat "-" end-line-str)
                                                  ""))
                                      ""))
                 pointer hand)))))))))

(add-hook 'agent-shell-section-functions #'agent-shell-file-links-buttonize)

(provide 'agent-shell-file-links)

;;; agent-shell-file-links.el ends here
