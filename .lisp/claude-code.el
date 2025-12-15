(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :config
  ;;(setq claude-code-ide-window-side 'bottom claude-code-ide-window-height 20)
  (setq claude-code-ide-enable-mcp-server t)
  (setq claude-code-ide-focus-on-open t)
  (claude-code-ide-emacs-tools-setup)
  (claude-code-ide-mcp-server-ensure-server)
  ;; tool: get current selection
  (defun claude-code-ide-get-selection ()
    "Return the currently selected text in the active buffer."
    (with-current-buffer (current-buffer)
      (if (use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end))
        "No text is currently selected.")))
  (claude-code-ide-make-tool
   :function #'claude-code-ide-get-selection
   :name "get-selection"
   :description "Return the currently selected text in the active Emacs buffer."
   :args '())
  ;; tool: get current file and line number
  (defun claude-code-ide-get-file-location ()
    "Return the current file path and line number in the active buffer."
    (let ((buf (current-buffer)))
      (if buf
          (with-current-buffer buf
            (let ((file (buffer-file-name))
                  (line (line-number-at-pos)))
              (if file
                  (format "%s:%d" file line)
                (format "Buffer '%s' is not visiting a file (line %d)"
                        (buffer-name) line))))
        "No active buffer found.")))
  (claude-code-ide-make-tool
   :function #'claude-code-ide-get-file-location
   :name "get-file-location"
   :description "Return the current file path and line number in the active Emacs buffer."
   :args '())
  ;; tool: get current buffer contents (including unsaved changes)
  (defun claude-code-ide-get-buffer-contents (&optional start-line end-line)
    "Return the contents of the current buffer.
Optionally limit to START-LINE to END-LINE (1-indexed, inclusive)."
    (with-current-buffer (current-buffer)
      (let* ((file (or (buffer-file-name) (buffer-name)))
             (total-lines (count-lines (point-min) (point-max)))
             (start (max 1 (or start-line 1)))
             (end (min total-lines (or end-line total-lines))))
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- start))
          (let ((beg (point)))
            (goto-char (point-min))
            (forward-line end)
            (format "File: %s\nLines %d-%d of %d:\n%s"
                    file start end total-lines
                    (buffer-substring-no-properties beg (point))))))))
  (claude-code-ide-make-tool
   :function #'claude-code-ide-get-buffer-contents
   :name "get-buffer-contents"
   :description "Return the contents of the current Emacs buffer (unsaved changes included). Optionally specify start_line and end_line (1-indexed) to limit the range."
   :args '((:name "start_line" :type "integer" :description "Starting line number (1-indexed, optional)")
           (:name "end_line" :type "integer" :description "Ending line number (1-indexed, inclusive, optional)")))
  ;; get compilation buffer contents
  (defun claude-code-ide-get-compilation-buffer (&optional start-line end-line)
    "Return the contents of the *compilation* buffer.
Optionally limit to START-LINE to END-LINE (1-indexed, inclusive)."
    (let ((buf (get-buffer "*compilation*")))
      (if buf
          (with-current-buffer buf
            (let* ((total-lines (count-lines (point-min) (point-max)))
                   (start (max 1 (or start-line 1)))
                   (end (min total-lines (or end-line total-lines))))
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- start))
                (let ((beg (point)))
                  (goto-char (point-min))
                  (forward-line end)
                  (format "Compilation buffer\nLines %d-%d of %d:\n%s"
                          start end total-lines
                          (buffer-substring-no-properties beg (point)))))))
        "No *compilation* buffer exists.")))
  (claude-code-ide-make-tool
   :function #'claude-code-ide-get-compilation-buffer
   :name "get-compilation-buffer"
   :description "Return the contents of the *compilation* buffer. Optionally specify start_line and end_line (1-indexed) to limit the range."
   :args '((:name "start_line" :type "integer" :description "Starting line number (1-indexed, optional)")
           (:name "end_line" :type "integer" :description "Ending line number (1-indexed, inclusive, optional)")))
  (provide 'nrdp-scripts-claude)
  )
