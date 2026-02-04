;;; agb-git-blame.el --- Interactive git blame mode  -*- lexical-binding: t; -*-

(require 'diff-mode)
(require 'cl-lib)
(require 'hl-line)

;; Faces for syntax highlighting
(defface agb-git-blame-hash-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for commit hashes in git blame output."
  :group 'agb-git-blame)

(defface agb-git-blame-author-face
  '((t :inherit font-lock-function-name-face))
  "Face for author names in git blame output."
  :group 'agb-git-blame)

(defface agb-git-blame-date-face
  '((t :inherit font-lock-comment-face))
  "Face for dates in git blame output."
  :group 'agb-git-blame)

(defface agb-git-blame-line-number-face
  '((t :inherit line-number))
  "Face for line numbers in git blame output."
  :group 'agb-git-blame)

(defface agb-git-blame-boundary-face
  '((t :inherit font-lock-warning-face))
  "Face for boundary commits (^) in git blame output."
  :group 'agb-git-blame)

(defface agb-git-blame-current-line-hash-face
  '((t :foreground "green" :weight bold))
  "Face for the commit hash on the current line."
  :group 'agb-git-blame)

(defvar-local agb-git-blame--current-hash-overlay nil
  "Overlay used to highlight the hash on the current line.")

(defvar agb-git-blame-font-lock-keywords
  `(;; Boundary commit (starts with ^)
    ("^\\(\\^[0-9a-f]+\\)" 1 'agb-git-blame-boundary-face)
    ;; Regular commit hash at start of line
    ("^\\([0-9a-f]\\{7,40\\}\\)" 1 'agb-git-blame-hash-face)
    ;; Author name (between hash and date, inside parens)
    ("^[0-9a-f^]+ (\\([^0-9]+\\)" 1 'agb-git-blame-author-face)
    ;; Date (various formats)
    ("\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9:]+\\( [+-][0-9]+\\)?\\)" 1 'agb-git-blame-date-face)
    ;; Relative date
    ("\\([0-9]+ \\(seconds?\\|minutes?\\|hours?\\|days?\\|weeks?\\|months?\\|years?\\) ago\\)" 1 'agb-git-blame-date-face)
    ;; Line number before the closing paren
    ("\\s-+\\([0-9]+\\))\\s-" 1 'agb-git-blame-line-number-face))
  "Font-lock keywords for agb-git-blame-mode.")

(defvar agb-git-blame-mode-hook nil
  "Hook run after entering agb-git-blame-mode.")
(defvar agb-git-blame-last-temp-buffer nil
  "Last temporary buffer created for showing diffs or revisions.")
(defvar-local agb-git-blame-last-file nil
  "The file being blamed in this buffer.")
(defvar-local agb-git-blame-commit-chain nil
  "Stack of commits visited, for navigation with `n'.")
(defvar agb-git-blame-last-blame-buffer nil
  "Last blame buffer created, for reuse when `agb-git-blame-reuse-buffers' is t.")
(defvar agb-git-blame-showing-smaller nil
  "Whether to use short blame format (-s flag).")
(defcustom agb-git-blame-date-format "format:%Y-%m-%d %H:%M"
  "Date format for git blame output.
Common values: \"relative\", \"local\", \"iso\", \"short\", \"human\".
Can also be a strftime format like \"format:%Y-%m-%d %H:%M\"."
  :group 'agb-git-blame
  :type '(choice (const :tag "ISO-like (2025-01-31 14:30)" "format:%Y-%m-%d %H:%M")
                 (const :tag "Month name (Jan 31 2025 14:30)" "format:%b %d %Y %H:%M")
                 (const :tag "Relative (e.g., 2 hours ago)" "relative")
                 (const :tag "ISO 8601" "iso")
                 (const :tag "Short (YYYY-MM-DD)" "short")
                 (string :tag "Custom format")))
(defcustom agb-git-blame-reuse-buffers
  t
  "Whether to reuse temp buffers for agb-git-blame."
  :group 'agb-git-blame
  :type 'boolean)

(defcustom agb-git-blame-follow-renames
  nil
  "Whether to follow file renames when blaming.
When non-nil, uses git blame -C -C -C to detect copies and renames.
Warning: This can be very slow on large repositories."
  :group 'agb-git-blame
  :type 'boolean)

(defcustom agb-git-blame-verbose nil
  "Controls logging verbosity for agb-git-blame.
nil means no logging.
t means log to *Messages* buffer via `message'.
A string means append to the file at that path."
  :group 'agb-git-blame
  :type '(choice (const :tag "No logging" nil)
                 (const :tag "Log to *Messages*" t)
                 (string :tag "Log to file")))

(defun agb-git-blame--log (format-string &rest args)
  "Log a message according to `agb-git-blame-verbose'.
FORMAT-STRING and ARGS are passed to `format'."
  (when agb-git-blame-verbose
    (let ((msg (apply #'format format-string args)))
      (if (stringp agb-git-blame-verbose)
          (with-temp-buffer
            (insert (format-time-string "[%Y-%m-%d %H:%M:%S] ")
                    msg "\n")
            (append-to-file (point-min) (point-max) agb-git-blame-verbose))
        (message "%s" msg)))))

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
(define-key agb-git-blame-mode-map (kbd "t") (function agb-git-blame-cycle-date-format))
(define-key agb-git-blame-mode-map (kbd "c") (function agb-git-blame-toggle-follow-renames))
(define-key agb-git-blame-mode-map (kbd "o") (function agb-git-blame-show-revision))
(define-key agb-git-blame-mode-map (kbd "f") (function agb-git-blame-show-revision))
(define-key agb-git-blame-mode-map (kbd "#") (function agb-git-blame-show-revision))
(define-key agb-git-blame-mode-map (kbd "<RET>") (function agb-git-blame-show-diff))
(define-key agb-git-blame-mode-map (kbd "<ENTER>") (function agb-git-blame-show-diff))

(define-derived-mode agb-git-blame-mode fundamental-mode "agb-git-blame"
  "Major mode for viewing git blame output."
  (setq font-lock-defaults '(agb-git-blame-font-lock-keywords))
  (font-lock-mode 1)
  (hl-line-mode 1)
  (use-local-map agb-git-blame-mode-map)
  (add-hook 'post-command-hook 'agb-git-blame-post-command-hook nil t)
  (run-hooks 'agb-git-blame-mode-hook)
  (setq buffer-read-only t))

(defun agb-git-blame-post-command-hook ()
  (when (eq major-mode 'agb-git-blame-mode)
    ;; Update current line hash highlight
    (save-excursion
      (beginning-of-line)
      (if (looking-at "\\([0-9a-f^]\\{7,40\\}\\)")
          (let ((start (match-beginning 1))
                (end (match-end 1)))
            (unless agb-git-blame--current-hash-overlay
              (setq agb-git-blame--current-hash-overlay (make-overlay start end))
              (overlay-put agb-git-blame--current-hash-overlay 'face 'agb-git-blame-current-line-hash-face))
            (move-overlay agb-git-blame--current-hash-overlay start end))
        (when agb-git-blame--current-hash-overlay
          (delete-overlay agb-git-blame--current-hash-overlay)
          (setq agb-git-blame--current-hash-overlay nil))))
    ;; Sync with magit-log if visible
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
                           (skip-chars-forward "A-Fa-f0-9")
                           (setq commit-length (- (point) (line-beginning-position)))))))
            (select-window (get-buffer-window buf))
            (goto-char (point-min))
            (when (re-search-forward (concat "^" (substring blame-commit 0 commit-length) " ") nil t)
              (goto-char (line-beginning-position)))
            (select-window cur)
            (setq wlist nil)))
        (setq wlist (cdr wlist))))))

;; Hook is added/removed by the mode itself

;;;###autoload
(defun agb-git-blame (&optional revision file target-line)
  "Show git blame for FILE at REVISION, jumping to TARGET-LINE.
If called interactively with a prefix argument, prompt for REVISION.
FILE defaults to the current buffer's file or blame filename.
TARGET-LINE defaults to the current line number."
  (interactive "P")
  (let* ((source-buf (current-buffer))
         (blame-file (cond (file)
                           ((buffer-file-name))
                           ((agb-git-blame-filename))))
         (buf (get-buffer-create (format "*%s - Blame - %s*" blame-file (or revision "HEAD"))))
         (old-buf (if (eq source-buf agb-git-blame-last-blame-buffer)
                      agb-git-blame-last-blame-buffer
                    source-buf))
         (lineno (or target-line (line-number-at-pos))))
    (unless blame-file
      (error "Can't blame this file"))
    (cond ((null revision) (setq revision "HEAD"))
          ((listp revision) (setq revision (read-from-minibuffer "Committish: ")))
          (t))
    ;; Don't kill old buffer yet - wait until we know blame succeeded
    (setq agb-git-blame-last-blame-buffer buf)
    (with-current-buffer buf
      (when (or (string= revision "HEAD") (not (string= blame-file agb-git-blame-last-file)))
        (setq agb-git-blame-last-file blame-file
              agb-git-blame-commit-chain nil))
      (unless (member revision agb-git-blame-commit-chain)
        (push revision agb-git-blame-commit-chain))
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((args (list "blame" "--no-progress"
                        (concat "--date=" agb-git-blame-date-format))))
        (when agb-git-blame-follow-renames
          (setq args (append args '("-C"))))
        (when agb-git-blame-showing-smaller
          (setq args (append args '("-s"))))
        (setq args (append args (list revision "--" blame-file)))
        (let* ((blame-dir (file-name-directory blame-file))
               ;; Use blame-dir if it exists, otherwise fall back to git root
               (default-directory (if (file-directory-p blame-dir)
                                      blame-dir
                                    (or (agb-git-blame-root-dir blame-dir) default-directory)))
               (process-environment (cons "GIT_PAGER=" process-environment))
               (process (make-process
                         :name "git-blame"
                         :buffer buf
                         :command (cons "git" args)
                         :stderr nil
                         :filter (agb-git-blame--make-filter buf)
                         :sentinel (agb-git-blame--make-sentinel buf old-buf lineno revision blame-file agb-git-blame-commit-chain))))
          (set-process-query-on-exit-flag process nil))))
    (switch-to-buffer buf)))

(defun agb-git-blame--make-filter (buf)
  "Create a process filter that keeps cursor at top while output streams in."
  (lambda (_process output)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (at-top (= (point) (point-min))))
          (save-excursion
            (goto-char (point-max))
            (insert output))
          (when at-top
            (goto-char (point-min))))))))

(defun agb-git-blame--date-format-fixed-width-p ()
  "Return t if current date format produces fixed-width output."
  (or (string-prefix-p "format:" agb-git-blame-date-format)
      (member agb-git-blame-date-format '("short" "iso" "iso-strict"))))

(defun agb-git-blame--align-output ()
  "Align blame output so the closing paren of metadata is at the same column.
Skips alignment if using a fixed-width date format."
  (unless (agb-git-blame--date-format-fixed-width-p)
    (goto-char (point-min))
    (let ((max-col 0))
      ;; First pass: find max column of ") "
      (while (re-search-forward ") " nil t)
        (setq max-col (max max-col (- (match-beginning 0) (line-beginning-position))))
        (forward-line 1)
        (beginning-of-line))
      ;; Second pass: pad each line to align
      (goto-char (point-min))
      (while (re-search-forward "\\() \\)" nil t)
        (let* ((current-col (- (match-beginning 0) (line-beginning-position)))
               (padding (- max-col current-col)))
          (when (> padding 0)
            (goto-char (match-beginning 0))
            (insert (make-string padding ? )))
          (forward-line 1)
          (beginning-of-line))))))

(defun agb-git-blame--find-old-name (file revision)
  "Find the previous name of FILE before REVISION.
Returns the relative path (to git root), or nil if not found."
  (let ((default-directory (or (agb-git-blame-root-dir (file-name-directory file))
                               default-directory))
        (relative-file (file-name-nondirectory file)))
    ;; Search for renames in the history leading up to this revision
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil
                                 "log" "--diff-filter=R" "-M"
                                 "--name-status" "-1" "--format="
                                 revision "--" (concat "*/" relative-file)))
        (goto-char (point-min))
        ;; Parse "R100\told-name\tnew-name" format
        (when (re-search-forward "^R[0-9]+\t\\([^\t\n]+\\)\t" nil t)
          (match-string 1))))))

(defun agb-git-blame--find-new-name (file)
  "Find the new name of FILE after it was renamed.
Returns the relative path (to git root), or nil if not found."
  (let ((default-directory (or (file-name-directory file) default-directory)))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil
                                 "log" "--diff-filter=R"
                                 "--name-status" "-1" "--format="
                                 "--" file))
        (goto-char (point-min))
        ;; Parse "R100\told-name\tnew-name" format - we want the new name
        (when (re-search-forward "^R[0-9]+\t[^\t\n]+\t\\([^\t\n]+\\)" nil t)
          (match-string 1))))))

(defun agb-git-blame--find-file-at-revision (file revision)
  "Find what FILE was called at REVISION, following renames.
Returns the absolute path, or nil if not found."
  (let* ((default-directory (or (file-name-directory file) default-directory))
         (gitroot (agb-git-blame-root-dir))
         ;; First try: find old name (for backward navigation)
         (old-name (agb-git-blame--find-old-name file revision))
         ;; Second try: find new name (for forward navigation)
         (new-name (unless old-name (agb-git-blame--find-new-name file))))
    (cond
     ;; Check if old name exists at target revision
     (old-name
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil
                                   "cat-file" "-e"
                                   (concat revision ":" old-name)))
          (concat gitroot old-name))))
     ;; Check if new name exists at target revision
     (new-name
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil
                                   "cat-file" "-e"
                                   (concat revision ":" new-name)))
          (concat gitroot new-name)))))))

(defun agb-git-blame--make-sentinel (buf old-buf lineno revision file commit-chain)
  "Create a process sentinel for git blame in BUF.
OLD-BUF is the previous blame buffer to restore on failure.
LINENO is the target line, REVISION the commit spec, FILE the file path.
COMMIT-CHAIN is the navigation history."
  (lambda (_process _event)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (goto-char (point-min))
        (if (looking-at "fatal")
            ;; Blame failed - try to follow rename, or go back to old buffer
            (let ((renamed-file (when agb-git-blame-follow-renames
                                  (agb-git-blame--find-file-at-revision file revision))))
              (if renamed-file
                  ;; Found renamed file - retry with the renamed path
                  (progn
                    (agb-git-blame--log "Following rename: %s -> %s (revision: %s)"
                                        file renamed-file revision)
                    ;; Restore old-buf first so recursive call has correct fallback
                    (when (and old-buf (buffer-live-p old-buf))
                      (setq agb-git-blame-last-blame-buffer old-buf))
                    (kill-buffer buf)
                    (agb-git-blame revision renamed-file lineno))
                ;; No rename found - restore old buffer and show friendly message
                (let ((error-text (buffer-substring-no-properties (point-min) (min (point-max) (+ (point-min) 200)))))
                  (agb-git-blame--log "No rename found for %s at %s. Fatal: %s" file revision error-text)
                  (kill-buffer buf)
                  (when (and old-buf (buffer-live-p old-buf) (not (eq buf old-buf)))
                    (setq agb-git-blame-last-blame-buffer old-buf)
                    (switch-to-buffer old-buf))
                  ;; Show user-friendly message
                  (if (string-match-p "no such path" error-text)
                      (message "No earlier history for this file at %s" revision)
                    (message "Git blame failed: %s" (string-trim error-text)))))))
          ;; Success - clean up old blame buffer if reusing (but not file buffers)
          (when (and agb-git-blame-reuse-buffers
                     old-buf
                     (buffer-live-p old-buf)
                     (not (eq buf old-buf))
                     (with-current-buffer old-buf
                       (eq major-mode 'agb-git-blame-mode)))
            (kill-buffer old-buf))
          ;; Strip redundant line number column for cleaner display
          ;; Replace " +LINENUM) " with ") "
          (goto-char (point-min))
          (while (re-search-forward " +[0-9]+) " nil t)
            (replace-match ") " nil nil))
          ;; Strip filename shown by -C flag (appears between hash and author)
          ;; Store it as a text property for later use by agb-git-blame-current-file
          ;; Format: "hash filename    (author" -> "hash (author"
          (goto-char (point-min))
          (while (re-search-forward "^\\([0-9a-f^]\\{7,40\\}\\) \\([^ ]+\\) +(" nil t)
            (let ((filename (match-string 2))
                  (line-start (line-beginning-position)))
              (replace-match "\\1 (" nil nil)
              (put-text-property line-start (1+ line-start) 'agb-git-blame-file filename)))
          ;; Align the closing paren of metadata section
          (agb-git-blame--align-output)
          (goto-char (point-min))
          (agb-git-blame-mode)
          ;; Set buffer-local vars AFTER mode activation (mode kills local vars)
          (setq agb-git-blame-last-file file)
          (setq agb-git-blame-commit-chain commit-chain)
          (setq buffer-read-only t)
          (goto-char (point-min))
          (when (> (count-lines (point-min) (point-max)) lineno)
            (forward-line (1- lineno)))
          (agb-git-blame--log "Git blame complete.")))))

(defun agb-git-reblame-for-revision (&optional suffix)
  "Reblame the current file at the commit on this line, with optional SUFFIX.
SUFFIX is typically \"~\" or \"^\" to get the parent commit."
  (interactive)
  (when-let ((commit (agb-git-blame-current-commit)))
    (agb-git-blame (concat commit suffix) (agb-git-blame-current-file) (line-number-at-pos))))

(defun agb-git-reblame-for-previous-revision-~ ()
  "Reblame at the parent commit using ~ suffix (first parent)."
  (interactive)
  (agb-git-reblame-for-revision "~"))

(defun agb-git-reblame-for-previous-revision-^ ()
  "Reblame at the parent commit using ^ suffix (first parent)."
  (interactive)
  (agb-git-reblame-for-revision "^"))

(defun agb-git-reblame-pop ()
  "Go back to the previous commit in the blame navigation history."
  (interactive)
  (when (and (agb-git-blame-filename)
             (> (length agb-git-blame-commit-chain) 1))
    (setq agb-git-blame-commit-chain (cdr agb-git-blame-commit-chain))
    (agb-git-blame (car agb-git-blame-commit-chain) (agb-git-blame-current-file) (line-number-at-pos))))

(defun agb-git-blame-filename ()
  "Extract the filename from a blame buffer name."
  (when (string-match "\\*\\(.*\\) - Blame - [^*]+\\*" (buffer-name))
    (match-string 1 (buffer-name))))

(defun agb-git-blame-current-commit ()
  "Return the commit hash at the beginning of the current blame line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\([0-9a-f]\\{7,40\\}\\)[ )]")
      (match-string 1))))

(defun agb-git-blame-current-file ()
  "Return the full path of the file shown in the current blame line.
Checks for a stored text property (from -C flag output),
otherwise assumes it's the current file being blamed."
  (let ((stored-file (get-text-property (line-beginning-position) 'agb-git-blame-file)))
    (if stored-file
        (concat (agb-git-blame-root-dir) stored-file)
      agb-git-blame-last-file)))

(defun agb-git-blame-buffer-visible (buffer)
  "Return the window displaying BUFFER, or nil if not visible."
  (get-buffer-window buffer))

(defun agb-git-blame-show-diff (&optional otherwindow)
  "Show the diff for the commit on the current line.
With OTHERWINDOW as 1, scroll forward in the other window if visible.
With OTHERWINDOW as -1, scroll backward in the other window if visible.
With any other non-nil OTHERWINDOW, show in other window."
  (interactive "P")
  (when-let ((commit (agb-git-blame-current-commit)))
    (let* ((bufname (format "*%s - %s*" (agb-git-blame-filename) commit))
           (buffer (get-buffer bufname))
           (visible (and buffer (agb-git-blame-buffer-visible buffer))))
      (if buffer
          (if otherwindow
              (if visible
                  (if (eql otherwindow 1)
                      (scroll-other-window)
                    (scroll-other-window-down nil))
                (when (= 1 (length (window-list)))
                  (split-window))
                (switch-to-buffer-other-window buffer)
                (other-window 1))
            (switch-to-buffer buffer))
        (when (and agb-git-blame-reuse-buffers
                   (buffer-live-p agb-git-blame-last-temp-buffer))
          (kill-buffer agb-git-blame-last-temp-buffer))
        (if (not otherwindow)
            (switch-to-buffer (get-buffer-create bufname))
          (when (= 1 (length (window-list)))
            (split-window-vertically))
          (switch-to-buffer-other-window (get-buffer-create bufname))
          (other-window 1))
        (setq agb-git-blame-last-temp-buffer (current-buffer))
        (erase-buffer)
        (call-process "git" nil (current-buffer) nil "show" commit)
        (goto-char (point-min))
        (diff-mode)
        (setq buffer-read-only t)))))

(defun agb-git-blame-show-diff-other-window ()
  "Show diff in other window, or scroll forward if already visible."
  (interactive)
  (agb-git-blame-show-diff 1))

(defun agb-git-blame-show-diff-other-window-back ()
  "Show diff in other window, or scroll backward if already visible."
  (interactive)
  (agb-git-blame-show-diff -1))


(defvar agb-git-blame-show-revision-keymap (make-sparse-keymap))
(define-key agb-git-blame-show-revision-keymap (kbd "q") 'bury-buffer)

(defun agb-git-blame-root-dir (&optional directory)
  "Find the git root directory containing DIRECTORY.
Returns the path with a trailing slash, or nil if not in a git repository."
  (unless directory (setq directory default-directory))
  (let ((check-dir (if (equal (substring directory -1) "/")
                       directory
                     (concat directory "/"))))
    (while (not (or (string-equal check-dir "/")
                    (file-exists-p (concat check-dir ".git"))))
      (setq check-dir (substring check-dir 0 (string-match "[^/]*/?$" check-dir))))
    (unless (string-equal check-dir "/") check-dir)))


(defun agb-git-blame-show-revision ()
  "Show the file content at the commit on the current line."
  (interactive)
  (let ((commit (agb-git-blame-current-commit))
        (gitroot (agb-git-blame-root-dir)))
    (when (and commit gitroot)
      (let* ((fn (agb-git-blame-filename))
             (bufname (format "*%s@%s*" fn commit)))
        (when (get-buffer bufname)
          (kill-buffer bufname))
        (when (and agb-git-blame-reuse-buffers
                   (buffer-live-p agb-git-blame-last-temp-buffer))
          (kill-buffer agb-git-blame-last-temp-buffer))
        (switch-to-buffer (get-buffer-create bufname))
        (setq agb-git-blame-last-temp-buffer (current-buffer))
        (let ((gitfn fn))
          (when (and (>= (length gitfn) (length gitroot))
                     (string= gitroot (substring gitfn 0 (length gitroot))))
            (setq gitfn (substring gitfn (length gitroot))))
          (call-process "git" nil (current-buffer) nil "show" (concat commit ":" gitfn)))
        (setq buffer-file-name fn)
        (set-auto-mode)
        (use-local-map agb-git-blame-show-revision-keymap)
        (setq buffer-file-name nil)
        (goto-char (point-min))
        (font-lock-ensure)
        (setq buffer-read-only t)))))

(defun agb-git-blame-toggle-smaller ()
  "Toggle short blame format (-s flag) and refresh the blame view."
  (interactive)
  (setq agb-git-blame-showing-smaller (not agb-git-blame-showing-smaller))
  (when (and (agb-git-blame-filename)
             (>= (length agb-git-blame-commit-chain) 1))
    (agb-git-blame (car agb-git-blame-commit-chain) (agb-git-blame-current-file) (line-number-at-pos))))

(defvar agb-git-blame-date-format-cycle '("format:%Y-%m-%d %H:%M" "format:%b %d %Y %H:%M" "relative" "short")
  "List of date formats to cycle through with `agb-git-blame-cycle-date-format'.")

(defun agb-git-blame-cycle-date-format ()
  "Cycle through date formats and refresh the blame view."
  (interactive)
  (let* ((current-pos (cl-position agb-git-blame-date-format agb-git-blame-date-format-cycle :test #'string=))
         (next-pos (if current-pos
                       (mod (1+ current-pos) (length agb-git-blame-date-format-cycle))
                     0))
         (file agb-git-blame-last-file)
         (commit (car agb-git-blame-commit-chain))
         (line (line-number-at-pos)))
    (setq agb-git-blame-date-format (nth next-pos agb-git-blame-date-format-cycle))
    (message "Date format: %s (file: %s)" agb-git-blame-date-format file)
    (when file
      (agb-git-blame (or commit "HEAD") file line))))

(defun agb-git-blame-toggle-follow-renames ()
  "Toggle following file renames (-C flag) and refresh the blame view."
  (interactive)
  (setq agb-git-blame-follow-renames (not agb-git-blame-follow-renames))
  (message "Follow renames: %s" (if agb-git-blame-follow-renames "enabled" "disabled"))
  (when (and (agb-git-blame-filename)
             (>= (length agb-git-blame-commit-chain) 1))
    (agb-git-blame (car agb-git-blame-commit-chain) (agb-git-blame-current-file) (line-number-at-pos))))


(defun agb-git-blame-from-diff-get-line-number (for-previous-commit)
  "Get the line number in the file corresponding to current position in diff.
If FOR-PREVIOUS-COMMIT is non-nil, calculate line number for old version.
Returns the line number or nil if not found."
  (save-excursion
    (let ((current-line (line-number-at-pos))
          (line-num nil)
          (hunk-start-old nil)
          (hunk-start-new nil))
      ;; Find the hunk header backwards
      (when (re-search-backward "^@@ -\\([0-9]+\\),?[0-9]* \\+\\([0-9]+\\),?[0-9]* @@" nil t)
        (setq hunk-start-old (string-to-number (match-string 1)))
        (setq hunk-start-new (string-to-number (match-string 2)))
        (setq line-num (if for-previous-commit hunk-start-old hunk-start-new))
        ;; Count lines from hunk start to current position
        (forward-line 1)
        (while (< (line-number-at-pos) current-line)
          (let ((line-type (char-after)))
            (cond ((eq line-type ?+)
                   ;; Added lines only count in new version
                   (unless for-previous-commit
                     (cl-incf line-num)))
                  ((eq line-type ?-)
                   ;; Deleted lines only count in old version
                   (when for-previous-commit
                     (cl-incf line-num)))
                  ((eq line-type ? )
                   ;; Context lines count in both versions
                   (cl-incf line-num))))
          (forward-line 1)))
      line-num)))

(defun agb-git-blame-from-diff-get-commit-and-file (for-previous-commit)
  "Extract commit hash and file path from a git diff buffer.
If FOR-PREVIOUS-COMMIT is non-nil, calculate line number for the old version.
Returns a list (commit-hash file-path line-number) or nil if not found."
  (let ((commit-hash
         (save-excursion
           (when (re-search-backward "^commit \\([a-f0-9]\\{40\\}\\)" nil t)
             (match-string 1))))
        (file-path
         (save-excursion
           (when (re-search-backward "^diff --git [^ ]+ b/\\(.+\\)$" nil t)
             (match-string 1))))
        (line-number (agb-git-blame-from-diff-get-line-number for-previous-commit)))
    (cond ((and commit-hash file-path) (list commit-hash file-path line-number))
          ((and (null commit-hash) (null file-path))
           (message "Can't find commit or file")
           nil)
          ((null commit-hash)
           (message "Can't find commit")
           nil)
          ((null file-path)
           (message "Can't find file")
           nil))))

(defun agb-git-blame-from-diff-with-suffix (suffix)
  "Call agb-git-blame from a diff buffer with the given SUFFIX.
SUFFIX is typically \"~\" or \"^\" for parent commits, or \"\" for the commit itself."
  (let ((for-previous-commit (not (string= suffix ""))))
    (when-let ((commit-and-file (agb-git-blame-from-diff-get-commit-and-file for-previous-commit)))
      (cl-destructuring-bind (commit-hash file-path line-number) commit-and-file
        (agb-git-blame (concat commit-hash suffix)
                       (concat (agb-git-blame-root-dir) file-path)
                       line-number)))))

(defun agb-git-blame-from-diff-tilde ()
  "Call agb-git-blame with ~ suffix from diff buffer."
  (interactive)
  (agb-git-blame-from-diff-with-suffix "~"))

(defun agb-git-blame-from-diff-caret ()
  "Call agb-git-blame with ^ suffix from diff buffer."
  (interactive)
  (agb-git-blame-from-diff-with-suffix "^"))

(defun agb-git-blame-from-diff ()
  "Call agb-git-blame with from diff buffer."
  (interactive)
  (agb-git-blame-from-diff-with-suffix ""))

;; Add keybindings to diff-mode
(eval-after-load 'diff-mode
  '(progn
     (define-key diff-mode-map (kbd "~") 'agb-git-blame-from-diff-tilde)
     (define-key diff-mode-map (kbd "^") 'agb-git-blame-from-diff-caret)
     (define-key diff-mode-map (kbd "b") 'agb-git-blame-from-diff)))


(provide 'agb-git-blame)
