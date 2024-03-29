(defgroup netflix-log nil "Mode for netflix log files." :group 'tools :prefix "netflix-log")

(defface netflix-log-time-face
  '((t :inherit font-lock-constant-face))
  "Face used to display timestamp in NRDP log files"
  :group 'netflix-log)

(defface netflix-log-thread-name-face
  '((t :inherit font-lock-doc-face))
  "Face used to display thread name in NRDP log files"
  :group 'netflix-log)

(defface netflix-log-thread-id-face
  '((t :inherit font-lock-function-name-face))
  "Face used to display thread id in NRDP log files"
  :group 'netflix-log)

(defface netflix-log-area-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to display trace area name in NRDP log files"
  :group 'netflix-log)

(defface netflix-log-trace-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to display trace area name in NRDP log files"
  :group 'netflix-log)

(defface netflix-log-debug-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to display trace area name in NRDP log files"
  :group 'netflix-log)

(defface netflix-log-info-face
  '((t :inherit default))
  "Face used to display trace area name in NRDP log files"
  :group 'netflix-log)

(defface netflix-log-warn-face
  '((t :inherit font-lock-warning-face))
  "Face used to display trace area name in NRDP log files"
  :group 'netflix-log)

(defface netflix-log-error-face
  '((t :inherit font-lock-warning-face))
  "Face used to display trace area name in NRDP log files"
  :group 'netflix-log)

(defface netflix-log-success-face
  '((t :inherit font-lock-warning-face))
  "Face used to display trace area name in NRDP log files"
  :group 'netflix-log)

(defface netflix-log-fatal-face
  '((t :inherit font-lock-warning-face))
  "Face used to display trace area name in NRDP log files"
  :group 'netflix-log)

(defvar netflix-log-time-regexp
  "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\)?[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{3\\}")

(defvar netflix-log-thread-regexp
  "[\\[(]\\(\\([^(]+\\)?:\\)?\\([x[:xdigit:]]+\\)\\()\\|\\]\\)")

(defvar netflix-log-area-regexp
  "\\([A-Z0-9_]+\\)(\\(trace\\|debug\\|info\\|warn\\|error\\|fatal\\|success\\)):")

(defvar netflix-log-font-lock-keywords
  `((,(concat "\\(" netflix-log-time-regexp "\\)"
              " \\(" netflix-log-thread-regexp "\\)"
              " \\(" netflix-log-area-regexp "\\)")
     (1 'netflix-log-time-face t t)
     (5 'netflix-log-thread-name-face t t)
     (6 'netflix-log-thread-id-face t t)
     (9 'netflix-log-area-name-face t t)
     ("(\\(trace\\)): \\(.*\\)" (backward-word 2) nil
      (1 'netflix-log-trace-face)
      (2 'netflix-log-trace-face))
     ("(\\(debug\\)): \\(.*\\)" (backward-word 2) nil
      (1 'netflix-log-debug-face)
      (2 'netflix-log-debug-face))
     ("(\\(info\\)): \\(.*\\)" (backward-word 2) nil
      (1 'netflix-log-info-face)
      (2 'netflix-log-info-face))
     ("(\\(warn\\)): \\(.*\\)" (backward-word 2) nil
      (1 'netflix-log-warn-face t t)
      (2 'netflix-log-warn-face t t))
     ("(\\(error\\)): \\(.*\\)" (backward-word 2) nil
      (1 'netflix-log-error-face t t)
      (2 'netflix-log-error-face t t))
     ("(\\(success\\)): \\(.*\\)" (backward-word 2) nil
      (1 'netflix-log-success-face t t)
      (2 'netflix-log-success-face t t))
     ("(\\(fatal\\)): \\(.*\\)" (backward-word 2) nil
      (1 'netflix-log-fatal-face t t)
      (2 'netflix-log-fatal-face t t)))))

(defvar netflix-log-filtered-areas nil
  "List of trace areas that should be displayed, or nil for all")
(make-variable-buffer-local 'netflix-log-filtered-areas)

(defvar netflix-log-excluded-areas nil
  "List of trace ares that should not be displayed")
(make-variable-buffer-local 'netflix-log-excluded-areas)

(defvar netflix-log-areas '(
  "ASFDEMUX" "BUFFERMANAGER" "CONFIGDATA" "CRLOCSP" "CRYPTO" "CURL_MULTI_THREAD" "DISK_CACHE"
  "DISK_STORE" "DNS_MANAGER" "DNS_WORKER" "DPI" "EVENTCONNECTION" "GIBBON_ANIMATION"
  "GIBBON_DISK_CACHE" "GIBBON_FONTS" "GIBBON_GRAPHICS" "GIBBON_HTTP" "GIBBON_NETWORK"
  "GIBBON_NETWORK_CURL" "GIBBON_NETWORK_HST" "GIBBON_SAMPLE" "GIBBON_SCRIPT" "GIBBON_SURFACE"
  "GIBBON_TEXT" "GIBBON_WIDGET" "HEARTBEAT" "HTTP" "HTTPCONN" "HTTPLIB" "HTTP_SERVICE_THREAD"
  "INSTRUMENTATION" "IP_CONNECTIVITY_MANAGER" "JAVASCRIPT" "LICENSEACQUISITION" "LOG"
  "MANIFESTCACHE" "MDX" "MDXLISTENER" "MDX_MONGOOSE" "MDX_MONGOOSE_REQUEST" "MEDIACONTROL"
  "MEDIALISTENER" "MEDIAPLAYBACK" "MEDIASTARTUP" "MONGOOSE" "MONGOOSE_PROXY"
  "MONGOOSE_REQUEST" "NBP" "NBP_NCCPHANDLER" "NBP_SIGNING" "NCCP" "NCCPLOGGER" "NCCP_AUTH"
  "NCCP_REG" "NETWORK" "NETWORKMANAGER" "NETWORKMANAGER_CURL" "NETWORKMANAGER_HST" "NRDJS"
  "PERFORMANCE" "PERIODIC_WORKER" "PLAYBACK_REPORTER" "PLAYDATA" "RESOURCEMANAGER"
  "RESOURCES" "SECURE_STORE" "SSL" "STARTUP" "STREAMERSLOG" "STREAMINGMANAGER"
  "STREAMINGSTAT" "SUBTITLEMANAGER" "SYSTEM" "TELNET" "THREAD" "THREADPOOL" "TRACE"
  "TRICKPLAYMANAGER" "UI_ENGINE" "UI_SCRIPT" "VARIANT" "WEBSOCKET" "WMDRM" "XML"
  ))

(defun netflix-log-hide-nrdp-log-log ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "Method: nrdp.log.log\\(\\[.*\\]\\)?\\((\\)" nil t)
      (condition-case nil
          (let ((o (make-overlay (+ 1 (match-beginning 2)) (- (scan-sexps (match-beginning 2) 1) 1))))
            (overlay-put o 'invisible 'log-log))
        (error)))))

(defun netflix-log-limit (regexp subexp str-match overval &optional exclude)
  (message "limiting %s to lines that %smatch: %s"
           (symbol-name overval) (if exclude "do not " "") str-match)
  (remove-overlays (point-min) (point-max) 'invisible overval)
  (save-excursion
    (goto-char (point-min))
    (let ((prev-point (point))
          (ex-fn (if exclude 'not 'identity))
          ln-match)
      (while (search-forward-regexp regexp nil t)
        (setq ln-match (match-string subexp))
        (if (or (and (stringp str-match)
                     (apply ex-fn (list (string-match (concat "^" str-match "$") ln-match))))
                (and (listp str-match)
                     (apply ex-fn (list (member ln-match str-match)))))
            (progn
              (if (and prev-point
                       (not (= prev-point (point-at-bol))))
                  (let ((o (make-overlay prev-point (point-at-bol))))
                    (overlay-put o 'invisible overval)
                    (remove-overlays prev-point (point-at-bol) 'invisible 'log-log)))
              (setq prev-point nil))
          (if (not prev-point)
              (setq prev-point (point-at-bol)))))
      (if prev-point
          (let ((o (make-overlay prev-point (point-max))))
            (overlay-put o 'invisible overval)
            (remove-overlays prev-point (point-max) 'invisible 'log-log))))))

(defun netflix-log-current-name (regexp subexp)
  (let ((bol (point-at-bol))
        (eol (point-at-eol))
        point-at-match
        current-name)
    (save-excursion
      ;; first, look backwards for our current name
      (if (search-backward-regexp regexp nil t)
          (setq current-name (match-string subexp)
                point-at-match (point))))
    ;; if we didn't find it, or we found it on a previous line, see if
    ;; we can match it on the current line
    (save-excursion
      (beginning-of-line)
      (if (or (not current-name) (> bol point-at-match))
          (if (search-forward-regexp regexp nil t)
              (if (> eol (point))
                  ;; found it on the current line
                  (setq current-name (match-string subexp))))))
    (substring-no-properties current-name)))

(defun netflix-log-limit-to-thread (&optional arg)
  (interactive "P")
  (if current-prefix-arg
      (progn
        (remove-overlays (point-min) (point-max) 'invisible 'thread)
        (netflix-log-hide-nrdp-log-log))
    (let* ((regexp (concat netflix-log-time-regexp " " netflix-log-thread-regexp " " netflix-log-area-regexp))
           (thread-name (netflix-log-current-name regexp 4)))
      (if thread-name
          (netflix-log-limit regexp 4 thread-name 'thread)))))

(defun netflix-log-remove-traces (&optional arg)
  (interactive "P")
  (if current-prefix-arg
      (progn
        (remove-overlays (point-min) (point-max) 'invisible 'level)
        (netflix-log-hide-nrdp-log-log))
    (let ((regexp (concat netflix-log-time-regexp " " netflix-log-thread-regexp " " netflix-log-area-regexp)))
      (netflix-log-limit regexp 7 "\\(debug\\|info\\|warn\\|error\\|fatal\\|success\\)" 'level))))

(defun netflix-log-current-area ()
  (netflix-log-current-name
   (concat netflix-log-time-regexp " " netflix-log-thread-regexp " " netflix-log-area-regexp)
   6))

(defun netflix-log-limit-to-area (&optional arg)
  (interactive "P")
  (let ((area-name (netflix-log-current-area)))
    (if area-name
        (if current-prefix-arg
            (netflix-log-remove-area-from-limit area-name)
          (netflix-log-add-area-to-limit area-name)))))

(defun netflix-log-add-area-to-limit (area)
  (interactive (list (completing-read (concat "Add area (default: " (netflix-log-current-area) "): ")
                                      netflix-log-areas
                                      nil ; predicate
                                      nil ; require-match
                                      nil ; initial-input
                                      nil ; hist
                                      (netflix-log-current-area))))
  (add-to-list 'netflix-log-filtered-areas area)
  (let ((regexp (concat netflix-log-time-regexp " " netflix-log-thread-regexp " " netflix-log-area-regexp)))
    (netflix-log-limit regexp 6 netflix-log-filtered-areas 'area)))

(defun netflix-log-remove-area-from-limit (area)
  (interactive (list (completing-read "Remove area: " netflix-log-filtered-areas nil t)))
  (setq netflix-log-filtered-areas (remove area netflix-log-filtered-areas))
  (remove-overlays (point-min) (point-max) 'invisible 'area)
  (netflix-log-hide-nrdp-log-log)
  (if netflix-log-filtered-areas
      (let ((regexp (concat netflix-log-time-regexp " " netflix-log-thread-regexp " " netflix-log-area-regexp)))
        (netflix-log-limit regexp 6 netflix-log-filtered-areas 'area))))

(defun netflix-log-clear-area-limit ()
  (interactive)
  (setq netflix-log-filtered-areas nil)
  (setq netflix-log-excluded-areas nil)
  (remove-overlays (point-min) (point-max) 'invisible 'area)
  (netflix-log-hide-nrdp-log-log))

(defun netflix-log-exclude-area (area)
  (interactive (list (completing-read (concat "Exclude area (default: " (netflix-log-current-area) "): ")
                                      netflix-log-areas
                                      nil ; predicate
                                      nil ; require-match
                                      nil ; initial-input
                                      nil ; hist
                                      (netflix-log-current-area))))
  (add-to-list 'netflix-log-excluded-areas area)
  (let ((regexp (concat netflix-log-time-regexp " " netflix-log-thread-regexp " " netflix-log-area-regexp)))
    (netflix-log-limit regexp 6 netflix-log-excluded-areas 'area t)))

(defvar netflix-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'netflix-log-limit-to-thread)
    (define-key map (kbd "C-c C-l") 'netflix-log-remove-traces)
    (define-key map (kbd "C-c C-a") 'netflix-log-limit-to-area)
    (define-key map (kbd "C-c C-i") 'netflix-log-add-area-to-limit)
    (define-key map (kbd "C-c C-e") 'netflix-log-remove-area-from-limit)
    (define-key map (kbd "C-c C-k") 'netflix-log-exclude-area)
    (define-key map (kbd "C-c C-c") 'netflix-log-clear-area-limit)
    map))

(define-derived-mode netflix-log-mode text-mode "Netflix"
  "Major mode for viewing NRDP log files.

\\{netflix-log-mode-map}"
  (if (fboundp 'ansi-color-filter-region)
      (ansi-color-filter-region (point-min) (point-max)))
  (set (make-local-variable 'font-lock-defaults)
       '(netflix-log-font-lock-keywords nil))
  (add-to-invisibility-spec '(thread . nil))
  (add-to-invisibility-spec '(level . nil))
  (add-to-invisibility-spec '(area . nil))
  (add-to-invisibility-spec '(log-log . t))
  (setq global-auto-revert-ignore-buffer t)
  ;; calls to nrdp.log.log() are going to show up twice, once for NBP and once for UI_SCRIPT
  (netflix-log-hide-nrdp-log-log))

(defun netflix-log-is-log ()
  (or (search-forward-regexp (concat netflix-log-time-regexp " "
                                     netflix-log-thread-regexp " "
                                     netflix-log-area-regexp) nil t)
      (search-forward-regexp "Entering AppMain" nil t)))

(add-to-list 'magic-mode-alist '(netflix-log-is-log . netflix-log-mode))

(defun nfltime (idx) (string-to-number (match-string idx)))

(defun netflix-log-time (&optional point)
  (save-excursion
    (if point
        (goto-char point))
    (goto-char (point-at-bol))
    (cond ((looking-at "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) \\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\).\\([0-9][0-9][0-9]\\) ") ;; YYYY-MM-DD hh:mm:ss.zzz
           (+ (float-time (encode-time (nfltime 6) (nfltime 5) (nfltime 4) (nfltime 3) (nfltime 2) (nfltime 1)))
              (/ (float (nfltime 7)) 1000)))
          ((looking-at "\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\).\\([0-9][0-9][0-9]\\) ") ;; hh:mm:ss.zzz
               (+ (float-time (encode-time (nfltime 3) (nfltime 2) (nfltime 1) 1 1 2000))
                  (/ (float (nfltime 4)) 1000)))
          (t nil))
    )
  )

(defun netflix-log-time-offset ()
  (interactive)
  (let* ((start (if (region-active-p) (min (region-beginning) (region-end)) (1- (point-at-bol))))
         (end (if (region-active-p) (1- (max (region-beginning) (region-end))) (point)))
         (starttime (netflix-log-time start))
         (endtime (netflix-log-time end)))
    ;; (message "%f %f %f" starttime endtime (- endtime starttime))
    (if (and starttime endtime)
        (format "%0.3fs " (- endtime starttime))
      "")
    )
  )

(provide 'netflix-log-mode)
