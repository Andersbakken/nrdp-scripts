(defvar agb-howdoi-mode-map nil)
;; assign command to keys
(setq agb-howdoi-mode-map (make-sparse-keymap))
(define-key agb-howdoi-mode-map (kbd "q") 'bury-buffer)
(define-key agb-howdoi-mode-map (kbd "j") 'next-line)
(define-key agb-howdoi-mode-map (kbd "k") 'previous-line)
(define-key agb-howdoi-mode-map (kbd "n") 'agb-howdoi-next)
(define-key agb-howdoi-mode-map (kbd "p") 'agb-howdoi-prev)

(define-derived-mode agb-howdoi-mode text-mode
  (setq mode-name "agb-howdoi")
  (use-local-map agb-howdoi-mode-map)
  (run-hooks 'agb-howdoi-hook)
  (goto-char (point-min))
  )

(defvar agb-howdoi-executable "howdoi")
(defvar agb-howdoi-last-index nil)
(defvar agb-howdoi-last-search nil)
(defun agb-howdoi (&optional answer-index search)
  "Replace region with agb-howdoi search results for that text."
  (interactive)
  (let ((args '("--all")))
    (unless search
      (setq search (shell-quote-argument
                    (if (and mark-active (> 0 (abs (- (point) (mark)))))
                        (buffer-substring-no-properties (point) (mark))
                      (read-from-minibuffer "How do I: ")))))
    (setq agb-howdoi-last-search search)
    (push search args)
    (if (numberp answer-index)
        (progn
          (setq agb-howdoi-last-index answer-index)
          (if (= answer-index 0)
              (progn
                (push "100" args)
                (push "-n" args))
            (progn
              (push (number-to-string answer-index) args)
              (push "-p" args))))
      (setq agb-howdoi-last-index 1))
    (if (get-buffer "*agb-howdoi*")
        (kill-buffer "*agb-howdoi*"))
    (switch-to-buffer (get-buffer-create "*agb-howdoi*"))
    (apply #'call-process agb-howdoi-executable nil t t args)
    (goto-char (point-min))
    (insert (format "%s %s (%d) \n" agb-howdoi-executable (combine-and-quote-strings args) agb-howdoi-last-index))
    (agb-howdoi-mode))
  )

(defun agb-howdoi-next () (interactive) (agb-howdoi-skip 1))
(defun agb-howdoi-prev () (interactive) (agb-howdoi-skip -1))

(defun agb-howdoi-skip (by)
  (interactive)
  (unless (and agb-howdoi-last-index (> (length agb-howdoi-last-search) 0))
    (error "No previous howdoi to skip from"))
  (unless by
    (setq by (read-number "How do I skip by: ")))
  (let ((idx (+ agb-howdoi-last-index by)))
    (and (>= idx 0) (agb-howdoi idx agb-howdoi-last-search)))
  )

(provide 'agb-howdoi)
