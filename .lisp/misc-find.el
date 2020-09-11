(require 'thingatpt)
(require 'typescript-mode)
(require 'find-file-in-repository)
(require 'tide)
(require 'ido)
(defvar misc-find-symbol-has-slime nil)
(defvar misc-find-symbol-has-rtags nil)
(setq misc-find-symbol-has-slime (require 'elisp-slime-nav nil t))
(setq misc-find-symbol-has-rtags (and (require 'rtags nil t) (rtags-executable-find "rc")))

(defun --misc-grep-find-deepest-ancestor (file)
  (let ((ret)
        (dir default-directory))
    (while dir
      (let ((val (find-ancestor-file file dir)))
        (if (not val)
            (setq dir nil)
          (setq ret val)
          (setq dir (file-name-directory val))
          (if (string= dir "/")
              (setq dir nil)
            (setq dir (expand-file-name (concat dir "/..")))))))
    ret))

(defun --misc-grep-find-symbol (&optional symbol)
  (interactive "sSymbol: ")
  (unless (and (stringp symbol) (> (length symbol) 0))
    (error "Nothing to find"))
  (let ((command (concat "--recurse-submodules -I -n \"\\<" symbol "\\>\""
                         " ':!*/sunspider/*' ':!*/error-text/*' ':!*/xboxupsellpage.js' ':!*/mkdocs-material*' ':!*min.js*' ':!*/jquery*.js' ':!*bundle.js*' ':!*.yuv' ':!*.y4m' ':!*/ttrlibs.js'"))
        (gitdir (magit-toplevel)))
    (if gitdir
        (let ((default-directory gitdir))
          (grep-find (concat "git --no-pager grep " command)))
      (let ((ancestor (cond ((--misc-grep-find-deepest-ancestor "configure"))
                            ((--misc-grep-find-deepest-ancestor "CMakeLists.txt"))
                            ((--misc-grep-find-deepest-ancestor "Makefile"))
                            (t default-directory))))
        (grep-find (concat "find " ancestor "-type f -print0 | xargs -0 grep -nH " command))))))

(defun misc-find-file (&optional prefix)
  (interactive "P")
  (call-interactively
   (if (and misc-find-symbol-has-rtags
            (rtags-has-filemanager))
       'rtags-find-file
     'find-file-in-repository)))

(defun misc-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (cond ((and misc-find-symbol-has-rtags (rtags-is-indexed))
         (call-interactively 'rtags-find-symbol-at-point))
        ((eq major-mode 'emacs-lisp-mode)
         (call-interactively (if misc-find-symbol-has-slime 'elisp-slime-nav-find-elisp-thing-at-point 'find-function)))
        ((and (member 'tide-mode minor-mode-list) (eq major-mode 'typescript-mode))
         (call-interactively 'tide-jump-to-definition))
        (t
         (let ((cur (thing-at-point 'symbol)))
           (if cur
               (--misc-grep-find-symbol cur)
             (call-interactively '--misc-grep-find-symbol))))))

(defun misc-find-symbol (&optional prefix)
  (interactive "P")
  (cond ((and misc-find-symbol-has-rtags (rtags-is-indexed))
         (call-interactively 'rtags-find-symbol))
        ((eq major-mode 'emacs-lisp-mode)
         (call-interactively 'find-function))
        ((and (member 'tide-mode minor-mode-list) (eq major-mode 'typescript-mode))
         (call-interactively 'tide-nav))
        (t
         (call-interactively 'grep-find-symbol))))

(defun misc-find-references-at-point (&optional prefix)
  (interactive "P")
  (cond ((and misc-find-symbol-has-rtags (rtags-is-indexed))
         (call-interactively 'rtags-find-references-at-point))
        ((eq major-mode 'emacs-lisp-mode)
         (call-interactively 'find-function)) ;; not sure what else to do here
        ((and (member 'tide-mode minor-mode-list) (eq major-mode 'typescript-mode))
         (call-interactively 'tide-references))
        (t
         (let ((cur (thing-at-point 'symbol)))
           (if cur
               (--misc-grep-find-symbol cur)
             (call-interactively '--misc-grep-find-symbol))))))

(defun misc-imenu (&optional prefix)
  (interactive "P")
  (call-interactively
   (if (and misc-find-symbol-has-rtags
            (rtags-is-indexed)
            (or (eq major-mode 'c++-mode)
                (eq major-mode 'c-mode)))
       (if prefix 'rtags-taglist 'rtags-imenu)
     'idomenu)))

(defun misc-find-next-match ()
  (interactive)
  (cond ((and misc-find-symbol-has-rtags (rtags-is-indexed))
         (call-interactively 'rtags-next-match))
        ((and (member 'tide-mode minor-mode-list) (eq major-mode 'typescript-mode))
         (call-interactively 'tide-find-next-reference))
        (t (call-interactively (or next-error-function 'next-error)))))

(defun misc-find-previous-match ()
  (interactive)
  (cond ((and misc-find-symbol-has-rtags (rtags-is-indexed))
         (call-interactively 'rtags-previous-match))
        ((and (member 'tide-mode minor-mode-list) (eq major-mode 'typescript-mode))
         (call-interactively 'tide-find-previous-reference))
        (t (call-interactively (or prev-error-function 'previous-error)))))

(provide 'misc-find)
