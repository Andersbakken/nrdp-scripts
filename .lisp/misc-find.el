(require 'thingatpt)
(require 'typescript-mode)
(require 'find-file-in-repository)
(require 'tide)
(require 'ido)
(require 'nrdp-misc "misc")
(require 'simple)
(defvar misc-find-symbol-has-slime nil)
(defvar misc-find-symbol-has-rtags nil)
(setq misc-find-symbol-has-slime (require 'elisp-slime-nav nil t))
(setq misc-find-symbol-has-rtags (and (require 'rtags nil t) (rtags-executable-find "rc")))

(defcustom misc-find-use-misc-next-previous-error
  nil
  "Whether to use misc-next-error/misc-previous-error"
  :type 'boolean
  :safe 'booleanp
  :group 'misc-find)

(defun --misc-grep-tide-next-prev-error (forward settransientmap)
  (let ((ref-buffer (get-buffer "*tide-references*")))
    (when ref-buffer
      (with-current-buffer ref-buffer
        (call-interactively
         (if forward
             'tide-cycle-next-reference
           'tide-cycle-previous-reference))
        (call-interactively 'tide-goto-line-reference)
        (when (and settransientmap misc-find-use-misc-next-previous-error)
          (set-transient-map misc-next-error-map))))))

(defun --misc-grep-find-symbol (&optional symbol)
  (interactive "sSymbol: ")
  (unless (and (stringp symbol) (> (length symbol) 0))
    (error "Nothing to find"))
  (let ((command (concat "-I -n \"\\<" symbol "\\>\""))
        (gitdir (magit-toplevel)))
    (if gitdir
        (let ((default-directory gitdir))
          (grep-find (concat "git --no-pager grep --recurse-submodules " command " ':!*/sunspider/*' ':!*/error-text/*' ':!*/xboxupsellpage.js' ':!*/mkdocs-material*' ':!*min.js*' ':!*/jquery*.js' ':!*bundle.js*' ':!*.yuv' ':!*.y4m' ':!*/ttrlibs.js'")))
      (let ((ancestor (cond ((--misc-find-deepest-ancestor-directory "configure"))
                            ((--misc-find-deepest-ancestor-directory "CMakeLists.txt"))
                            ((--misc-find-deepest-ancestor-directory "Makefile"))
                            (t default-directory))))
        (grep-find (concat "find " ancestor " -type f -print0 | xargs -0 grep -nH " command))))))

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
  (cond ((eq major-mode 'emacs-lisp-mode)
         (call-interactively 'find-function))
        ((and (member 'tide-mode minor-mode-list) (eq major-mode 'typescript-mode))
         (call-interactively 'tide-nav))
        ((and misc-find-symbol-has-rtags (rtags-has-filemanager))
         (call-interactively 'rtags-find-symbol))
        (t
         (call-interactively '--misc-grep-find-symbol))))

(defun misc-find-references-at-point (&optional prefix)
  (interactive "P")
  (cond ((and misc-find-symbol-has-rtags (rtags-is-indexed))
         (call-interactively 'rtags-find-references-at-point))
        ((eq major-mode 'emacs-lisp-mode)
         (call-interactively 'find-function)) ;; not sure what else to do here
        ((and (member 'tide-mode minor-mode-list) (eq major-mode 'typescript-mode))
         (call-interactively 'tide-references)
         (call-interactively '--misc-grep-tide-next-prev-error t nil))
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
       (if prefix
           'rtags-taglist
         'rtags-imenu)
     'idomenu)))

(defun misc-find-next-match ()
  (interactive)
  (cond ((and misc-find-symbol-has-rtags (rtags-is-indexed))
         (call-interactively 'rtags-next-match))
        ((and (member 'tide-mode minor-mode-list) (eq major-mode 'typescript-mode))
         (--misc-grep-tide-next-prev-error t t))
        (t (call-interactively (if misc-find-use-misc-next-previous-error 'misc-next-error 'next-error)))))

(defun misc-find-previous-match ()
  (interactive)
  (cond ((and misc-find-symbol-has-rtags (rtags-is-indexed))
         (call-interactively 'rtags-previous-match))
        ((and (member 'tide-mode minor-mode-list) (eq major-mode 'typescript-mode))
         (--misc-grep-tide-next-prev-error nil t))
        (t (call-interactively (if misc-find-use-misc-next-previous-error 'misc-previous-error 'previous-error)))))

(provide 'misc-find)
