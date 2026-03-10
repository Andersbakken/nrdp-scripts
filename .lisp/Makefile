EMACS ?= emacs

EL := $(wildcard *.el)
ELC := $(EL:.el=.elc)

EMACS_LOAD_PATH := --eval "(dolist (d '(\"$(CURDIR)\" \
  \"$(HOME)/.emacs.d/straight/build\" \
  \"$(HOME)/.emacs.d/elpa\")) \
  (when (file-directory-p d) \
    (add-to-list 'load-path d) \
    (dolist (sd (directory-files d t \"^[^.]\")) \
      (when (file-directory-p sd) \
        (add-to-list 'load-path sd)))))"

all: $(ELC)

%.elc: %.el
	$(EMACS) -Q --batch $(EMACS_LOAD_PATH) -f batch-byte-compile $<

clean:
	rm -f $(ELC)

.PHONY: all clean
