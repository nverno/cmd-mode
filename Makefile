emacs ?= emacs
emacs := $(emacs) --eval "(package-initialize)" --batch
el = $(wildcard *.el)
elc = $(el:.el=.elc)

# probably want to change location of autoloads
autoloads = ../loaddefs.el

.PHONY: all $(autoloads) clean

all : $(autoloads) compile

compile : $(elc)

%.elc : %.el
	$(emacs) -f batch-byte-compile $<

$(autoloads):
	$(emacs) --eval "(let ((generated-autoload-file (expand-file-name \"../loaddefs.el\")) \
(backup-inhibited t)) (update-directory-autoloads \".\"))"

clean:
	$(RM) $(autoloads) $(elc)
