emacs ?= emacs
emacs := $(emacs) --eval "(package-initialize)" --batch
el = $(wildcard *.el)
elc = $(el:.el=.elc)
autoloads = loaddefs.el

all : $(autoloads) compile

compile : $(elc)

%.elc : %.el
	$(emacs) -f batch-byte-compile $<

$(autoloads):
	$(emacs) --eval "(let ((generated-autoload-file (expand-file-name \"loaddefs.el\")) \
(backup-inhibited t)) (update-directory-autoloads \".\"))"

.PHONY: clean
clean:
	$(RM) $(autoloads) $(elc)
