emacs ?= emacs
wget ?= wget

.PHONY: clean distclean
all: 

README.md: el2markdown.el $(el)
	$(emacs) -batch -l $< $(el) -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
