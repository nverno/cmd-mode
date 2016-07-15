emacs ?= emacs
wget ?= wget
batch = $(emacs) -batch \
	--eval "(let ((default-directory (expand-file-name \".emacs.d/elpa\" \"~\"))) \
		   (normal-top-level-add-subdirs-to-load-path))"

el = $(wildcard *.el)
elc = $(el:.el=.elc)

auto=../loaddefs.el
auto_flags= \
	--eval "(let ((generated-autoload-file \
                      (expand-file-name (unmsys--file-name \"$@\"))) \
                      (backup-inhibited t) \
                      (default-directory (expand-file-name \".emacs.d/elpa\" \"~\"))) \
                   (normal-top-level-add-subdirs-to-load-path) \
                   (update-directory-autoloads \".\"))"

.PHONY: $(auto) clean
all: compile $(auto) README.md

compile : $(elc)
%.elc : %.el
	$(batch) -f batch-byte-compile $<

$(auto):
	$(emacs) -batch $(auto_flags)

README.md: el2markdown.el $(el)
	$(emacs) -batch -l $< $(el) -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *.elc *~

