# Makefile for speechd-el
# Copyright (C) 2003 Brailcom, o.p.s.

EMACS = emacs

NAME = speechd-el
VERSION = 0.3
DISTDIR = $(NAME)-$(VERSION)
TARFILE = $(NAME)-$(VERSION).tar

.PHONY: all install install-strip uninstall clean distclean mostlyclean \
	maintainer-clean TAGS info dvi dist check

all: compile info

compile: speechd.elc speechd-speak.elc speechd-bug.elc
speechd.elc: speechd.el
	$(EMACS) --batch -l speechd.el -f batch-byte-compile $<
speechd-speak.elc: speechd-speak.el speechd.elc
	$(EMACS) --batch -l speechd.elc -l speechd-speak.el -f batch-byte-compile $<
speechd-bug.elc: speechd-bug.el speechd-speak.elc speechd.elc
	$(EMACS) --batch -l speechd.elc -l speechd-speak.elc -l speechd-bug.el -f batch-byte-compile $<

install:

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

uninstall:

mostlyclean:
	rm -f *.aux *.cp *.cps *.fn *.ky *.log *.pg *.toc *.tp *.vr *~

clean: mostlyclean
	rm -f *.dvi *.elc speechd-el.pdf *.ps

distclean: clean
	rm -rf $(DISTDIR) $(TARFILE)* *.orig *.rej

maintainer-clean: distclean
	rm -f *.info*

TAGS:
	etags *.el

doc: info pdf

info: speechd-el.info
%.info: %.texi
	makeinfo $<

info-cs: speechd-el.cs.info

pdf: speechd-el.pdf
%.pdf: %.texi
	texi2pdf $<

ps: speechd-el.ps
%.ps: %.texi
	texi2ps $<

dist: maintainer-clean info
	mkdir $(DISTDIR)
	install -m 644 `find . -maxdepth 1 -type f -name '[a-zA-Z]*'` \
		$(DISTDIR)
	(cd $(DISTDIR); $(MAKE) distclean)
	tar cvf $(TARFILE) $(DISTDIR)
	gzip -9 $(TARFILE)

check:

