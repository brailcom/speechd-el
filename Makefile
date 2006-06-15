# Makefile for speechd-el
# Copyright (C) 2003, 2004, 2005, 2006 Brailcom, o.p.s.

EMACS = emacs

NAME = speechd-el
VERSION = 2.0
DISTDIR = $(NAME)-$(VERSION)
TARFILE = $(NAME)-$(VERSION).tar

.PHONY: all compile install install-strip uninstall \
        clean distclean mostlyclean maintainer-clean TAGS info dvi dist check

all: compile info

compile: braille.elc brltty.elc mmanager.elc speechd.elc speechd-braille.elc \
         speechd-brltty.elc speechd-bug.elc speechd-common.elc \
         speechd-out.elc speechd-speak.elc speechd-ssip.elc

%.elc: %.el
	$(EMACS) --batch -l speechd-compile.el -f speechd-compile --kill

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
	chmod 755 $(DISTDIR)
	install -m 644 `find . -maxdepth 1 -type f -name '[a-zA-Z]*'` \
		$(DISTDIR)
	(cd $(DISTDIR); $(MAKE) distclean)
	tar cvf $(TARFILE) $(DISTDIR)
	gzip -9 $(TARFILE)

check:

