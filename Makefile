# Makefile for speechd-el
# Copyright (C) 2003, 2004, 2005, 2006, 2008, 2010 Brailcom, o.p.s.
# Copyright (C) 2012, 2013 Milan Zamazal

# COPYRIGHT NOTICE
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

EMACS = emacs

NAME = speechd-el
VERSION = 2.7
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
	rm -rf $(DISTDIR) $(TARFILE)* *.orig *.rej TAGS

maintainer-clean: distclean
	rm -f *.info* dir

TAGS:
	etags *.el

doc: info pdf

info: speechd-el.info dir
%.info: %.texi
	makeinfo $<
dir: speechd-el.info
	install-info $< dir

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

