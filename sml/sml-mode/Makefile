# Makefile for emacs-lisp package

# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.

# This file is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

EMACS   = emacs
prefix  = /usr/local

# the directory where you install third-party emacs packges
lispdir = $(prefix)/share/emacs/site-lisp

# the directory where you installed the elib .elc files.
# This is only needed if your site-start.el (or default.el) does not
# set up elib correctly.
elibdir = $(lispdir)/elib

# the directory where you install the info doc
infodir = $(prefix)/info
docdir = $(prefix)/doc

######################################################################
###        No changes below this line should be necessary          ###
######################################################################


PACKAGE = sml-mode

# the directory where the .elc files will be installed
elcdir  = $(lispdir)/$(PACKAGE)
eldir   = $(elcdir)

ELFLAGS	= --eval '(setq load-path (append (list "." "$(elibdir)" "$(lispdir)") load-path))'
ELC	= $(EMACS) -batch $(ELFLAGS) -f batch-byte-compile

ELFILES	= sml-compat.el sml-util.el sml-defs.el sml-move.el sml-mode.el \
	sml-proc.el
ELCFILES = $(ELFILES:.el=.elc)

TEXEXTS =  *.cps *.fns *.kys *.vr *.tp *.pg *.log *.aux *.toc *.cp *.ky *.fn

.SUFFIXES: .elc .el .info .ps .dvi .texi
.PHONY: elcfiles info clean distclean default
.PHONY: install_startup install_elc install install_el install_info

.el.elc:
	$(ELC) $<

.texi.info:
	makeinfo $<

.texi.dvi:
	texi2dvi $<

.dvi.ps:
	dvips -f $< >$@

$(PACKAGE)-startup.el:  $(ELFILES)
	$(EMACS) --batch --eval '(setq generated-autoload-file "'`pwd`'/$@")' -f batch-update-autoloads "."

######################################################################

default: elcfiles

elcfiles: $(ELCFILES)
info: $(PACKAGE).info

install_elc: $(ELCFILES)
	mkdir -p $(elcdir)
	cp $(ELCFILES) $(elcdir)/

install_el:
	mkdir -p $(eldir)
	cp $(ELFILES) $(eldir)/

install_info: $(PACKAGE).info
	mkdir -p $(infodir)
	cp *.info* $(infodir)/
	-[ ! -w $(infodir)/dir ] || install-info $(PACKAGE).info $(infodir)/dir

install_startup:
	mkdir -p $(lispdir)
	if grep $(PACKAGE) $(lispdir)/site-start.el >/dev/null 2>&1 || \
	   grep $(PACKAGE) $(lispdir)/default.el >/dev/null 2>&1; then :; else \
	    sed 's/@elcdir@/$(elcdir)/' \
		$(PACKAGE)-startup.el >>$(lispdir)/site-start.el ;\
	fi

install_dvi: $(PACKAGE).dvi
	mkdir -p $(docdir)
	cp *.dvi $(docdir)/

install: install_elc install_info # install_el

clean:
	$(RM) .\#* $(TEXEXTS)

distclean: clean
	$(RM) *.elc *.dvi *.info* *.ps

######################################################################
###                    don't look below                            ###
######################################################################

TAG = $(shell echo v$(VERSION) | tr '.' '_')
ftpdir=/home/ftp/pub/monnier/$(PACKAGE)

dist:
	cvs tag -F $(TAG) &&\
	cd $(TMP) &&\
	cvs export -r $(TAG) -d $(PACKAGE)-$(VERSION) elisp/$(PACKAGE) &&\
	cd $(PACKAGE)-$(VERSION) &&\
	gmake info &&\
	cd .. &&\
	ztar $(PACKAGE)-$(VERSION) &&\
	rm -rf $(PACKAGE)-$(VERSION)
	mv $(TMP)/$(PACKAGE)-$(VERSION).tar.gz $(ftpdir)/
	ln -sf $(PACKAGE)-$(VERSION).tar.gz $(ftpdir)/$(PACKAGE).tar.gz


#ident @(#)$Name$:$Id$
