SML=sml.x86-linux
BINDIR=bin.x86-unix
MLCMD=CMB.make()

$(SML): $(BINDIR)/COMPLIST
	-rm -f $@
	./xmakeml

$(BINDIR)/COMPLIST:
	[ -d $(BINDIR) -a -d $(BINDIR).old ] && \
	mv $(BINDIR) $(BINDIR).new && \
	mv $(BINDIR).old $(BINDIR)

recompile: $(SML)
	mv $(BINDIR) $(BINDIR).old && mv $(BINDIR).new $(BINDIR)
	echo '$(MLCMD);Compiler.Stats.summary();' | ./xrun $(SML)
	mv $(BINDIR) $(BINDIR).new && mv $(BINDIR).old $(BINDIR)

run: $(SML)
	mv $(BINDIR) $(BINDIR).old && mv $(BINDIR).new $(BINDIR)
	./xrun $(SML)
	mv $(BINDIR) $(BINDIR).new && mv $(BINDIR).old $(BINDIR)
