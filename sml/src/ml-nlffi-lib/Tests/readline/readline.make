# readline.make
#
#   Makefile for running ml-nlffigen to generate NLFFI stubs
#   for readline.
#
# Copyright (c) 2004 by Toyota Technological Institute at Chicago
#
# Author: Matthias Blume (blume@tti-c.org)
#
FILES = readline.h
H = LibH.libh
D = FFI
HF = ../libh.sml
CF = readline.cm

$(D)/$(CF): $(FILES)
	ml-nlffigen -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^
