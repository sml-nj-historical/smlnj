FILES = intr.c
H = LibH.libh
D = FFI
HF = ../libh.sml
CF = intr.cm

$(D)/$(CF): $(FILES)
	ml-nlffigen -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^
