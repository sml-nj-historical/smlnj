FILES = readline.h
H = LibH.libh
D = FFI
HF = ../libh.sml
CF = readline.cm

$(D)/$(CF): $(FILES)
	ml-nlffigen -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^
