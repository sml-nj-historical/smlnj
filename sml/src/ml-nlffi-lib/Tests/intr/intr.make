FILES = intr.c
H = LibH.libh
D = FFI
HF = ../libh.sml
CF = intr.cm
CPPO = -D__builtin_va_list=int

$(D)/$(CF): $(FILES)
	ml-nlffigen $(CPPO) -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^
