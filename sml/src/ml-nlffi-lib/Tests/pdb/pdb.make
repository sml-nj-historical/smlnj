FILES = pdb.c forward.c
H = PDBHandle.pdb
D = FFI
HF = ../pdbhandle.sml
CF = pdb.cm

$(D)/$(CF): $(FILES)
	ml-nlffigen -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^
