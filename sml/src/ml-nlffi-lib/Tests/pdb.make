FILES = pdb.c forward.c
H = PDBHandle.pdb
D = pdb
HF = ../pdbhandle.sml
CF = pdb.cm

pdb/pdb.cm: pdb.c forward.c
	ml-nlffigen -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^
