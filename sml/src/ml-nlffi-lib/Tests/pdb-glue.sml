local
    val library = DynLinkage.open_lib { name = "./pdb.so",
					lazy = true,
					global = true }
in
structure PDB = GenPDB.GenPDBFn
		    (val library = library
		     structure I_S_forward =
		         PointerToCompleteType (GenForward.S_forward)
		     structure I_S__IO_FILE = PointerToIncompleteType ())

structure Forward = GenForward.GenForwardFn
		    (val library = library
		     structure I_S_entry =
		         PointerToCompleteType (GenPDB.S_entry))
end
