local
    val lib = ref (NONE: DynLinkage.lib_handle option)
    fun library () =
	case !lib of
	    NONE => let
		val l = DynLinkage.open_lib { name = "./pdb.so",
					      lazy = true,
					      global = true }
	    in
		lib := SOME l;
		l
	    end
	  | SOME l => l
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
