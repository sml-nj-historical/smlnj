(*
 * Running ML-Yacc from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure YaccTool = struct
    val _ = Tools.registerStdShellCmdTool
	{ tool = "ML-Yacc",
	  class = "mlyacc",
	  suffixes = ["grm", "y"],
	  cmdStdPath = "ml-yacc",
	  template = NONE,
	  extensionStyle =
	      Tools.EXTEND [("sig", SOME "sml", fn _ => NONE),
			    ("sml", SOME "sml", fn too => too)],
	  dflopts = NONE }
end
