(*
 * Running ML-Yacc from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure YaccTool = struct
    val command = Tools.newCmdController ("YACC", "ml-yacc")
    val _ = Tools.registerStdShellCmdTool
	{ tool = "ML-Yacc",
	  class = "mlyacc",
	  suffixes = ["grm", "y"],
	  command = command,
	  extensionStyle = Tools.EXTEND [("sig", SOME "sml"),
					 ("sml", SOME "sml")] }
end
