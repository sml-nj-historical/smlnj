(*
 * Running ML-Lex from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure LexTool = struct
    val legacy_control =
	Tools.boolcontrol ("use-legacy-lex",
			   "whether to use the old ml-lex instead of ml-flex",
			   true)

    val _ = Tools.registerStdShellCmdTool
	{ tool = "ML-Lex",
	  class = "mllex",
	  suffixes = ["lex", "l"],
	  cmdStdPath = fn () => if #get legacy_control () then "ml-lex"
				else "ml-flex",
	  template = NONE,
	  extensionStyle = Tools.EXTEND [("sml", SOME "sml", fn too => too)],
	  dflopts = [] }
end
