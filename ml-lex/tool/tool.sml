(*
 * Running ML-Lex from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure LexTool = struct
    val legacy_control =
	Tools.stringcontrol ("lex-program",
			     "which version of the lexer generator to use",
			     "ml-lex")

    val _ = Tools.registerStdShellCmdTool
	{ tool = "ML-Lex",
	  class = "mllex",
	  cmdStdPath = fn () =>
			  case #get legacy_control () of
			      "ml-lex" => ("ml-lex", [])
			    | other => (other, ["--ml-lex-mode"]),
	  template = NONE,
	  extensionStyle = Tools.EXTEND [("sml", SOME "sml", fn too => too)],
	  dflopts = [] }
end
