(*
 * Running ML-Lex from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure LexTool = struct
    val command = Tools.newCmdController ("LEX", "ml-lex")
    val _ = Tools.registerStdShellCmdTool
	{ tool = "ML-Lex",
	  class = "mllex",
	  suffixes = ["lex", "l"],
	  command = command,
	  extensionStyle = Tools.EXTEND [("sml", SOME "sml")] }
end
