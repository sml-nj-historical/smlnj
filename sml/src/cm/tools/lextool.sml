(*
 * Running ML-Lex from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure LexTool =
    StdShellCmdTool (val tool = "ML-Lex"
		     val class = "mllex"
		     val suffixes = ["lex", "l"]
		     val command = ("LEX", "ml-lex")
		     val extensionStyle = Tools.EXTEND ["sml"]
		     val sml = true)
