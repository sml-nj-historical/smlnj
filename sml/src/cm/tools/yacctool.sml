(*
 * Running ML-Yacc from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure YaccTool =
    StdShellCmdTool (val tool = "ML-Yacc"
		     val class = "mlyacc"
		     val suffixes = ["grm", "y"]
		     val command = ("YACC", "ml-yacc")
		     val extensionStyle = Tools.EXTEND ["sig", "sml"]
		     val sml = true)
