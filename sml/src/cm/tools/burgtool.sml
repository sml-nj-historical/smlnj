(*
 * Running ML-Burg from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure BurgTool =
    StdShellCmdTool (val tool = "ML-Burg"
		     val class = "mlburg"
		     val suffixes = ["burg"]
		     val command = ("BURG", "ml-burg")
		     val extensionStyle = Tools.REPLACE (["burg"], ["sml"])
		     val sml = true)
