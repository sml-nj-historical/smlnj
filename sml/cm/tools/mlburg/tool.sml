(*
 * Running ML-Burg from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure BurgTool = struct
    val _ = Tools.registerStdShellCmdTool
	{ tool = "ML-Burg",
	  class = "mlburg",
	  suffixes = ["burg"],
	  cmdStdPath = fn () => ("ml-burg", []),
	  template = NONE,
	  extensionStyle =
	      Tools.REPLACE (["burg"], [("sml", SOME "sml", fn too => too)]),
	  dflopts = [] }
end
