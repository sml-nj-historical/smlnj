structure SMLCopyTool = struct
    val _ = Tools.registerStdShellCmdTool
	    { tool = "SML-Copy", class = "smlcopy", suffixes = ["cp"],
	      cmdStdPath = "cp", template = SOME "%c %s %1",
	      extensionStyle = Tools.EXTEND [("sml", SOME "sml")] }
end
