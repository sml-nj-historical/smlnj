structure YaccTool =
    StdShellCmdTool (val tool = "ML-Yacc"
		     val class = "mlyacc"
		     val suffixes = ["grm", "y"]
		     val command = ("YACC", "ml-yacc")
		     val extensionStyle = Tools.EXTEND ["sig", "sml"]
		     val sml = true)
