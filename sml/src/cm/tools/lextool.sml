structure LexTool =
    StdShellCmdTool (val tool = "ML-Lex"
		     val class = "mllex"
		     val suffixes = ["lex", "l"]
		     val command = ("LEX", "ml-lex")
		     val extensionStyle = Tools.EXTEND ["sml"]
		     val sml = true)
