structure BurgTool =
    StdShellCmdTool (val tool = "ML-Burg"
		     val class = "mlburg"
		     val suffixes = ["burg"]
		     val command = ("BURG", "ml-burg")
		     val extensionStyle = Tools.REPLACE (["burg"], ["sml"])
		     val sml = true)
