structure Null = GenNull.GenNullFn
		    (fun library () =
			 DynLinkage.open_lib { name = "./null.so",
					       lazy = true,
					       global = true })
