structure Null = GenNull.GenNullFn
		    (val library =
			 DynLinkage.open_lib { name = "./null.so",
					       lazy = true,
					       global = true })
