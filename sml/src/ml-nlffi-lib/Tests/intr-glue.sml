structure Intr = GenIntr.GenIntrFn
		    (val library =
			 DynLinkage.open_lib { name = "./intr.so",
					       lazy = true,
					       global = true })
