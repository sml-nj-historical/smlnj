structure Intr = GenIntr.GenIntrFn
		    (fun library () =
			 DynLinkage.open_lib { name = "./intr.so",
					       lazy = true,
					       global = true })
