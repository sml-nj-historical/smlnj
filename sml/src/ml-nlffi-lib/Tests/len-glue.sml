structure Len = GenLen.GenLenFn (fun library () =
				     DynLinkage.open_lib { name = "./len.so",
							   lazy = true,
							   global = true })
