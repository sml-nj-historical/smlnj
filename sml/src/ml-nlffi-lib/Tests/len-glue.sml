structure Len = GenLen.GenLenFn (val library =
				     DynLinkage.open_lib { name = "./len.so",
							   lazy = true,
							   global = true })
