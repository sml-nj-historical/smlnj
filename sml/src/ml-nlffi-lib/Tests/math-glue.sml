structure CMath =
    GenMath.GenMathFn (fun library s = let
			   open DynLinkage
			   val h = lib_symbol (main_lib, s)
		       in
			   fn () => addr h
		       end)
