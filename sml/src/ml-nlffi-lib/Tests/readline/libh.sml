structure LibH = struct
    local 
	val lh_curses = DynLinkage.open_lib
		    { name = "libcurses.so", global = true, lazy = true }
	val lh = DynLinkage.open_lib'
		    { name = "libreadline.so", global = true, lazy = true,
		      dependencies = [lh_curses] }
    in
        fun libh s = let
	    val sh = DynLinkage.lib_symbol (lh, s)
	in
	    fn () => DynLinkage.addr sh
	end
    end
end
