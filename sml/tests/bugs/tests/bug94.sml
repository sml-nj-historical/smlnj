(* bug 94, Tarditi -- Bind exception *)

functor mkDummy () : sig end  = 
    struct
    end

functor mkLalr () =
    struct
	datatype lcore = LCORE of int
    end

functor mkTable () =
   struct
	structure Dummy = mkDummy()
	structure Lalr = mkLalr()
	val x = fn (Lalr.LCORE l) => l
    end
