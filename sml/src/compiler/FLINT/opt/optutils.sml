(* copyright 1998 YALE FLINT PROJECT *)
(* monnier@cs.yale.edu *)

signature OPT_UTILS =
sig

    (* takes the fk of a function and returns the fk of the wrapper
     * along with the new fk of the actual body *)
    val fk_wrap : FLINT.fkind * FLINT.lty list option ->
	              (FLINT.fkind * FLINT.fkind)

    (* sometimes I get fed up rewriting the identity function *)
    val id : 'a -> 'a

    (* this is a known APL function, but I don't know its real name *)
    val filter : bool list * 'a list -> 'a list
end

structure OptUtils :> OPT_UTILS =
struct
local structure F = FLINT
      structure LK = LtyKernel
in
    fun bug msg = ErrorMsg.impossible ("OptUtils: "^msg)
				  
    fun fk_wrap (F.FK_FCT,_) = (F.FK_FCT, F.FK_FCT)
      | fk_wrap (F.FK_FUN{isrec,known,fixed,inline},rtys') =
	let val fixed' = case fixed
			  of LK.FF_VAR(f1,f2) => LK.FF_VAR(true, f2)
			   | LK.FF_FIXED => LK.FF_FIXED
	in (F.FK_FUN{isrec=isrec, known=known, fixed=fixed, inline=true},
	    F.FK_FUN{isrec=rtys', known=true, fixed=fixed', inline=inline})
	end

    fun filter ([],[]) = []
      | filter (true::fs,x::xs)  = x::(filter(fs, xs))
      | filter (false::fs,x::xs) = (filter(fs, xs))
      | filter _ = bug "unmatched list length in filter"

    fun id x = x

end
end
