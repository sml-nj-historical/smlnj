(* copyright 1998 YALE FLINT PROJECT *)
(* monnier@cs.yale.edu *)

signature OPT_UTILS =
sig

    datatype ('a,'b) either = A of 'a | B of 'b

    (* takes the fk of a function and returns the fk of the wrapper
     * along with the new fk of the actual body *)
    val fk_wrap : FLINT.fkind * FLINT.lty list option ->
	              (FLINT.fkind * FLINT.fkind)

    (* this is a known APL function, but I don't know its real name *)
    val filter : bool list * 'a list -> 'a list

    (* A less brain-dead version of ListPair.all: returns false if
     * length l1 <> length l2 *)
    val ListPair_all : ('a * 'b -> bool) -> 'a list * 'b list -> bool
end

structure OptUtils :> OPT_UTILS =
struct
local structure F = FLINT
      structure LK = LtyKernel
in
    datatype ('a,'b) either = A of 'a | B of 'b

    fun bug msg = ErrorMsg.impossible ("OptUtils: "^msg)
				  
    fun fk_wrap ({inline,known,isrec,cconv},rtys') =
	let val cconv' =
		case cconv
		 of F.CC_FUN(LK.FF_VAR(f1,f2)) => F.CC_FUN(LK.FF_VAR(true, f2))
		  | (F.CC_FCT | F.CC_FUN(LK.FF_FIXED)) => cconv
	    val isrec' = Option.map (fn ltys => (ltys, F.LK_UNKNOWN)) rtys'
	in ({isrec=isrec, known=known, cconv=cconv, inline=F.IH_ALWAYS},
	    {isrec=isrec', known=true, cconv=cconv', inline=inline})
	end

    fun filter ([],[]) = []
      | filter (true::fs,x::xs)  = x::(filter(fs, xs))
      | filter (false::fs,x::xs) = (filter(fs, xs))
      | filter _ = bug "unmatched list length in filter"

    fun ListPair_all pred =
	let fun allp (a::r1, b::r2) = pred(a, b) andalso allp (r1, r2)
	      | allp ([],[]) = true
	      | allp _ = false
	in allp
	end

end
end
