(* alignment.sml --- determine if the allocation pointer should be
 *		     aligned on entry to function.
 *)
signature ALIGNMENT = sig
  val build : CPS.function list -> (int -> bool) 
end

structure Alignment = struct
  structure C = CPS

  exception Alignment

  fun error msg = ErrorMsg.impossible ("Alignment." ^ msg)

  fun build(cluster) = let
    (* SortedList should be replaced by int-binary-set *)
    fun hasFloats(C.RECORD(rk, _, _, e)) = 
	 (case rk of (C.RK_FCONT | C.RK_FBLOCK) => true | _ => hasFloats(e))
      | hasFloats(C.SELECT(_, _, _, _, e)) = hasFloats(e)
      | hasFloats(C.OFFSET(_, _, _, e)) = hasFloats(e)
      | hasFloats(C.APP _) = false
      | hasFloats(C.FIX _) = error "hasFloats: FIX"
      | hasFloats(C.SWITCH(_, _, el)) = let
	  fun iter [] = false
	    | iter(e::el) = hasFloats(e) orelse iter(el)
	in iter el
	end
      | hasFloats(C.BRANCH(_, _, _, e1, e2)) = 
	  hasFloats(e1) orelse hasFloats(e2)
      | hasFloats(C.SETTER(_, _, e)) = hasFloats(e)
      | hasFloats(C.LOOKER(_, _, _, _, e)) = hasFloats(e)
      | hasFloats(C.ARITH(_, _, _, _, e)) = hasFloats(e)
      | hasFloats(C.PURE(C.P.fwrap, _, _, _, _)) = true
      | hasFloats(C.PURE(_, _, _, _, e)) = hasFloats(e)

    fun doFunction((_,f,_,_,e), set) = 
      if hasFloats e then SortedList.enter(f, set) else set
  in SortedList.member (List.foldl doFunction [] cluster)
  end (* build *)
end