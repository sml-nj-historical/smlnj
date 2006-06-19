(* inlinfo.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

(* [dbm, 6/19/06]
     Folded ii.sml into this structure, eliminating exn hack.
     Changed name of pureInfo to isPrimCast.
     Eliminated redundant INL_PRIM, INL_STR, INL_NO. *)

structure InlInfo : INL_INFO = struct

    fun bug s = ErrorMsg.impossible ("InlInfo: " ^ s)

    exception E of PrimOp.primop * Types.ty

    datatype inl_info
      = Info of PrimOp.primop * Types.ty
      | List of inl_info list
      | Null

    fun isPrimop (Info _) = true
      | isPrimop _ = false

    fun selStrInfo (List l, i) =
	(List.nth (l, i) handle Subscript => bug "Wrong field in List")
      | selStrInfo (Null, _) = Null
      | selStrInfo (Info _, i) = bug "Unexpected selection from Info"
    end

    fun match i { inl_prim, inl_str, inl_no } =
	case i
	  of Info x => inl_prim x
	   | List l => inl_str l
	   | Null => inl_no ()

    fun prInfo i = let
	fun loop (i, acc) =
	    case i
              of Info (p,_) => PrimOp.prPrimop p :: acc
	       | Null => "<InlNo>" :: acc
	       | List m => 
                 (case m
                   of [] => "{}" :: acc
		    | h::t =>
		      "{" :: loop (h,foldr (fn (x, a) => "," :: loop (x, a))
                                           ("}" :: acc)
					   t)
    in
	concat (loop (i, []))
    end

    fun isPrimCallcc (Info ((PrimOp.CALLCC | PrimOp.CAPTURE), _)) = true
      | isPrimCallcc _ = false

    fun isPrimCast (Info (PrimOp.CAST, _)) = true
      | isPrimCast _ = false

    val mkPrimInfo = Info
    val mkStrInfo = List
    val nullInfo = Null

    fun primopTy (Info (_, ty)) = SOME ty
      | primopTy _ = NONE

end (* structure InlInfo *)
