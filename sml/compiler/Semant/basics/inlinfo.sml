(* inlinfo.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure InlInfo : INL_INFO = struct

    fun bug s = ErrorMsg.impossible ("InlInfo: " ^ s)

    exception E of PrimOp.primop * Types.ty

    type inl_info = II.ii

    val INL_PRIM = II.Info o E
    val INL_STR = II.List
    val INL_NO = II.Null

    fun match i { inl_prim, inl_str, inl_no } =
	case i of
	    II.Info (E x) => inl_prim x
	  | II.Info _ => bug "bogus Info node"
	  | II.List l => inl_str l
	  | II.Null => inl_no ()

    fun prInfo i = let
	fun loop (i, acc) =
	    match i { inl_prim = fn (p, _) => PrimOp.prPrimop p :: acc,
		      inl_no = fn () => "<InlNo>" :: acc,
		      inl_str = fn [] => "{}" :: acc
				 | h::t =>
				   "{" :: loop (h,
						foldr (fn (x, a) =>
							  "," :: loop (x, a))
						      ("}" :: acc)
						      t) }
    in
	concat (loop (i, []))
    end

    val selInfo = II.sel

    val isPrimInfo = II.isSimple

    fun isPrimCallcc (II.Info (E ((PrimOp.CALLCC | PrimOp.CAPTURE), _))) = true
      | isPrimCallcc _ = false

    fun pureInfo (II.Info (E (p, _))) =
	let fun isPure PrimOp.CAST = true
	      | isPure _ = false
	(* val isPure = PrimOp.purePrimop *)
	in
	    isPure p
	end
      | pureInfo _ = false

    val mkPrimInfo = INL_PRIM
    val mkStrInfo = INL_STR
    val nullInfo = INL_NO
end
