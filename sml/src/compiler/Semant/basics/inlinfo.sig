(* inlinfo.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

signature INL_INFO = sig

    type inl_info

    val INL_PRIM : PrimOp.primop -> inl_info   (* PRIMOP *)
    val INL_STR : inl_info list -> inl_info
    val INL_NO : inl_info

    val match : inl_info ->
		{ inl_prim: PrimOp.primop -> 'a,  (* PRIMOP *)
		  inl_str: inl_info list -> 'a,
		  inl_no: unit -> 'a } ->
		'a

    val prInfo : inl_info -> string
    val selInfo : inl_info * int -> inl_info

    val isPrimInfo : inl_info -> bool
    val isPrimCallcc : inl_info -> bool
    val pureInfo : inl_info -> bool

    val mkPrimInfo : PrimOp.primop -> inl_info   (* PRIMOP *)
    val mkStrInfo : inl_info list -> inl_info

    val nullInfo : inl_info
end
