(* inlinfo.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

signature INL_INFO = sig

    type inl_info

    val INL_PRIM : PrimOp.primop * Types.ty -> inl_info
    val INL_STR : inl_info list -> inl_info
    val INL_NO : inl_info
    val INL_PGN: inl_info

    val match : inl_info ->
		{ inl_prim: PrimOp.primop * Types.ty -> 'a,
		  inl_str: inl_info list -> 'a,
		  inl_pgn: unit -> 'a,
		  inl_no: unit -> 'a } ->
		'a

    val prInfo : inl_info -> string
    val selInfo : inl_info * int -> inl_info

    val isPrimInfo : inl_info -> bool
    val isPrimCallcc : inl_info -> bool
    val pureInfo : inl_info -> bool

    val mkPrimInfo : PrimOp.primop * Types.ty -> inl_info
    val mkStrInfo : inl_info list -> inl_info

    val nullInfo : inl_info
end
