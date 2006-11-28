(* inlinfo.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

signature INL_INFO =
sig

    type inl_info

    val match : inl_info ->
		{ inl_prim: PrimOp.primop * Types.ty -> 'a,
		  inl_str: inl_info list -> 'a,
		  inl_no: unit -> 'a } ->
		'a

    val prInfo : inl_info -> string

    val isPrimop : inl_info -> bool
    val selStrInfo : inl_info * int -> inl_info

    val isPrimCallcc : inl_info -> bool
    val isPrimCast : inl_info -> bool

    val mkPrimInfo : PrimOp.primop * Types.ty -> inl_info
    val mkStrInfo : inl_info list -> inl_info
    val nullInfo : inl_info

    val primopTy : inl_info -> Types.ty option

end (* signature INL_INFO *)
