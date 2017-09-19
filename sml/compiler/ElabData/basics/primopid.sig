(* primopid.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

(* PRIMOPID: front-end representation of information identifying
 * primops (either in variables, or in structures). Replaces
 * INL_INFO *)

signature PRIMOPID =
sig

  datatype primId = Prim of PrimopBindings.primop_bind | NonPrim

  datatype strPrimElem
    = PrimE of primId
    | StrE of strPrimInfo

  withtype strPrimInfo = strPrimElem list

  val isPrimop : primId -> bool

  val isPrimCallcc : primId -> bool
  val isPrimCast : primId -> bool

  val selStrPrimId : strPrimElem list * int -> strPrimElem list
  val selValPrimFromStrPrim : strPrimElem list * int -> primId

  val ppPrim : primId -> string
  val ppStrInfo : strPrimInfo -> unit

end (* signature PRIMOPID *)
