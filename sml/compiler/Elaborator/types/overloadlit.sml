(* COPYRIGHT 1997 Bell Laboratories *)
(* overloadlit.sml *)

(* overloaded literals *)
signature OVERLOADLIT =
sig

(*
  (* functions for setting up, recording, and resolving literal overloadings *)
  val new : unit -> { push : Types.ty -> unit, resolve : unit -> unit }
*)
  val litTypes: Types.litKind -> Types.ty list

  (* isLiteralTy is for checking compatability when instantiating 
     overloaded literal type variables *)
  val isLiteralTy : Types.litKind * Types.ty -> bool
end  (* signature OVERLOADLIT *)

structure OverloadLit : OVERLOADLIT = 
struct

  structure T = Types
  structure BT = BasicTypes
  structure TU = TypesUtil

  (* eventually, these may be defined elsewhere, perhaps via some
     compiler configuration mechanism *)
  fun litTypes(T.INT) = [BT.intTy, BT.int32Ty, BT.int64Ty, BT.intinfTy]
    | litTypes(T.WORD) = [BT.wordTy, BT.word8Ty, BT.word32Ty, BT.word64Ty]
    | litTypes(T.REAL) = [BT.realTy]
    | litTypes(T.CHAR) = [BT.charTy]
    | litTypes(T.STRING) = [BT.stringTy]

  fun isLiteralTy(kind,ty) = TU.inClass(ty,litTypes(kind))

  (* Default for kind is is first element of litTypes(kind) (or filtered
     version resulting from unificaton).
  *)

end (* structure OverloadLit *)
