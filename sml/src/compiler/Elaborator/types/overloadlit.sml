(* COPYRIGHT 1997 Bell Laboratories *)
(* overloadlit.sml *)

(* overloaded literals *)
signature OVERLOADLIT =
sig

  (* functions for setting up, recording, and resolving literal overloadings *)
  val reset : unit -> unit
  val push  : Types.ty -> unit
  val resolve : unit -> unit

  (* isLiteralTy is for checking compatability when instantiating 
     overloaded literal type variables *)
  val isLiteralTy : Types.litKind * Types.ty -> bool

  val debugging : bool ref

end  (* signature OVERLOADLIT *)

structure OverloadLit : OVERLOADLIT = 
struct

  structure T = Types
  structure BT = BasicTypes
  structure TU = TypesUtil

  (* debugging *)
  val say = Control_Print.say
  val debugging = ref false
  fun debugmsg (msg: string) = if !debugging then (say msg; say "\n") else ()
  fun bug msg = ErrorMsg.impossible("OverloadLit: "^msg)

  (* list ref storing literal types for a given typecheck call *)
  val lits = ref(nil: T.ty list)

  fun reset () =
      lits := []

  fun push x = lits := x :: !lits

  (* eventually, these may be defined elsewhere, perhaps via some
     compiler configuration mechanism *)
  val intTypes = [BT.intTy, BT.int32Ty]
  val wordTypes = [BT.wordTy, BT.word8Ty, BT.word32Ty]
  val realTypes = [BT.realTy]
  val charTypes = [BT.charTy]
  val stringTypes = [BT.stringTy]

  fun inClass(ty, tys) = List.exists (fn ty' => TU.equalType(ty,ty')) tys

  fun isLiteralTy(T.INT,ty) = inClass(ty,intTypes)
    | isLiteralTy(T.WORD,ty) = inClass(ty,wordTypes)
    | isLiteralTy(T.REAL,ty) = inClass(ty,realTypes)
    | isLiteralTy(T.CHAR,ty) = inClass(ty,charTypes)
    | isLiteralTy(T.STRING,ty) = inClass(ty,stringTypes)

  fun default T.INT = BT.intTy
    | default T.WORD = BT.wordTy
    | default T.REAL = BT.realTy
    | default T.CHAR = BT.charTy
    | default T.STRING = BT.stringTy

  fun resolve () =
      let fun resolveLit ty =
	      case TU.prune ty
		of T.VARty(tv as ref(T.LITERAL{kind,...})) =>
		     tv := T.INSTANTIATED(default kind)
		 | _ => () (* ok, must have been successfully instantiated *)
       in app resolveLit (!lits)
      end

end (* structure OverloadLit *)

