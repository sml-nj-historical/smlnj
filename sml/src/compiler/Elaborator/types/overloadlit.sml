(* COPYRIGHT 1997 Bell Laboratories *)
(* overloadlit.sml *)

(* overloaded literals *)
signature OVERLOADLIT =
sig

  (* functions for setting up, recording, and resolving literal overloadings *)
  val new : unit -> { push : Types.ty -> unit, resolve : unit -> unit }

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
  val intTypes = [BT.intTy, BT.int32Ty, BT.int64Ty, BT.intinfTy]
  val wordTypes = [BT.wordTy, BT.word8Ty, BT.word32Ty, BT.word64Ty]
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

  fun new () = let
      val lits = ref []
      fun push x = lits := x :: !lits
      fun resolve () =
	  let fun resolveLit ty =
		  case TU.prune ty
		   of T.VARty(tv as ref(T.LITERAL{kind,...})) =>
		      tv := T.INSTANTIATED(default kind)
		    | _ => () (* ok, must have been successfully instantiated *)
	  in app resolveLit (!lits)
	  end
  in { push = push, resolve = resolve }
  end

end (* structure OverloadLit *)
