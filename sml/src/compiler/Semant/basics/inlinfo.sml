(* Copyright 1996 by Bell Laboratories *)
(* inlinfo.sml *)

signature INL_INFO = 
sig

  datatype inl_info
    = INL_PRIM of PrimOp.primop   * Types.ty option
(*  | INL_LEXP of FLINT.prog * Types.ty option *)
    | INL_PATH of Access.access   * Types.ty option
    | INL_STR  of inl_info list
    | INL_NO
  
  val prInfo : inl_info -> string
  val selInfo : inl_info * int -> inl_info
  
  val isPrimInfo : inl_info -> bool
  val isPrimCallcc : inl_info -> bool
  val pureInfo : inl_info -> bool
  
  val mkPrimInfo : PrimOp.primop * Types.ty option -> inl_info
  val mkAccInfo : Access.access * Types.ty option -> inl_info
  val mkStrInfo : inl_info list -> inl_info
  
  val nullInfo : inl_info

end (* signature INL_INFO *)


structure InlInfo : INL_INFO =
struct

local structure A  = Access
      structure PO = PrimOp
      structure T  = Types
      structure EM = ErrorMsg
in 

fun bug msg = EM.impossible("InlInfo: "^msg)

(*
 * inl_info: the information used for inter-module or intra-module
 * inlining and specializations. Each access path is associated with
 * specific inlining-information. INL_NO means that there is no specific
 * information available so the dynamic access path must be used. INL_PRIM
 * means the access is actually a built-in primops. INL_LEXP refers to
 * an access whose implementation is memorized as an intermediate lambda
 * expression lexp; the expression might be containing free variables, but
 * they must have acc_paths of the form PATH(i1,...(PATH(i_n, EXTERN pid))).
 * INL_PATH means the current access shares the inlining information with
 * the one with acc_path. INL_STR means the current access is a module 
 * structure with proper inlining information for each of its components.
 *)
datatype inl_info
  = INL_PRIM of PO.primop * T.ty option
(*| INL_LEXP of FLINT.prog * T.ty option   (* should be lty option *) *)
  | INL_PATH of A.access * T.ty option
  | INL_STR of inl_info list
  | INL_NO


(****************************************************************************
 *                   UTILITY FUNCTIONS FOR INL_INFO                         *
 ****************************************************************************)

(** printing an inl_info object *)
fun prInfo (INL_PRIM (p, _)) = PO.prPrimop p
(*| prInfo (INL_LEXP _) = "<InlLexp>" *)
  | prInfo (INL_PATH (acc, _)) = A.prAcc(acc)
  | prInfo (INL_STR []) = "{}" 
  | prInfo (INL_STR (a::r)) = 
      let val r' = foldr (fn (i,s) => ("," ^ (prInfo i) ^ s)) "}" r
       in "{" ^ (prInfo a) ^ r'
      end
  | prInfo (INL_NO) = "<InlNo>"


(** selecting a component out of a structure info *)
fun selInfo (INL_STR sl, i) = 
      (List.nth(sl, i) handle Subscript => bug "Wrong field in INL_STR !")
  | selInfo (INL_NO, i) = INL_NO
  | selInfo _ = bug "Unexpected or un-implemented cases in selInfo"
        
(** checking if it is a primop *)
fun isPrimInfo (INL_PRIM _) = true
  | isPrimInfo _ = false

(** checking if a particular primop captures the continuations *)
fun isPrimCallcc (INL_PRIM (PO.CALLCC, _)) = true
  | isPrimCallcc (INL_PRIM (PO.CAPTURE, _)) = true
  | isPrimCallcc _ = false

(** checking if a particular primop can incur side-effects *)
fun pureInfo (INL_PRIM (PO.CAST, _)) = true 
  | pureInfo (INL_PRIM (p, _)) = false (* PO.purePrimop p *)
  | pureInfo _ = false

(** build a new primop info *)
fun mkPrimInfo x = INL_PRIM x

(** build a new access info *)
fun mkAccInfo x = INL_PATH x

(** build a new structure info *)
fun mkStrInfo x = INL_STR x

val nullInfo = INL_NO

end (* toplevel local *)
end (* structure InlInfo *)


(*
 * $Log: inlinfo.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:34  george
 * Version 110.5
 *
 *)
