(* tyvarset.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature TYVARSET = 
sig 
  type tyvarset
  val empty : tyvarset
  val singleton : Types.tyvar -> tyvarset
  val mkTyvarset : Types.tyvar list -> tyvarset
  val union : tyvarset * tyvarset * ErrorMsg.complainer -> tyvarset
  val diff : tyvarset * tyvarset * ErrorMsg.complainer -> tyvarset
  val diffPure : tyvarset * tyvarset -> tyvarset
  val elements: tyvarset -> Types.tyvar list
end (* signature TYVARSET *)

structure TyvarSet :> TYVARSET =
struct

local 
  structure EM = ErrorMsg
  open Types 
  fun bug msg = ErrorMsg.impossible("TyvarSet: "^msg)
in

type tyvarset = tyvar list

val empty = nil
fun singleton t = [t]
fun mkTyvarset l = l
fun elements s = s

fun mem(a as ref(UBOUND{name=name_a,eq=eq_a,depth=depth_a}), 
	(b as ref(UBOUND{name=name_b,eq=eq_b,depth=depth_b}))::rest,err) =
      if a=b then true
      else if Symbol.eq(name_a,name_b) then
	    (if eq_a<>eq_b then
		err EM.COMPLAIN ("type variable " ^ (Symbol.name name_a) ^
			      " occurs with different equality properties \
			       \in the same scope")
		    EM.nullErrorBody
		else ();
	     if depth_a<>depth_b then bug "mem - depths differ" else ();
		(* UBOUND tyvars are created with depth infinity and
		 * this should not change until type checking is done *)
	     a := INSTANTIATED(VARty b);
	     true)
      else mem(a,rest,err)
  | mem _ = false

fun memP(a as ref(UBOUND{name=name_a,...}), 
	 (b as ref(UBOUND{name=name_b,...}))::rest) =
      if a=b then true
      else if Symbol.eq(name_a,name_b) then true
      else memP(a,rest)
  | memP _ = false

fun union([],s,err) = s
  | union(s,[],err) = s
  | union(a::r,s,err) =
     if mem(a,s,err) then union(r,s,err)
     else a::union(r,s,err)

fun diff(s,[],err) = s
  | diff([],_,err) = []
  | diff(a::r,s,err) =
     if mem(a,s,err) then diff(r,s,err)
     else a::diff(r,s,err)

fun diffPure(s,[]) = s
  | diffPure([],_) = []
  | diffPure(a::r,s) =
     if memP(a,s) then diffPure(r,s)
     else a::diffPure(r,s)

end (* local *)
end (* abstraction TyvarSet *)

(*
 * $Log: tyvarset.sml,v $
 * Revision 1.5  1997/09/05  04:43:48  dbm
 *   Changes in names of exported values.  New function diffPure. (bug 1246)
 *
 * Revision 1.4  1997/04/14  21:31:43  dbm
 *   Added new function mktyvarset that was needed in ElabCore in the
 *   elaboration of explicit type variable bindings.
 *
 * Revision 1.3  1997/03/22  18:18:43  dbm
 * Modified for changes to tyvar representation introduced for better
 * handling of overloaded literals and fix for bug 905/952.
 *
 * Revision 1.2  1997/01/31  20:39:57  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:35  george
 *   Version 109.24
 *
 *)
