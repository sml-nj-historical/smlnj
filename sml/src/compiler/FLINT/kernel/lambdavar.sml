(* Copyright 1996 by AT&T Bell Laboratories *)
(* lambdavar.sml *)

structure LambdaVar : LAMBDA_VAR = 
struct

local 

structure S = Symbol
structure IM = Intmap

in

fun inc r = r := !r + 1
fun newLvar r () = (inc r; !r)
val varcount = ref 0

exception NoLvarName
val lvarNames : string IM.intmap = IM.new(32, NoLvarName)
val name = IM.map lvarNames
val giveLvarName = IM.add lvarNames

type lvar = int      (* lambda variable id number *)

val saveLvarNames = Control.saveLvarNames
fun lvarIsNamed lv = (name lv; true) handle NoLvarName => false
fun prLvar(lvar:lvar) = Int.toString(lvar)

fun sameName(v,w) =
      if !saveLvarNames
      then giveLvarName(v,name w) handle NoLvarName => 
                (giveLvarName(w, name v) handle NoLvarName => ())
      else ()

val mkLvar = newLvar varcount

fun clear () = (varcount := 0; IM.clear lvarNames)

fun dupLvar v =
  let val nv = mkLvar()
   in if !saveLvarNames then 
        (giveLvarName(nv,name v) handle NoLvarName => ())
      else ();
      nv
  end

fun namedLvar(id: S.symbol) =
  let val nv = mkLvar()
   in if !saveLvarNames then giveLvarName(nv,S.name id) else ();
      nv
  end

fun lvarSym(lv : lvar) : S.symbol option 
  = SOME (S.varSymbol (name lv)) handle NoLvarName => NONE

fun lvarName(lv : lvar) : string =
  let val s = Int.toString lv
   in (name lv ^ s) handle NoLvarName => ("v" ^ s)
  end

end (* local *)

end (* structure LambdaVar *)

(*
 * $Log: lambdavar.sml,v $
 * Revision 1.1.1.1  1997/01/14  01:38:10  george
 *   Version 109.24
 *
 *)
