(* Copyright 1996 by AT&T Bell Laboratories *)
(* env/statenv.sml *)

structure StaticEnv : STATICENV =
struct

local structure B  = Bindings
      structure E = Env
in 

type staticEnv = B.binding Env.env
type binding = B.binding

exception Unbound = E.Unbound
exception SpecialEnv = E.SpecialEnv

val empty = E.empty
val look = E.look
val bind = E.bind
val special = E.special
val atop = E.atop
val consolidate = E.consolidate
val consolidateLazy = E.consolidateLazy
val app = E.app
val map = E.map
val fold = E.fold

(* 
 * sort: sort the bindings in an environment.
 *  
 * This is used for the assignment of dynamic access slots in structure
 * elaborate, for printing, and for other purposes.
 * The bindings are sorted in the following order:
 *
 *   signatures
 *   functors
 *   structures
 *   types
 *   constructors
 *   values
 *   fixity declarations
 *
 * It is only correct to sort environments which have no duplicate bindings.
 * All routines which build structure environments maintain this
 * invariant, so it is ok to sort any structure environment using
 * this function.
 *)

fun sort env = Sort.sort B.binderGt (fold (op ::) nil env)

end (* local *)
end (* structure StaticEnv *)

(*
 * $Log$
 *)
