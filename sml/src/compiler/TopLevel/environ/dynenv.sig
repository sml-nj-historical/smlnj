(* Copyright 1996 by Bell Laboratories *)
(* dynenv.sig *)

signature DYNENV =
sig
  type object = Unsafe.Object.object
  type dynenv
  exception Unbound  
  exception SpecialEnv
  val empty: dynenv
  val special: (PersStamps.persstamp -> object) * dynenv -> dynenv
  val look: dynenv -> PersStamps.persstamp -> object
  val bind: PersStamps.persstamp * object * dynenv -> dynenv

  val atop: dynenv * dynenv -> dynenv (* atop(e1,e2): place e1 on top of e2 *)

  val remove: PersStamps.persstamp list * dynenv -> dynenv
  val consolidate: dynenv -> dynenv
  val singleton: PersStamps.persstamp * object -> dynenv

end (* signature DYNENV *)


