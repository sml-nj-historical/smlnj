(* Copyright 1992 by AT&T Bell Laboratories *)
(* env/statenv.sig *)

signature STATICENV =
sig

  type staticEnv
  type binding
  exception Unbound  
  exception SpecialEnv

  val empty: staticEnv
  val look: staticEnv * Symbol.symbol -> binding
  val bind: Symbol.symbol * binding * staticEnv -> staticEnv
  val special: (Symbol.symbol -> binding) * (unit -> Symbol.symbol list)
                  -> staticEnv

  val atop: staticEnv * staticEnv -> staticEnv
  val consolidate: staticEnv -> staticEnv
  val consolidateLazy: staticEnv -> staticEnv
  val app: (Symbol.symbol * binding -> unit) -> staticEnv -> unit
  val map: (binding -> binding) -> staticEnv -> staticEnv
  val fold: ((Symbol.symbol * binding) * 'a -> 'a) -> 'a -> staticEnv -> 'a
  val sort: staticEnv -> (Symbol.symbol * binding) list

end (* signature STATICENV *)


(*
 * $Log: statenv.sig,v $
 * Revision 1.2  1997/08/15  20:40:48  dbm
 *   Introduced new consolidateLazy, used by Environment.concatEnv.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:37  george
 *   Version 109.24
 *
 *)
