(* Copyright 1996 by AT&T Bell Laboratories *)
(* env.sig *)

signature ENV =
sig
  structure Symbol : SYMBOL
  structure FastSymbol : FASTSYMBOL

  type 'b env
  exception Unbound  
  exception SpecialEnv

  val empty: 'b env
  val look: 'b env * Symbol.symbol -> 'b
  val bind: Symbol.symbol * 'b * 'b env -> 'b env

  val special: (Symbol.symbol -> 'b) * (unit -> Symbol.symbol list) -> 'b env
      (* Note: special(f,NONE) means Don't Memoize! *)

  val atop: 'b env * 'b env -> 'b env
      (* atop(e1,e2): place e1 on top of e2 *)

  val consolidate: 'b env -> 'b env
  val consolidateLazy: 'b env -> 'b env
  val app: (Symbol.symbol * 'b -> unit) -> 'b env -> unit
  val map: ('b -> 'b) -> 'b env -> 'b env
  val fold: ((Symbol.symbol * 'b) * 'a -> 'a) -> 'a -> 'b env -> 'a

  val symbols : 'b env -> Symbol.symbol list 
                                (* may contain duplicate symbols *)

end (* signature ENV *)


(*
 * $Log$
 *)
