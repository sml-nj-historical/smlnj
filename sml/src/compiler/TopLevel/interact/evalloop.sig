(* Copyright 1996 by Bell Laboratories *)
(* evalloop.sig *)
 
signature EVALLOOP =
sig
  exception Interrupt 

  val interact    : unit -> unit
  val evalStream  : string * TextIO.instream -> unit

  val installCompManager : (Ast.dec * EnvRef.envref -> unit) -> unit

end (* signature EVALLOOP *)
