(* Copyright 1996 by AT&T Bell Laboratories *)
(* frontend.sig *)

signature FRONT_END = 
sig
  datatype parseResult
    = EOF   
    | ERROR 
    | ABORT 
    | PARSE of Ast.dec

  val parse : Source.inputSource -> unit -> parseResult

end (* signature FRONT_END *)


(*
 * $Log$
 *)
