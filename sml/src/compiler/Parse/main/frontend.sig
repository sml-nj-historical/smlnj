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
 * $Log: frontend.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:19  george
 * Version 110.5
 *
 *)
