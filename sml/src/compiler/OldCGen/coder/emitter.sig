(* emitter.sig
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * This is the signature of the assembler and machine code emitters.
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *
 *)

signature EMITTER =
sig
    datatype 'label info = INFO of {addrOf: 'label -> int, 
				    nameOf: 'label->string}
    type 'label instruction

    val emitLong : int -> unit              (* emit an integer constant *)
    val emitString : string -> unit         (* emit a (padded) string constant *)
    val emitReal : string -> unit           (* emit a real constant *)
(* NOTE: the following is useful for asm code, but could be replaced by
 * emitLong. *)
    val emitAddr : 'label info -> ('label * int) -> unit    
					 (* emit a label value (with offset) *)

    val define : 'label info -> 'label -> unit   (* define a label *)
    val mark : unit -> unit                 (* emit a back-pointer mark *)

    val emitInstr : 'label info -> 'label instruction -> unit
					     (* emit an instruction *)

    val comment : string -> unit

    val init : int -> unit                  (* initialize to emit n bytes of code *)

end (* signature EMITTER *)

(*
 * $Log: emitter.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:49  george
 * Version 110.5
 *
 *)
