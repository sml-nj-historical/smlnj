(* x86coder.sig
 * by Yngvi Guttesen (ysg@id.dth.dk) and Mark Leone (mleone@cs.cmu.edu)
 *
 * Copyright 1989 by	  Department of Computer Science, 
 *			  The Technical University of Denmak
 *			  DK-2800 Lyngby 
 *
 *)

signature X86CODER = sig

eqtype Label
val newlabel : unit -> Label

datatype Size = SevenBits |            (* only used with Immed32 *)
	        Byte | Word | Long

datatype EA = Direct of int
	    | Displace of int * int
	    | Index of int * int * int * Size
	    | Immedlab of Label
	    | Immed of int
	    | Immed32 of Word32.word
	    | Floatreg of int

(*************** The 80386 registers ****************************)

val eax : int
val ebx : int
val ecx : int
val edx : int
val esi : int
val edi : int
val ebp : int
val esp : int

(**************** Misc. functions *******************************)

val comment    : string -> unit
val finish     : unit	-> Word8Vector.vector
val align      : unit	-> unit
val mark       : unit	-> unit
val define     : Label	-> unit

(***************** Emitters *************************************)

val emitstring : string -> unit
val realconst : string -> unit
val emitlong : int -> unit
val emitlab : int * Label -> unit

(***************** Memory functions *****************************)

val movl  : EA * EA -> unit
val movb  : EA * EA -> unit
val movzx : EA * EA -> unit
val stos  : EA	    -> unit
val lea	  : EA * EA -> unit
val push  : EA	    -> unit
val pop	  : EA	    -> unit
val xchg  : EA * EA -> unit

(***************** Logical functions ****************************)

val orl	 : EA * EA -> unit
val notl : EA	   -> unit
val andl : EA * EA -> unit
val xorl : EA * EA -> unit
val btst : EA * EA -> unit

(**************** Arithmetic functions *************************)

val incl  : EA -> unit
val decl  : EA -> unit
val addl  : EA * EA -> unit
(* val addl2 : EA * EA -> unit *)
val subl  : EA * EA -> unit
val negl  : EA	    -> unit
val cmpl  : EA * EA -> unit
val asrl  : EA * EA -> unit
val asll  : EA * EA -> unit
val lsrl  : EA * EA -> unit
val idivl : EA	    -> unit
val udivl : EA	    -> unit
val mull  : EA * EA -> unit
val mullExtend : EA * EA -> unit  (* sign-extend *)
val cdq	  : unit    -> unit

(**************** Jumps ****************************************)

val jra : EA -> unit
val jmp : EA -> unit

val jne : EA -> unit
val jeq : EA -> unit
val jgt : EA -> unit
val jge : EA -> unit
val jlt : EA -> unit
val jle : EA -> unit
val jb	: EA -> unit
val jbe : EA -> unit
val ja	: EA -> unit
val jae : EA -> unit
val jc	: EA -> unit
val jnc : EA -> unit
val jp  : EA -> unit
val jnp : EA -> unit

(****************** Floating point functions **********************)

val fadd  : bool -> EA * EA -> unit
val fsub  : bool -> EA * EA -> unit
val fsubr : bool -> EA * EA -> unit
val fmul  : bool -> EA * EA -> unit
val fcom  : bool -> EA * EA -> unit
val fucom : bool -> EA * EA -> unit
val fdiv  : bool -> EA * EA -> unit
val fdivr : bool -> EA * EA -> unit
val fst	  : bool -> EA -> unit
val fld	  : EA	 -> unit
val fild  : EA	 -> unit
val fchs  : unit -> unit
val fabs  : unit -> unit
val fstsw : unit -> unit
val fnstsw: unit -> unit

(***************** Misc. functions ********************************)

val sahf   : unit -> unit
val into   : unit -> unit

end (* signature X86CODER *)



(*
 * $Log: x86coder.sig,v $
 * Revision 1.1.1.1  1997/01/14 01:38:50  george
 *   Version 109.24
 *
 *)
