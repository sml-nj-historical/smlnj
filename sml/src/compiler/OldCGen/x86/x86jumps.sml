(* x86jumps.sml
 * by Yngvi Guttesen (ysg@id.dth.dk) and Mark Leone (mleone@cs.cmu.edu)
 *
 * Copyright 1989 by	  Department of Computer Science, 
 *			  The Technical University of Denmak
 *			  DK-2800 Lyngby 
 *
 *)

structure X86Jumps : X86JUMPS = struct

datatype JumpKind = JMP | Jcc of int | LEA of int | LABPTR of int

datatype Size = SevenBits | Byte | Word | Long

fun die s = ErrorMsg.impossible ("x86/x86jumps.sml: " ^ s)

val wtoi = Word.toIntX
val itow = Word.fromInt

(* this should also know about unsigned qunatities *)
fun sizeint i =
    if i < 128 andalso i > ~129 then Byte
    else if i < 32768 andalso i > ~32769 then Word
    else Long

fun sizejump (LEA _, _, _, _) = 12
  | sizejump (LABPTR _, _, _, _) = 4
  | sizejump (Jcc _, _, s, d)	 =
	if sizeint(d-s-2)=Byte then 2 else 6
  | sizejump (JMP, _, s, d)    =
	if sizeint(d-s-2)=Byte then 2 else 5

fun signedbyte i = if i<0 then signedbyte (256+i) else i

fun ebyte i = if i>255 then die "ebyte: out of range"
	      else String.str(Char.chr(signedbyte i))
fun eword i = implode
  [Char.chr(wtoi (Word.andb(itow i, 0w255))), 
   Char.chr(wtoi (Word.andb(Word.>>(itow i, 0w8), 0w255)))]

fun elong i = 
      eword(wtoi (Word.andb(itow i, 0w65535))) ^ 
      eword(wtoi (Word.~>>(itow i, 0w16)))

val emitlong = elong

local
    structure W = Word32
in
    fun ebyteW32 w = if W.>(w,0w255) then die "ebyteW32: out of range"
	             else String.str(Char.chr(signedbyte(W.toInt w)))
    fun ewordW32 w = implode [
	    Char.chr(W.toInt(W.andb(w,0w255))),
	    Char.chr(W.toInt(W.andb(W.>>(w,0w8),0w255)))
	  ]
    fun elongW32 w = eword(W.toInt(W.andb(w,0w65535))) ^ 
	             eword(W.toInt(W.>>(w,0w16)))
    fun sizeintW32 w = if W.<(w,0wx80) then SevenBits
                       else if W.<=(w,0w255) then Byte
	               else if W.<=(w,0w65535) then Word
		       else Long
end

fun emitBackptr len =
      elongW32(X86Spec.ObjDesc.makeDesc(len, X86Spec.ObjDesc.tag_backptr))

fun emitjump (Jcc(cond), 2, s, d) =
	ebyte(112 + cond) ^ ebyte(d-s-2)
  | emitjump (Jcc(cond), 6, s, d) =
	ebyte(15) ^ ebyte(128 + cond) ^ elong(d-s-6)
  | emitjump (JMP, 2, s, d) = ebyte(235) ^ ebyte(d-s-2)
  | emitjump (JMP, 5, s, d) = ebyte(233) ^ elong(d-s-5)
  | emitjump (LABPTR i, 4, s, d) = elong(d-s+i)
  | emitjump (LEA(r), _, s, d) =
	ebyte(232) ^ elong(0) ^				(* call relative 0   *)
	ebyte(88 + r) ^					(* pop r	     *)
	ebyte(129) ^ ebyte(192 + r) ^ elong(d-s-5)	(* add r,(d-s-5)     *)
  | emitjump _ = die "emitjump: bad arg"



end (* structure X86Jumps *)

(*
 * $Log$
 *)
