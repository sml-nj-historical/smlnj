(* codeString.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature CODESTRING = sig
  val init          : int -> unit
  val update        : int * Word8.word -> unit
  val getCodeString : unit -> Word8Vector.vector
end

structure CodeString : CODESTRING = struct
  structure WA=Word8Array

  val code = ref (WA.array(0,0w0))
  fun init n = code := WA.array(n,0w0) 
  fun update(n,v) = WA.update(!code,n,v) 
  fun getCodeString() = let
    val s = WA.extract(!code,0,SOME(WA.length(!code)))
  in
      code:=WA.array(0,0w0); s
  end
end


(*
 * $Log: codeString.sml,v $
 * Revision 1.1.1.1  1998/11/16 21:48:19  george
 *  Version 110.10
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
