(* hppaShuffle.sig -- shuffle src registers into destination registers *)

signature HPPASHUFFLE = sig
  structure I : HPPAINSTR
 
  type t = {regMap:int->int, temp:I.ea option, dst:int list, src:int list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end

(*
 * $Log: hppaShuffle.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
