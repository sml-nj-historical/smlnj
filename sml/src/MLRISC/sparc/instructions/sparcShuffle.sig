(* sparcShuffle.sig -- shuffle src registers into destination registers *)

signature SPARCSHUFFLE = sig
  structure I : SPARCINSTR
 
  type t = {tmp:I.ea option, dst:I.C.cell list, src:I.C.cell list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end

