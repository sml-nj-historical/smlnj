signature BBSCHED = sig
  structure F : FLOWGRAPH

  val bbsched : F.cluster -> unit
  val finish : unit -> unit
  val cleanUp : unit -> unit
end

(*
 * $Log: bbsched.sig,v $
 * Revision 1.1.1.1  1997/04/19 18:14:20  george
 *   Version 109.27
 *
 *)
