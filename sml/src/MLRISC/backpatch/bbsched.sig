signature BBSCHED = sig
  structure F : FLOWGRAPH

  val bbsched : F.cluster -> unit
  val finish : unit -> unit
  val cleanUp : unit -> unit
end

(*
 * $Log: bbsched.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:47:14  george
 *   Version 110.10
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
