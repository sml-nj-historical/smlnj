signature GRAPH_DISPLAY =
sig

   val suffix    : unit -> string
   val program   : unit -> string
   val visualize : (string -> unit) -> GraphLayout.layout -> unit

end

(*
 * $Log: graphDisplay.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:49:17  george
 *   Version 110.10
 *
 *)
