(*
 * Insert various types of dummy blocks into the CFG
 *)

signature CONTROL_FLOW_GRAPH_RESTRUCTURE = 
sig

   structure Loop : LOOP_STRUCTURE

   val restructure : 
        ('n,'e,'g) Graph.graph * ('n,'e,'g) Loop.loop_structure -> 
             { add_preheader    : ({header  : 'n Graph.node,
                                    entries : 'e Graph.edge list
                                   } -> unit) option,
               add_landing_pad  : ({exit:'e Graph.edge} -> unit) option
             } -> unit

end

(*
 * $Log: cfg-restructure.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:47  george
 *   Version 110.10
 *
 *)
