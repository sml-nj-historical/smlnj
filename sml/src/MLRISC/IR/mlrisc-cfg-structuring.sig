signature CFG_STRUCTURING =
sig

   structure IR : MLRISC_IR
 
   val reshape : IR.IR ->
                 { add_preheader        : bool,
                   split_critical_edges : bool
                 } -> unit

end

(*
 * $Log: mlrisc-cfg-structuring.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:46:54  george
 *  Version 110.10
 *
 *)
