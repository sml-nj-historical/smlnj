signature ENS_VAR =  sig

   val add_var_def : VarCon.var -> int*int -> unit
   val add_var_use : Access.access -> int*int -> unit
   val add_var_inst : (Types.ty * Access.access) -> unit
   val add_type_def : Types.tycon -> int * int -> unit
   val add_type_use : Types.ty -> int * int -> unit
   val add_cons_use : Types.datacon -> int * int -> unit
   val add_cons_inst : Types.datacon -> Types.ty -> unit

   val clear : unit -> unit
   val clear_it : unit -> unit
   val clear_intern : unit -> unit

   val print_ens : unit -> unit
   val print_types : unit -> unit
   val print_cons : unit -> unit

   val change_access : Access.access -> Access.access -> unit

   val maj : StaticEnv.staticEnv -> unit

end
