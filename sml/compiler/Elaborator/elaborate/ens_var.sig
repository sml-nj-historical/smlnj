signature ENS_VAR =  sig

   val add_var_def : VarCon.var -> int*int -> unit
   val add_var_use : Access.access -> int*int -> unit
   val add_var_inst : (Types.ty * Access.access) -> unit

   val add_type_def : Types.tycon -> int * int -> unit
   val add_type_use : Types.ty -> int * int -> unit

   val add_cons_use : Types.datacon -> int * int -> unit
   val add_cons_inst : Types.datacon -> Types.ty -> unit

   val add_str_def : Modules.Structure -> int * int -> Access.access -> unit
   val add_str_bnd : Modules.Structure -> Access.access -> Access.access -> unit
   val add_mapping : Access.access -> int -> Access.access -> unit


   val change_access_var : Access.access -> Access.access -> unit
   val change_access_str : Access.access -> Access.access -> unit
 

   val print_ens : unit -> unit
   val print_types : unit -> unit
   val print_cons : unit -> unit
   val print_str : unit -> unit


   val maj : StaticEnv.staticEnv -> unit
   val clear : unit -> unit
		       
   type var = {var : VarCon.var, def : int * int, usage : (int * int) list ref, instance : Types.ty list ref};
   type ty = {tycon : Types.tycon, def : int * int, usage : (int * int) list ref, instance : Types.ty list ref};
   type cons = {cons : Types.datacon, def: int * int, usage : (int * int) list ref, instance : Types.ty list ref};
   type str = {str : Modules.Structure, def: int * int, usage : (int * int) list ref, instance : Types.ty list ref,
	       map : (int * Access.access) list ref}
   datatype id = Type of ty | Constructor of cons | Variable of var | Structure of str | Error;

   val find : string -> int -> id
end
