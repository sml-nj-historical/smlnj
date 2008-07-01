(* ens_var.sig *)

(* ENS short for ensemble *)
signature ENS_VAR =  sig

    val debugging : bool ref

    val add_var_def : VarCon.var -> int*int -> unit
    val add_var_use : VarCon.var -> int*int -> unit
    val add_var_inst : Types.ty -> Access.access -> unit
						    
    val add_type_def : Types.tycon -> int * int -> unit
    val add_type_use : Types.ty -> int * int -> unit
    val add_cons_use : Types.datacon -> int * int -> unit
    val add_cons_inst : Types.datacon -> Types.ty -> unit
						     
    val add_str_use : Modules.Structure -> int * int -> unit
    val add_mapping : Access.access -> int -> Access.access -> unit
    val add_str_def : Modules.Structure -> int * int -> Access.access -> unit
    val add_str_bnd : Modules.Structure -> Access.access -> Access.access -> 
		      int * int -> unit

    val add_sig_def : Modules.Signature -> int * int -> unit
    val add_sig_use : Symbol.symbol -> Modules.Signature -> int * int -> unit
    val add_sig_alias : Symbol.symbol -> Modules.Signature -> int * int -> unit
									   
    val print_var : unit -> unit
    val print_types : unit -> unit
    val print_cons : unit -> unit
    val print_str : unit -> unit
    val print_sig : unit -> unit
    val print_all : unit -> unit
    val print_ext : unit -> unit
			    
    val change_access_var : Access.access -> Access.access -> unit
    val change_access_str : Access.access -> Access.access -> unit
							      
    val set_source : string -> unit
    val set_eri : (Symbol.symbol -> string option) -> unit
    val set_pid : (PersStamps.persstamp -> unit)
    val add_lvar_ext : Access.access -> Access.access -> unit
    val clear : unit -> unit

    val give_all : unit -> Ens_types.all

end (* signature ENS_VAR*)
