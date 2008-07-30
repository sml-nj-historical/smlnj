signature DATABASE = 
sig
    (* variables *)
    val add_var_def : 
	VarCon.var -> 
	int*int -> 
	{name: Symbol.symbol, str: Modules.Structure, def: Absyn.strexp} -> 
	unit
    val add_var_use : VarCon.var -> int*int -> Types.tyvar list -> unit
    val print_var : unit -> unit

    (* tycons *)
    val add_ty_def : Types.tycon -> int*int -> unit
    val add_ty_use : Types.ty -> int*int -> unit
    val print_ty : unit -> unit
    val print_cons : unit -> unit

    (* signatures *)
    val add_sig_def : Modules.Signature -> int*int -> unit
    val print_sig : unit -> unit

    (* structures *)
    val add_str_def : 
	{name: Symbol.symbol, str: Modules.Structure, def: Absyn.strexp} -> 
	Bindings.binding list option -> 
	Bindings.binding list -> 
	int*int ->
	Access.access option -> 
	unit
    val add_str_alias : 
	{name: Symbol.symbol, str: Modules.Structure, def: Absyn.strexp} -> 
	Modules.Structure -> 
	int*int -> 
	int*int -> 
	Access.access option -> 
	unit
    val add_str_sig_alias : 
	{name: Symbol.symbol, str: Modules.Structure, def: Absyn.strexp} -> 
	Modules.Structure ->
	Bindings.binding list -> 
	int*int -> 
	int*int -> 
	Access.access option -> 
	unit
    val add_str_use : Modules.Structure -> int*int -> unit
    val print_str : unit -> unit

    (* external references (structures only) *)
    val add_lvar : Access.access -> unit
    val add_ext_acc : Access.access -> unit

    val print_all : unit -> unit
    val print_all_g : unit -> unit
    val set_source : string -> unit
    val pickling_over : unit -> unit
    val set_eri : (Symbol.symbol -> string option) -> unit
    val set_pid : PersStamps.persstamp -> unit
    val clear_lvar : unit -> unit
    val clear : unit -> unit
    val clear_all : unit -> unit
    (*val save : unit -> unit*)
    val load_merge : string -> unit
    val merge_pickle : string -> string -> unit
    val test : unit -> unit
    val get_pickle : unit -> string
    val remove : string -> unit

    (* query support functions *)
    val find_var : (DBTypes.var_elem -> bool) -> DBTypes.var_elem option
    val exists_var : 
	(DBTypes.var_elem -> bool) -> bool
    val filter_var : 
	(DBTypes.var_elem -> bool) -> DBTypes.var_elem list

    val find_str : (DBTypes.str_elem -> bool) -> DBTypes.str_elem option
    val exists_str : 
	(DBTypes.str_elem -> bool) -> bool
    val filter_str : 
	(DBTypes.str_elem -> bool) -> DBTypes.str_elem list

    val find_typ : 
	(DBTypes.type_elem -> bool) -> DBTypes.type_elem option
    val exists_typ : 
	(DBTypes.type_elem -> bool) -> bool
    val filter_typ : 
	(DBTypes.type_elem -> bool) -> DBTypes.type_elem list

    val find_cons : 
	(DBTypes.cons_elem -> bool) -> DBTypes.cons_elem option
    val exists_cons : 
	(DBTypes.cons_elem -> bool) -> bool
    val filter_cons : 
	(DBTypes.cons_elem -> bool) -> DBTypes.cons_elem list

    val find_sig : (DBTypes.sig_elem -> bool) -> DBTypes.sig_elem option
    val exists_sig : 
	(DBTypes.sig_elem -> bool) -> bool
    val filter_sig : 
	(DBTypes.sig_elem -> bool) -> DBTypes.sig_elem list

    (* access translation: from filepath and path to definition point lvar 
     * (or lvar to itself) *)
    val get_str_lvar_g : string -> Access.access -> Access.access
    val get_var_lvar_g : string -> Access.access -> Access.access

    (* map a (charpos,filename) pair to the next symbol occurrence at that 
     * position in the file *)
    val charposToOccurrence : int * string -> DBTypes.occurrence


end (* signature DATABASE *)
