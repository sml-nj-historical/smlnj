structure Ens_types2 = 
struct
    type file = string

    type location = file * int * int

    type var_elem = { access : Access.access,
		      name : Symbol.symbol,
		      parent : Access.access,
		      typ : Types.ty,
		      def : location, 
		      usage : (location * Types.ty * Access.access) list ref}

    type type_elem = { tycon : Types.tycon, 
		       def : location, 
		       usage : (location * Types.ty) list ref}
		     
    type cons_elem = { name : Symbol.symbol,
		       typ : Types.ty,
		       gen_typ : Stamps.stamp * int,
		       def : location, 
		       usage : (location * Types.ty) list ref}
		     
    datatype key = Var of Access.access 		
		     | Str of Access.access 
		     | Type of Stamps.stamp 
		     | Cons of Stamps.stamp * int
		     | Sig of Stamps.stamp

    datatype elements = 
	     Def of (int * Symbol.symbol * key) list
	   | Constraint of (int * Symbol.symbol * int) list * Access.access
	   | Alias of Access.access
    (*
     * et aussi garder les access avec les use de variables
     * le mieux serait de garder toutes les structures, 
     * nommees ou pas, et eventuellement on peut essayer de
     * supprimer les structures anonymes
     *)
		      
    type str_elem = { name : Symbol.symbol, 
		      access : Access.access,
		      parent : Access.access option,
		      sign : Stamps.stamp option, (*pas les sign. inferee*)
		      def : location, 
		      elements : elements,
		      usage : location list ref}
		    
    datatype key_sig = Typ of Types.ty
		     | Sig of unit

    type sig_elem = { name : Symbol.symbol,
		      stamp : Stamps.stamp,
		      def : location, 
		      parent : Access.access option,
		      elements : (Symbol.symbol * key_sig) list ref,
		      alias : (location * Symbol.symbol) list ref, 
		      usage : (location * Symbol.symbol) list ref}
		    
    type all = var_elem list * 
	       type_elem list * 
	       cons_elem list * 
	       str_elem list * 
	       sig_elem list

    fun locFile ((f,_,_) : location) = f
end
