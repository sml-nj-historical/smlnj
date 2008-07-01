structure Ens_types = 
struct
    type file = string

    type location = file * int * int

    type var_elem = {var : VarCon.var, 
		     def : location, 
		     usage : (location * Types.ty) list ref}

    type type_elem = {tycon : Types.tycon, 
		      def : location, 
		      usage : (location * Types.ty) list ref}
		     
    type cons_elem = {cons : Types.datacon, 
		      def : location, 
		      usage : (location * Types.ty) list ref}
		     
    type str_elem = {str : Modules.Structure, 
		     def : location, usage : (location * Types.ty) list ref, 
		     map : (int * Access.access) list ref}
		    
    type sig_elem = {sign : Modules.Signature, 
		     def : location, 
		     alias : (location * Symbol.symbol) list ref, 
		     usage : (location * Symbol.symbol) list ref}
		    
    type all = var_elem list * 
	       type_elem list * 
	       cons_elem list * 
	       str_elem list * 
	       sig_elem list

    fun locFile ((f,_,_) : location) = f
end
