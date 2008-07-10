structure Ens_types2 = 
struct
    type file = string

    type location = file * int * int

    datatype stub_tycon = General of Stamps.stamp * InvPath.path
			| Record of Symbol.symbol list

    datatype ty' = Conty of stub_tycon * ty' list
		 | Ibound of int

    datatype tycon' = Datatype of bool * Symbol.symbol list (*eq * cons list*)
		    | Abstract of Symbol.symbol list
		    | Deftyc
		    | Primtyc of bool (* : eq *)

    local 
	structure T = Types
    in
	fun conv_stub_tycon tycon = 
	    case tycon of
		T.RECORDtyc ll => 
		Record ll
	      | T.DEFtyc {stamp, path, ...} =>
		General (stamp, path)
	      | T.GENtyc {stamp, eq, path, ...} =>
		General (stamp, path)
	      | (T.ERRORtyc | T.FREEtyc _ | T.RECtyc _ | T.PATHtyc _) =>
		ErrorMsg.impossible "Ens_types2: conv_stub_tycon.1"
	      
        fun conv_ty ty = 
	    case ty of
		T.IBOUND i => 
		Ibound i
	      | T.VARty _ => (
		case TypesUtil.prune ty of
		    (*que faire dans ce cas la, lbound par ex?*)
		    T.VARty (ref (T.LBOUND {index, ...})) => Ibound index
		  | T.VARty _ => ErrorMsg.impossible "Ens_types2: conv_ty.2"
		  | typ => conv_ty typ
		)
	      | T.POLYty {tyfun = T.TYFUN {body, ...}, ...} =>
		conv_ty body
	      | T.CONty (tycon, tyl) =>
		Conty (conv_stub_tycon tycon, List.map conv_ty tyl)
	      | (T.WILDCARDty | T.UNDEFty) => 
		ErrorMsg.impossible "Ens_types2: conv_ty.1"

	fun conv_tyc tyc = 
	    case tyc of
		(T.ERRORtyc | T.FREEtyc _ | T.RECtyc _ | 
		 T.PATHtyc _ | T.RECORDtyc _) =>
		ErrorMsg.impossible "Ens_types2: conv_tyc.1"
	      | T.DEFtyc {stamp, path, ...} =>
		(stamp, path, Deftyc)
	      | T.GENtyc { kind = T.DATATYPE { index, 
					       family = {mkey, members, ...},
					       ...
					     }, 
			   ...
			 } => 
		let val {tycname, eq, dcons, ...} = Vector.sub (members,index)
		in
		    ( mkey, 
		      InvPath.IPATH [tycname], 
		      Datatype (!eq = T.YES, List.map #name dcons)
		    )
		end
	      | T.GENtyc {stamp, eq, path, kind = T.PRIMITIVE _, ...} =>
		(stamp, path, Primtyc (!eq = T.YES))
	      | T.GENtyc {stamp, path, kind = T.ABSTRACT (T.GENtyc {kind = T.DATATYPE {index, family = {members, ...}, ...}, ...}), ...} => 
		let val {tycname, dcons, ...} = Vector.sub (members,index)
		in
		    ( stamp,
		      path,
		      Abstract (List.map #name dcons)
		    )
		end
	      | _ =>
		ErrorMsg.impossible "Ens_types2: conv_tyc.2"

    end

    type var_elem = { access : Access.access,
		      name : Symbol.symbol,
		      parent : Access.access,
		      typ : ty',
		      def : location, 
		      usage : (location * ty' * Access.access) list ref
		    }

    (*type type_elem = { tycon : Types.tycon, 
		       def : location, 
		       usage : (location * Types.ty) list ref
		     }
		     
    type cons_elem = { name : Symbol.symbol,
		       typ : Types.ty,
		       gen_typ : Stamps.stamp * int,
		       def : location, 
		       usage : (location * Types.ty) list ref
		     }*)
		     
    type type_elem = { tycon : tycon',
		       stamp : Stamps.stamp,
		       name : Symbol.symbol,
		       def : location, 
		       usage : location list ref
		     }
		     
    type cons_elem = { name : Symbol.symbol,
		       ty : ty',
		       dataty : Stamps.stamp,
		       def : location, 
		       usage : (location * ty') list ref
		     }

    datatype key = Var of Access.access 		
		 | Str of Access.access 
		 | Type of Stamps.stamp 
		 | Cons of Stamps.stamp * Symbol.symbol
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
		      usage : location list ref
		    }
		    
    datatype key_sig = Typ of Types.ty
		     | Sig of unit

    type sig_elem = { name : Symbol.symbol,
		      stamp : Stamps.stamp,
		      def : location, 
		      parent : Access.access option,
		      elements : (Symbol.symbol * key_sig) list ref,
		      alias : (location * Symbol.symbol) list ref, 
		      usage : (location * Symbol.symbol) list ref
		    }
		    
    type all = var_elem list * 
	       type_elem list * 
	       cons_elem list * 
	       str_elem list * 
	       sig_elem list

    fun locFile ((f,_,_) : location) = f
end
