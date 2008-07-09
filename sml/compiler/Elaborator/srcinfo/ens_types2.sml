structure Ens_types2 = 
struct
    type file = string

    type location = file * int * int

    datatype ty' = CONty of tycon' * ty' list
		 | IBOUND of int
	 and tycon' = GENtyc of { stamp : Stamps.stamp, 
				  eq : bool, 
				  name : Symbol.symbol,
				  cons : Symbol.symbol list
				}
		    | DEFtyc of { stamp : Stamps.stamp, 
				  name : Symbol.symbol
				}
		    | RECORDtyc of Symbol.symbol list 
		    | PRIMtyc of { stamp : Stamps.stamp, 
				   eq : bool, 
				   name : Symbol.symbol
				 }

    type type_elem' = { tycon : tycon',
			def : location, 
			usage : (location * Types.ty) list ref
		      }

    type cons_elem' = { name : Symbol.symbol,
			ty : ty',
			dataty : Stamps.stamp,
			def : location, 
			usage : (location * Types.ty) list ref
		      }
    local 
	structure T = Types
    in
        fun conv_ty ty = 
	    case ty of
		(T.WILDCARDty | T.UNDEFty) => 
		ErrorMsg.impossible "Ens_types2: conv_ty.1"
	      | T.IBOUND i => 
		IBOUND i
	      | T.VARty _ => (
		case TypesUtil.prune ty of
		    (*que faire dans ce cas la, lbound par ex?*)
		    T.VARty (ref (T.LBOUND {index, ...})) => IBOUND index
		  | T.VARty _ => ErrorMsg.impossible "Ens_types2: conv_ty.2"
		  | typ => conv_ty typ
		)
	      | T.POLYty {tyfun = T.TYFUN {body, ...}, ...} =>
		conv_ty body
	      | T.CONty (tycon, tyl) => 
		CONty (conv_tyc tycon, List.map conv_ty tyl)

	and conv_tyc tyc = 
	    case tyc of
		(T.ERRORtyc | T.FREEtyc _ | T.RECtyc _ | T.PATHtyc _) =>
		ErrorMsg.impossible "Ens_types2: conv_tyc.1"
	      | T.RECORDtyc ll => 
		RECORDtyc ll
	      | T.DEFtyc {stamp, path, ...} =>
		DEFtyc { stamp = stamp, 
			 name = InvPath.last path
		       }
	      | T.GENtyc { kind = T.DATATYPE { index, 
					       family = {mkey, members, ...},
					       ...
					     }, 
			   ...
			 } => ( case Vector.sub (members,index) of
				    {tycname, eq, dcons, ...} =>
				    GENtyc { stamp = mkey, 
					     eq = !eq = T.YES, 
					     name = tycname, 
					     cons = List.map #name dcons
					   }
			      )
	      | T.GENtyc {stamp, eq, path, kind = T.PRIMITIVE _, ...} =>
		PRIMtyc { stamp = stamp, 
			  eq = !eq = T.YES,
			  name = InvPath.last path
			}
	      | _ =>
		ErrorMsg.impossible "Ens_types2: conv_tyc.2"
    end

    type var_elem = { access : Access.access,
		      name : Symbol.symbol,
		      parent : Access.access,
		      typ : Types.ty,
		      def : location, 
		      usage : (location * Types.ty * Access.access) list ref
		    }

    type type_elem = { tycon : Types.tycon, 
		       def : location, 
		       usage : (location * Types.ty) list ref
		     }
		     
    type cons_elem = { name : Symbol.symbol,
		       typ : Types.ty,
		       gen_typ : Stamps.stamp * int,
		       def : location, 
		       usage : (location * Types.ty) list ref
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
