(* conversion.sml *)

signature CONVERSION = 
sig
   val to_stub : Types.tycon -> Ens_types2.stub_tycon
   val ty_to_ty' : Types.ty -> Ens_types2.ty'
   val tycon_to_tycon' : 
       Types.tycon -> Stamps.stamp * InvPath.path * Ens_types2.tycon'
   val sig_to_elem : Modules.Signature -> Ens_types2.sig_elem
end

local
    structure T = Types
    structure M = Modules
    structure MU = ModuleUtil
    open Ens_types2
in

structure Conversion : CONVERSION =
struct

    fun bug x = ErrorMsg.impossible ("Conversion: " ^ x)

    fun to_stub tycon = 
	case tycon of
	    T.RECORDtyc ll => 
	    Record ll
	  | T.DEFtyc {stamp, path, ...} =>
	    General (stamp, path)
	  | T.GENtyc {stamp, eq, path, ...} =>
	    General (stamp, path)
	  | T.PATHtyc {path, ...} =>
	    Path path
	  | (T.ERRORtyc | T.FREEtyc _ | T.RECtyc _) =>
	    ErrorMsg.impossible "Ens_types2: to_stub.1"
	    
    fun ty_to_ty' ty = 
	case ty of
	    T.IBOUND i => 
	    Ibound {index = i, depth = ~1} (* ?? *)
	  | T.VARty _ => (
	    case TypesUtil.prune ty of
		(*que faire dans ce cas la, lbound par ex?*)
		T.VARty (ref (T.LBOUND {index,depth,...})) => 
		Ibound {index=index,depth=depth}
	      | T.VARty (ref (T.UBOUND {name,...})) => Ubound name
	      | T.VARty _ => ErrorMsg.impossible "Ens_types2: ty_to_ty'.2"
	      | typ => ty_to_ty' typ
	    )
	  | T.POLYty {tyfun = T.TYFUN {arity, body}, ...} =>
	    Poly {arity = arity, body = ty_to_ty' body}
	  | T.CONty (tycon, tyl) =>
	    Conty (to_stub tycon, List.map ty_to_ty' tyl)
	  | (T.WILDCARDty | T.UNDEFty) => 
	    ErrorMsg.impossible "Ens_types2: ty_to_ty'.1"

    fun tycon_to_tycon' tyc = 
	case tyc of
	    (T.ERRORtyc | T.FREEtyc _ | T.RECtyc _ | 
	     T.PATHtyc _ | T.RECORDtyc _) =>
	    ErrorMsg.impossible "Ens_types2: tycon_to_tycon'.1"
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
	    ErrorMsg.impossible "Ens_types2: tycon_to_tycon'.2"
	    

    fun sig_to_elem (Modules.SIG {name, stamp, inferred, elements, ...}) = 
	let
	    (*fun conv_spec (M.VALspec {spec = ty, ...}) = 
		Val (ty_to_ty' ty)
	      (*| conv_spec 
		    (M.STRspec {def = SOME (M.CONSTstrDef str, _), ...}) =
		NamedStr (InvPath.last (MU.getStrName str), MU.getStrStamp str)
	      | conv_spec 
		    (M.STRspec {def = SOME (M.VARstrDef (sign, _), _), ...}) =
		InlineStr (#elements (sig_to_elem sign))*)
	      | conv_spec (M.STRspec {def = NONE, sign = (sign as M.SIG {name = NONE, ...}), ...}) = 
		InlineStr (#elements (sig_to_elem sign))
	      | conv_spec (M.STRspec {def = NONE, sign = (sign as M.SIG {name = SOME name, stamp, ...}), ...}) = 
		NamedStr (name, stamp)
	      | conv_spec _ =
		bug "non implemente"

	    fun conv (x, y) = (x, conv_spec y)*)

	    fun conv_spec (M.VALspec {spec = ty, ...}) = 
		SOME (Val (ty_to_ty' ty))
	      | conv_spec (M.STRspec {def = NONE, sign = (sign as M.SIG {name = NONE, ...}), ...}) = 
		SOME (InlineStr (#elements (sig_to_elem sign)))
	      | conv_spec (M.STRspec {def = NONE, sign = (sign as M.SIG {name = SOME name, stamp, ...}), ...}) = 
		SOME (NamedStr (name, stamp))
	      | conv_spec _ =
		NONE

	    fun conv (x, y) = 
		case conv_spec y of
		    NONE => NONE
		  | SOME y' => SOME (x, y')
	in
	    { name = case name of 
			 SOME s => s 
		       | NONE => Symbol.sigSymbol "<anonymousSig>",
	      stamp = stamp,
	      inferred = inferred,
	      def = ("", ~1, ~1),
	      elements = List.mapPartial conv elements,
	      alias = ref [],
	      usage = ref []
	    }
	end
    
end (* structure Conversion *)
end (* end local *)
