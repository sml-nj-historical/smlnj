(* conversion.sml *)

(* module used to convert from internal representations to simplified one*)
signature CONVERSION =
sig
   val to_stub : Types.tycon -> DBTypes.stub_tycon
   val ty_to_ty' : Types.ty -> DBTypes.ty'
   val tycon_to_tycon' :
       Types.tycon -> Stamps.stamp * InvPath.path * DBTypes.tycon'
   val sig_to_elem : Modules.Signature -> DBTypes.sig_elem
end

local
    structure T = Types
    structure M = Modules
    structure MU = ModuleUtil
    open DBTypes
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
	    bug "to_stub.1"

    fun ty_to_ty' ty =
	case ty of
	    T.IBOUND i =>
	    Ibound i
	  | T.VARty _ =>
	    let fun strip (T.MARKty (ty, _)) = strip ty
		  | strip ty = ty
	    in case strip (TypesUtil.prune ty) of
		   T.VARty (ref (T.LBOUND {index,depth,...})) =>
		   Lbound {index=index,depth=depth}
		 | T.VARty (ref (T.UBOUND {name,...})) => Ubound name
		 | T.VARty (ref (T.OPEN _)) =>
		   bug "ty_to_ty'.2 OPEN"
		 | T.VARty (ref (T.LITERAL _)) =>
		   bug "ty_to_ty'.2 LITERAL"
		 | T.VARty _ => bug "ty_to_ty'.2"
		 | typ => ty_to_ty' typ
	    end
	   | T.POLYty {tyfun = T.TYFUN {arity, body}, ...} =>
	       Poly {arity = arity, body = ty_to_ty' body}
	   | T.CONty (tycon, tyl) =>
	       Conty (to_stub tycon, List.map ty_to_ty' tyl)
	   | T.MARKty (ty, _) => ty_to_ty' ty
	   | (T.WILDCARDty | T.UNDEFty) =>
	       bug "ty_to_ty'.1"

    fun tycon_to_tycon' tyc =
	case tyc of
	    (T.ERRORtyc | T.FREEtyc _ | T.RECtyc _ |
	     T.PATHtyc _ | T.RECORDtyc _) =>
	    bug "tycon_to_tycon'.1"
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
	  | T.GENtyc {stamp, path,
		      kind = T.ABSTRACT
				 (T.GENtyc {kind = T.DATATYPE
						       {index,
							family = {members,...},
							...},
					    ...}),
		      ...} =>
	    let val {tycname, dcons, ...} = Vector.sub (members,index)
	    in
		( stamp,
		  path,
		  Abstract (List.map #name dcons)
		)
	    end
	  | _ =>
	    bug "tycon_to_tycon'.2"


    fun sig_to_elem (Modules.SIG {name,stamp,inferred,elements,...}):sig_elem=
	let
	    fun conv_spec (M.VALspec {spec = ty, ...}) =
		SOME (Val (ty_to_ty' ty))
	      | conv_spec (M.STRspec {def = NONE,
				      sign = (sign as M.SIG {name = NONE,...}),
				      ...}) =
		( case #elements (sig_to_elem sign) of
		      AliasSig _ => bug "sig_to_elem0"
		    | DefSig defsig => SOME (InlineStr defsig)
		)
	      | conv_spec (M.STRspec {def = NONE,
				      sign = (sign as M.SIG {name = SOME name,
							     stamp, ...}),
				      ...}) =
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
	      def = ("", ~1, ~1),    (* DBM: needs to be properly defined using
				      * marked sig *)
	      elements = DefSig (List.mapPartial conv elements),
	      usage = ref []
	    }
	end
      | sig_to_elem M.ERRORsig =
	bug "sig_to_elem"

end (* structure Conversion *)
end (* end local *)
