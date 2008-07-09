structure Ens_var2 = struct
local 
    structure A = Access
    structure VC = VarCon
    structure T = Types
    structure S = Symbol
    structure M = Modules
    structure B = Bindings
    structure EP = Ens_print2
    open Ens_types2
in
    fun bug msg = ErrorMsg.impossible("Bugs in Ens_var2: "^msg);

    fun loc_reg (r1, r2) = (("", r1, r2):location)

    fun compare_acc (A.LVAR i, A.LVAR j) = Int.compare (i,j)
      | compare_acc (A.LVAR _, _) = LESS
      | compare_acc (_, A.LVAR _) = GREATER
      | compare_acc (A.EXTERN s1, A.EXTERN s2) = 
	PersStamps.compare (s1, s2)
      | compare_acc (A.EXTERN _, _) = LESS
      | compare_acc (_, A.EXTERN _) = GREATER
      | compare_acc (A.PATH (acc1', s1), A.PATH (acc2', s2)) = 
	( case compare_acc (acc1', acc2') of
	      EQUAL => Int.compare (s1, s2)
	    | ord => ord
	)
      | compare_acc (A.PATH _, _) = LESS
      | compare_acc (_, A.PATH _) = GREATER
      | compare_acc (A.NO_ACCESS, A.NO_ACCESS) = EQUAL

    fun print_ty (ty:T.ty) = 
	case ty of
	    T.VARty (ref v) => (
	    case v of
		T.INSTANTIATED ty => 
		(print "(instantiated "; print_ty ty; print ")")
	      | T.OPEN _ => print "open"
	      | T.UBOUND _ => print "ubound"
	      | T.LITERAL _ => print "literal"
	      | T.SCHEME _ => print "scheme"
	      | T.LBOUND _ => print "lbound"
	    )
	  | T.IBOUND i => print ("(ibound " ^ Int.toString i ^ ")")
	  | T.CONty (tyc, tyl) => 
	    ( print "(conty ";
	      print_tyc tyc;
	      print ", ";
	      List.app print_ty tyl;
	      print ")"
	    )
	  | T.POLYty {tyfun = T.TYFUN {body, ...}, ...} => 
	    (print "(polyty "; print_ty body; print ")")
	  | _ => print "other_ty"

    and print_tyc (tyc:T.tycon) = (
	case tyc of
	    T.GENtyc _ => print "gentyc"
	  | T.DEFtyc _ => print "deftyc"
	  | T.RECORDtyc _ => print "recordtyc"
	  | _ => print "other_tyc";
	print ("_" ^ EP.stoS (TypesUtil.tycName tyc))
    )

    fun print_ty' ty = 
	case ty of
	    CONty (tycon, tyl) => (
	    print "(CONty ";
	    print_tyc' tycon;
	    print ", ";
	    List.app print_ty' tyl;
	    print ")"
	    )
	  | IBOUND i => print ("(ibound " ^ Int.toString i ^ ")")

    and print_tyc' tyc = 
	case tyc of
	    GENtyc  {name, ...} => print ("(gentyc "  ^ EP.stoS name ^ ")")
	  | PRIMtyc {name, ...} => print ("(primtyc " ^ EP.stoS name ^ ")")
	  | DEFtyc  {name, ...} => print ("(deftyc "  ^ EP.stoS name ^ ")")
	  | RECORDtyc ll => 
	    ( print "(recordtyc"; 
	      List.app (fn x => print (" " ^ EP.stoS x)) ll; 
	      print ")")

    fun print_ty'' (ty : ty') : unit = 
	case ty of
	    CONty (RECORDtyc [], []) => print "unit"
	  | CONty (RECORDtyc ((Symbol.SYMBOL (_, "1"))::_), tyl) =>
	    let fun p [] = ErrorMsg.impossible "Ens_var2: print_ty''.1"
		  | p [x] = print_ty'' x
		  | p (x::y) = (print_ty'' x; print " * "; p y)
	    in
		p tyl
	    end
	  | CONty (RECORDtyc ll, tyl) =>
	    ( print "{";
	      List.app 
		  (fn (x, y) => 
		      (print (EP.stoS x ^ ":"); print_ty'' y; print ", "))
		  (ListPair.zip (ll, tyl)); 
	      print "}"
	    )
	  | CONty (tycon, []) => 
	    print_tyc'' tycon
	  | CONty (tycon, [t]) => 
	    ( print_ty'' t; 
	      print " "; 
	      print_tyc'' tycon
	    )
	  | CONty (tycon, [t1, t2]) => 
	    ( print_ty'' t1; 
	      print " "; 
	      print_tyc'' tycon; 
	      print " "; 
	      print_ty'' t2
	    )
	  | CONty _ => 
	    ErrorMsg.impossible "Ens_var2: print_ty''.2"
	  | IBOUND i => 
	    print ("'" ^ str (Char.chr (Char.ord #"a" + i)))

    and print_tyc'' (tyc : tycon') : unit =
	case tyc of
	    GENtyc {name, ...} => print (EP.stoS name)
	  | PRIMtyc {name, ...} => print (EP.stoS name)
	  | DEFtyc {name, ...} => print (EP.stoS name)
	  | RECORDtyc _ => ErrorMsg.impossible "Ens_var2: print_tyc''"
	    

    (* variables *)
    structure VarKey : ORD_KEY =
    struct
        type ord_key = var_elem
	fun compare ({access = acc1, def = loc1, ...} : ord_key, 
		     {access = acc2, def = loc2, ...} : ord_key) = 
	    case String.compare (locFile loc1, locFile loc2) of
		EQUAL => compare_acc (acc1, acc2)
	      | ord => ord 
    end (* structure VarKey *)

    (* variable sets *)
    structure VarSet = RedBlackSetFn(VarKey)
    val ens_var = (ref VarSet.empty);

    fun add_var_def var 
		    region 
		    {str = M.STR {access = parent_acc, ...}, def, name} = 
	( case var of
	      (VC.VALvar {path = SymPath.SPATH [S.SYMBOL (_, "it")],...}) => ()
	    | VC.VALvar {access, typ, path = SymPath.SPATH path, ...} => (
	      EP.printer (!typ); print " : "; (*print_ty (!typ); print "\n";
	      print "\t"; print_ty'  (conv_ty (!typ)); print "\n";
	      print "\t";*) print_ty'' (conv_ty (!typ)); print "\n";
	      ens_var := VarSet.add(!ens_var, 
				    {access = access, 
				     parent = parent_acc, (* temporary *)
				     typ = !typ, 
				     name = List.last path,
				     def= loc_reg region, 
				     usage=ref []})
	      )
	    | _ => ()
	)
      | add_var_def _ _ _ = ()

    fun add_var_use (VC.VALvar {access, path = SymPath.SPATH path, typ, ...})
		    region
		    (typ' : T.tyvar list) = 
	( case VarSet.find
		   (fn {access = access', ...} => access = access') 
		   (!ens_var) 
	   of NONE => ()
	    | SOME {usage, ...} => 
	      let
		  val typ'' = 
		      case !typ of 
			  T.POLYty _ =>
			  TypesUtil.applyPoly 
			      (!typ, List.map TypesUtil.pruneTyvar typ')
			| _ => !typ
	      in
		  usage := (loc_reg region, typ'', access) :: (!usage)
	      end
	)
      | add_var_use _ _ _ = ()

    fun print_var () = 
	VarSet.app EP.print_var (!ens_var)
			  



    (* structures *)
    structure StrKey : ORD_KEY =
    struct
        type ord_key = str_elem
	fun compare ({access = access1, def = loc1, ...} : ord_key,
		     {access = access2, def = loc2, ...} : ord_key) =
	    ( case String.compare(locFile loc1, locFile loc2)
	       of EQUAL => compare_acc (access1, access2)
		| ord => ord
	    )
    end (* structure StrKey *)
    
    (* structure sets *)
    structure StrSet = RedBlackSetFn(StrKey)
    val ens_str = (ref StrSet.empty);

    fun add_str_def {name, 
		     str = M.STR {sign = M.SIG {stamp, elements, ...}, 
				  access, ...}, 
		     def} 
		    bl 
		    region 
		    parent_acc = 
	let 
	    fun get_symbol (B.VALbind (VC.VALvar {path, ...})) =
		SOME (SymPath.last path)
	      | get_symbol (B.STRbind (M.STR {rlzn = {rpath, ...}, ...})) = 
		SOME (InvPath.last rpath)
	      | get_symbol _ = NONE

	    fun get_symbol' (x, _) = x

	    fun get_acc b = 
		let val s = get_symbol b
		in
		    case s of
			NONE => NONE
		      | SOME symbol => 
			List.find (fn x => symbol = get_symbol' x) elements
		end

	    fun get_trip b = 
		case (b, get_acc b)
		 of (B.VALbind (VC.VALvar {access, ...}), 
		     SOME (s, M.VALspec {slot, ...})) => 
		    SOME (slot, s, Var access)
		  | (B.STRbind (M.STR {access, ...}), 
		     SOME (s, M.STRspec {slot, ...})) =>
		    SOME (slot, s, Str access)
		  | _ => NONE

	    val elements' = 
		List.mapPartial get_trip bl

	    fun get_slot (A.PATH (_, s)) = s

	    val elements'' = 
		case elements' of
		    (_, _, Var (A.PATH (acc, _)))::_ =>
		    Constraint 
			( List.map 
			      (fn ((x, y, Var z) | (x, y, Str z)) => 
				  (x, y, get_slot z)) 
			      elements',
			  acc
			)
		  | _ => 
		    Def elements'

	in
	    (*en profiter pour mettre a jour le champ parent des enfants?*)
	    ens_str := StrSet.add(!ens_str, 
				  { name = name, 
				    access = access,
				    parent = parent_acc,
				    sign = SOME stamp,
				    def = loc_reg region,
				    elements = elements'',
				    usage = ref []}
				 )
	end
      | add_str_def _ _ _ _ = ()

    fun add_str_alias {name, 
		       str = M.STR {sign = M.SIG {stamp, ...}, access, ...}, 
		       def}
		      (M.STR {access = access', ...})
		      region 
		      parent_acc = 
	ens_str := StrSet.add(!ens_str, 
			      { name = name, 
				access = access,
				parent = parent_acc,
				sign = SOME stamp,
				def = loc_reg region,
				elements = Alias access',
				usage = ref []}
			     )
      | add_str_alias _ _ _ _ = ()

    fun add_str_use (M.STR {access = access', ...}) region = 
	( case StrSet.find (fn {access, ...} => access=access') (!ens_str) of
	      NONE => ()
	    | SOME {usage, ...} => usage := loc_reg region :: !usage
	)
      | add_str_use _ _ = ()
	  
	
    fun print_str () = 
	StrSet.app EP.print_str (!ens_str)


    fun print_all () = (
	print_var ();
	print_str ()
    )

end (*end local*)
end (*end struct*)
