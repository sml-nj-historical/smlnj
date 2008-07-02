structure Ens_var2 = struct
local 
    structure A = Access
    structure VC = VarCon
    structure T = Types
    structure S = Symbol
    structure M = Modules
    structure B = Bindings
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
	    | VC.VALvar {access, typ, path = SymPath.SPATH path, ...} => 
	      ens_var := VarSet.add(!ens_var, 
				    {access = access, 
				     parent = parent_acc, (* temporary *)
				     typ = !typ, 
				     name = List.last path,
				     def= loc_reg region, 
				     usage=ref []})
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
	VarSet.app Ens_print2.print_var (!ens_var)
			  



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
	    fun get_symbol (B.VALbind 
				(VC.VALvar {path = SymPath.SPATH path, ...})) =
		SOME (List.last path)
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
		case (b, get_acc b) of
		    ((B.VALbind (VC.VALvar {access, ...})), 
		     SOME (s, M.VALspec {slot, ...})) => 
		    SOME (slot, s, Var access)
		  | _ => NONE

	    val elements' = 
		List.mapPartial get_trip bl

	    fun get_slot (A.PATH (_, s)) = s

	    val elements'' = 
		case elements' of
		    (_, _, Var (A.PATH (a, _)))::_ =>
		    Constraint (
		    List.map (fn (x, y, Var z)=> (x, y, get_slot z)) elements',
		    a)
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
	StrSet.app Ens_print2.print_str (!ens_str)


    fun print_all () = (
	print_var ();
	print_str ()
    )

end (*end local*)
end (*end struct*)
