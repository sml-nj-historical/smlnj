structure Database : DATABASE = struct
local 
    structure A = Access
    structure VC = VarCon
    structure T = Types
    structure S = Symbol
    structure M = Modules
    structure B = Bindings
    structure P = DBPrint
    open DBTypes
    open Conversion
in
    val source = ref ""
    fun set_source s = source := s

    val pid = ref NONE : PersStamps.persstamp option ref
    fun set_pid pid' = pid := SOME pid'

    val lvars = ref [] : A.access list ref
    val exts = ref [] : A.access list ref

    fun clear_lvar () = (lvars := []; exts := []; pid := NONE)
    fun add_lvar access = lvars := access :: !lvars 
    fun add_ext_acc access = exts := access :: !exts

    val extRefInfo = ref (fn _ => NONE) : (Symbol.symbol -> string option) ref
    fun set_eri eri = 
	(extRefInfo := eri)


    structure PidFileKey : ORD_KEY = 
    struct
        type ord_key = (PersStamps.persstamp * string)
	fun compare ((pid1, _), (pid2, _))= PersStamps.compare (pid1, pid2)
    end
    structure PidFileSet = RedBlackSetFn (PidFileKey)
    val pid_file = ref PidFileSet.empty
    fun print_pids () = 
	( PidFileSet.app 
	      (fn (x,y) => print (PersStamps.toHex x ^ "->" ^ y ^ ", "))
	      (!pid_file);
	  print "\n"
	)
    (******)

    fun is_available_rsl rev_symbol_list = 
	!extRefInfo (List.last rev_symbol_list) <> NONE

    fun is_available (InvPath.IPATH rpath) = 
	is_available_rsl rpath
	
    fun bug msg = ErrorMsg.impossible("Bugs in Database: "^msg);

    fun loc_reg (r1, r2) = ((!source, r1, r2):location)


    fun is_accessible (A.EXTERN _) = SOME false
      | is_accessible (A.NO_ACCESS) = NONE
      | is_accessible (A.PATH (s, _)) = is_accessible s
      | is_accessible (A.LVAR _) = SOME true

    (******)

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

    (*mapping from PATH (... EXTERN) to LVAR*) 
    structure LvarExtKey : ORD_KEY = 
    struct
        type ord_key = A.access * A.access
        fun compare ((acc1, _), (acc2, _))= compare_acc (acc1,acc2)
    end
    structure LvarExtSet = RedBlackSetFn (LvarExtKey)
    val lvar_ext = ref LvarExtSet.empty

    fun pickling_over () = 
	lvar_ext := LvarExtSet.addList ( !lvar_ext,
					 ListPair.zipEq (!exts, !lvars)
				       )
    fun print_lvars () = 
	( LvarExtSet.app 
	      (fn (x, y) => print (A.prAcc x ^ "->" ^ A.prAcc y ^ ", ")) 
	      (!lvar_ext);
	  print "\n"
	)

    (* end of LvarExt part*)

    fun compare_loc_acc (loc1, loc2) accs = 
	case String.compare (locFile loc1, locFile loc2) of
	    EQUAL => compare_acc accs
	  | ord => ord 

    (* external values *)
    structure ExtKey : ORD_KEY =
    struct
        type ord_key = ext_elem
	fun compare (ExtVar {access = acc1, ...}, ExtVar {access = acc2, ...})=
	    compare_acc (acc1, acc2)
	  | compare (ExtVar _, _) = LESS
	  | compare (_, ExtVar _) = GREATER
	  | compare (ExtStr {access = acc1, ...}, ExtStr {access = acc2, ...})=
	    compare_acc (acc1, acc2)
	  | compare (ExtStr _, _) = LESS
	  | compare (_, ExtStr _) = GREATER
	  | compare (ExtType {stamp = st1, ...}, ExtType {stamp = st2, ...}) =
	    Stamps.compare (st1, st2)
	  | compare (ExtType _, _) = LESS
	  | compare (_, ExtType _) = GREATER
	  | compare (ExtCons {stamp = st1,name = n1, ...}, 
		     ExtCons {stamp = st2,name = n2, ...})=
	    ( case String.compare (Symbol.name n1, Symbol.name n2) of
		  EQUAL => Stamps.compare (st1, st2)
		| ord => ord
	    )
	  | compare (ExtCons _, _) = LESS
	  | compare (_, ExtCons _) = GREATER
	  | compare (ExtSig {stamp = st1, ...}, ExtSig {stamp = st2, ...}) = 
	    Stamps.compare (st1, st2)
    end (* structure ExtKey *)

    (* external value set *)
    structure ExtSet = RedBlackSetFn(ExtKey)
    val ens_ext = (ref ExtSet.empty)
    val ens_ext_removed = (ref ExtSet.empty)

    (* variables *)
    structure VarKey : ORD_KEY =
    struct
        type ord_key = var_elem
	fun compare ({access = acc1, def = loc1, ...} : ord_key, 
		     {access = acc2, def = loc2, ...} : ord_key) = 
	    compare_loc_acc (loc1, loc2) (acc1, acc2)
    end (* structure VarKey *)

    (* variable sets *)
    structure VarSet = RedBlackSetFn(VarKey)
    val ens_var = (ref VarSet.empty);
    val ens_var_g = (ref VarSet.empty);

    fun find_var p = VarSet.find p (!ens_var_g)
    fun exists_var p = VarSet.exists p (!ens_var_g)
    fun filter_var p = VarSet.listItems (VarSet.filter p (!ens_var_g))

    (* structures *)
    structure StrKey : ORD_KEY =
    struct
        type ord_key = str_elem
	fun compare ({access = acc1, def = loc1, ...} : ord_key,
		     {access = acc2, def = loc2, ...} : ord_key) =
	    compare_loc_acc (loc1, loc2) (acc1, acc2)
    end (* structure StrKey *)
    
    (* structure sets *)
    structure StrSet = RedBlackSetFn(StrKey)
    val ens_str = (ref StrSet.empty);
    val ens_str_g = (ref StrSet.empty);

    fun find_str p = StrSet.find p (!ens_str_g)
    fun exists_str p = StrSet.exists p (!ens_str_g)
    fun filter_str p = StrSet.listItems (StrSet.filter p (!ens_str_g))

    (* type *)
    structure TypeKey : ORD_KEY =
    struct
        type ord_key = type_elem
	fun compare ({stamp = stamp1, ...}:ord_key, 
		     {stamp = stamp2, ...}:ord_key) =
	    Stamps.compare (stamp1, stamp2)
    end (* structure TypeKey *)
    
    (* type sets *)
    structure TySet = RedBlackSetFn(TypeKey)
    val ens_ty = (ref TySet.empty);
    val ens_ty_g = (ref TySet.empty);

    fun find_typ p = TySet.find p (!ens_ty_g)
    fun exists_typ p = TySet.exists p (!ens_ty_g)
    fun filter_typ p = TySet.listItems (TySet.filter p (!ens_ty_g))

    (* constructors *)
    structure ConsKey : ORD_KEY =
    struct
        type ord_key = cons_elem
	fun compare ({name = name1, dataty = s1, ...}:ord_key, 
		     {name = name2, dataty = s2, ...}:ord_key) =
	    case Stamps.compare (s1, s2) of
		EQUAL => String.compare (Symbol.name name1, Symbol.name name2)
	      | ord => ord
    end (* structure ConsKey *)
    
    (* constructor sets *)
    structure ConsSet = RedBlackSetFn(ConsKey)
    val ens_cons = (ref ConsSet.empty);
    val ens_cons_g = (ref ConsSet.empty);

    fun find_cons p = ConsSet.find p (!ens_cons_g)
    fun exists_cons p = ConsSet.exists p (!ens_cons_g)
    fun filter_cons p = ConsSet.listItems (ConsSet.filter p (!ens_cons_g))

    (*signature*)
    structure SigKey : ORD_KEY =
    struct
        type ord_key = sig_elem
	fun compare ({stamp = stamp1, ...}:ord_key, 
		     {stamp = stamp2, ...}:ord_key)=
	    Stamps.compare (stamp1, stamp2)
    end (* structure SigKey *)
    
    (* signature sets *)
    structure SigSet = RedBlackSetFn(SigKey)
    val ens_sig = ref SigSet.empty;
    val ens_sig_g = ref SigSet.empty;

    fun find_sig p = SigSet.find p (!ens_sig_g)
    fun exists_sig p = SigSet.exists p (!ens_sig_g)
    fun filter_sig p = SigSet.listItems (SigSet.filter p (!ens_sig_g))


    fun add_ext v = 
	ens_ext := ExtSet.add (!ens_ext, v)

    fun add_str v = 
	ens_str := StrSet.add (!ens_str, v)

    fun find_file_acc file acc ({access, def, ...}:str_elem) = 
	acc = access andalso locFile def = file

    fun find_acc acc = find_file_acc (!source) acc


    (* acc can be anything and we gave back an lvar *) 
    fun get_lvar set file test test2 (acc as A.PATH (a, slot)) = 
	let val acc' = get_lvar set file test2 test2 a in
	    case StrSet.find (find_file_acc (!file) acc') (!set) of
		NONE => bug ("get_lvar: " ^ A.prAcc acc)
	      | SOME {elements = Def l, ...} => 
		( case List.find (fn (x, _, _) => x = slot) l of
		      NONE => bug ("get_lvar2: " ^ A.prAcc acc)
		    | SOME (_, _, key) => 
		      ( case test key of
			    NONE => bug ("get_lvar3: " ^ A.prAcc acc)
			  | SOME a => get_lvar set file test test2 a
		      )
		)
	      | SOME {elements = Constraint (l, acc2), ...} => 
		( case List.find (fn (x, _, _) => x = slot) l of
		      NONE => bug ("get_lvar4: " ^ A.prAcc acc)
		    | SOME (_, _, slot2) => 
		      get_lvar set file test test2 (A.PATH (acc2,slot2))
		)
	      | SOME {elements = Alias a, ...} => 
		get_lvar set file test test2 (A.PATH (a,slot))
	end
      | get_lvar _ _ _ _ (acc as A.LVAR _) = acc
      | get_lvar _ _ _ _ _ = bug "get_lvar5"
		
    fun key_str (Str a) = SOME a
      | key_str _ = NONE

    fun key_val (Var a) = SOME a
      | key_val _ = NONE

    val get_str_lvar        = get_lvar ens_str   source     key_str key_str
    fun get_str_lvar_g file = get_lvar ens_str_g (ref file) key_str key_str
    val get_var_lvar        = get_lvar ens_str   source     key_val key_str
    fun get_var_lvar_g file = get_lvar ens_str_g (ref file) key_val key_str

    fun modify [] region = 
	([], region)
      | modify (h :: q) (r1, _) = 
	(q, (r1-1-String.size (Symbol.name h), r1-1))

    (* when saying  A.x, or open A ... x, the use of A is added here *)
    (* gives back the lvar corresponding to the structure or variable *)
    fun add_implicite_use (A.PATH (access, slot0)) rpath region is_str = 
	let fun add_uses (a as A.LVAR _) _ _ = 
		let val (rpath, region) = modify rpath region in
		    case StrSet.find (find_acc a) (!ens_str) of
			NONE => print "pb in add_implicite_use1\n"
		      | SOME {usage, ...} => 
			usage := loc_reg region :: !usage;
		    a
		end
	      | add_uses (a as (A.PATH (a', slot))) rpath region = 
		let val (rpath, region) = modify rpath region
		    val a' = add_uses a' rpath region
		    val a = get_str_lvar (A.PATH (a',slot))
		in
		    case StrSet.find (find_acc a) (!ens_str) of
			NONE => print "pb in add_implicite_use1\n"
		      | SOME {usage, ...} => 
			usage := loc_reg region :: !usage;
		    a
		end
	      | add_uses a _ _ = bug ("add_implicite_use3" ^ A.prAcc a)
	in
	    if is_str then
		get_str_lvar (A.PATH ((add_uses access rpath region),slot0))
	    else
		get_var_lvar (A.PATH ((add_uses access rpath region),slot0))
	end
      | add_implicite_use (access as A.LVAR _) _ _ _ = access
      | add_implicite_use _ _ _ _ = bug "add_implicite_use4"

    fun str_find access = 
	case 
	    ExtSet.find 
		(fn x => 
		    ExtKey.compare (ExtStr {access = access, usage = ref []},x)
		    = EQUAL)
		(!ens_ext)
	 of NONE => NONE
	  | SOME (ExtStr a) => SOME a
	  | SOME _ => bug "str_find"

    fun var_find access = 
	case 
	    ExtSet.find 
		(fn x => 
		    ExtKey.compare (ExtVar {access = access, usage = ref []},x)
		    = EQUAL)
		(!ens_ext)
	 of NONE => NONE
	  | SOME (ExtVar a) => SOME a
	  | SOME _ => bug "str_find"

    fun add_implicite_uses_ext (A.EXTERN _) _ _ = ()
      | add_implicite_uses_ext (a as A.PATH (a', _)) rpath region =
	let val (rpath, region) = modify rpath region
	in
	    case str_find a of
		NONE => 
		add_ext (ExtStr {access = a,
				 usage = ref [loc_reg region]})
	      | SOME {usage, ...} => 
		usage := loc_reg region :: !usage;
	    add_implicite_uses_ext a' rpath region
	end
      | add_implicite_uses_ext _ _ _ = bug "add_implicite_uses_ext"

    fun print_ext () = 
	ExtSet.app P.print_ext (!ens_ext)


    fun add_var_def var 
		    (r1, _)
		    {str = M.STR {access = parent_acc, ...}, def, name} = 
	( case var of
	      (VC.VALvar {path = SymPath.SPATH [S.SYMBOL (_, "it")],...}) => ()
	    | VC.VALvar {access, typ, path = SymPath.SPATH path, ...} =>
		  ens_var := 
		  VarSet.add(!ens_var, 
			     {access = access, 
			      parent = parent_acc,
			      typ = ty_to_ty' (!typ),
			      name = List.last path,
			      def= loc_reg (r1,r1+String.size 
						      (Symbol.name 
							   (List.last path))), 
			      usage=ref []})
	    | _ => ()
	)
      | add_var_def _ _ _ = ()

    fun get_ty' typ typ' = 
	let
	    val typ'' = 
		case !typ of 
		    T.POLYty _ =>
		    TypesUtil.applyPoly 
			(!typ,List.map TypesUtil.pruneTyvar typ')
		  | _ => !typ
	in
	    ty_to_ty' typ''
	end

    fun modify' rpath (_, r2) = 
	let (* +1 pour compenser le -1 qui aura lieu dans 
	     * modify *)
	    val region = (r2+1, r2+1)
	 in modify rpath region
	end

    fun add_var_use (VC.VALvar {access, path = SymPath.SPATH path, typ, ...})
		    region
		    (typ' : T.tyvar list) = 
	( case is_accessible access of
	      NONE => bug "add_var_use0"
	    | SOME false =>
	      ( case access of
		    (A.PATH (suite, _)) => 
		    let val rpath = rev path
		    in
			if is_available_rsl rpath then
			    let val (rpath, region) = modify' rpath region
				val triplet = 
				    (loc_reg region, get_ty' typ typ', access)
			    in
				case var_find access of
				    NONE => 
				    add_ext (ExtVar {access = access,
						     usage = ref [triplet]})
				  | SOME {usage, ...} => 
				    usage := triplet :: !usage;
				add_implicite_uses_ext suite rpath region
			    end
			else
			    ()
		    end
		  | _ => bug "add_var_use2"
	      )
	    | SOME true =>
	      let val (rpath,region) = modify' (rev path) region
		  val new_acc=add_implicite_use access rpath region false
	      in
		  case VarSet.find
			   (fn {access, ...} => access = new_acc) 
			   (!ens_var) 
		   of NONE => ()
		    | SOME {usage, ...} => 
		      usage := (loc_reg region, get_ty' typ typ', access) ::
			       (!usage)
	      end
	)
      | add_var_use _ _ _ = ()

    fun print_var () = 
	VarSet.app P.print_var (!ens_var)
			  
    fun print_var_g () = 
	VarSet.app P.print_var (!ens_var_g)


    fun add_ty_def tycon region = 
	let val (stamp, path, tycon'(*, dcons*)) = tycon_to_tycon' tycon in
	    ens_ty := TySet.add ( !ens_ty,
				  { tycon = tycon',
				    name = InvPath.last path,
				    stamp = stamp,
				    def = loc_reg region,
				    usage = ref []}
				)(*;
	    case (tycon', dcons) of
		((Datatype (_, sl) | Abstract sl), SOME dcons) =>
		List.foldl
		    ( fn (x, n) => (
			 ConsSet.add 
			     ( !ens_cons,
			       { name = x, 
				 dataty = stamp,
				 def = loc_reg region,
				 ty=ty_to_ty' 
					(Option.valOf 
					     (#domain
						  (#dcons 
						       (Vector.sub(members,n))
						  )
					     )
					),
				 usage = ref []
			       }
			     );
			 (n+1)
			 )
		    )
		    0
		    sl
	      | ((Deftyc | Primtyc _), _) => 0;
	    ()*)
	end

    fun add_ty_use ty region = 
	let val region = loc_reg region in
	    case ty_to_ty' ty of
		Conty (General (st, p), _) =>
		( case TySet.find 
			   (fn {stamp, ...} => Stamps.eq (stamp,st)) 
			   (!ens_ty) 
		   of NONE =>  (*probablement un truc primitif, mais peut etre 
				* un type extern*)
		      if is_available p then (* c'est a dire qu'on a le fichier
					      * ou le type est defini*)
			  (*considerer les utilisations implicites*)
			  case ExtSet.find 
				   (fn (ExtType {stamp, ...}) => 
				       Stamps.eq (stamp,st)
				     | _ => false
				   )
				   (!ens_ext)
			   of NONE => add_ext (ExtType {stamp = st, 
							usage = ref [region]}
					      )
			    | SOME (ExtType {usage, ...}) => 
			      usage := region :: !usage
			    | SOME _ => bug "add_ty_use"
		      else
			  ()
		    | SOME {usage, ...} => usage := region :: (!usage)
		)
	      | _ => ()
	end

    fun print_ty () = 
	TySet.app P.print_type (!ens_ty)

    fun print_ty_g () = 
	TySet.app P.print_type (!ens_ty_g)

    fun print_cons () = 
	ConsSet.app P.print_cons (!ens_cons)

    fun print_cons_g () = 
	ConsSet.app P.print_cons (!ens_cons_g)


    fun add_sig_def sign region = 
	let val {name, stamp, inferred, def, elements, alias, usage} = 
		sig_to_elem sign in
	    ens_sig := SigSet.add ( !ens_sig,
				    { name = name, 
				      stamp = stamp,
				      inferred = inferred,
				      def = loc_reg region,
				      elements = elements,
				      alias = alias,
				      usage = usage
				    }
				  )
	end

    fun add_sig_use (M.SIG {name, stamp, ...}) region = 
	( case SigSet.find 
		   (fn {stamp = st, ...} => Stamps.eq (stamp, st)) 
		   (!ens_sig)
	   of NONE => bug "add_sig_use"
	    | SOME {usage, name, ...} => 
	      usage := (loc_reg region, name) :: !usage
	)
      | add_sig_use _ _ = 
	bug "add_sig_use2"
	
    fun print_sig () = 
	SigSet.app P.print_sig (!ens_sig)
	
    fun print_sig_g () = 
	SigSet.app P.print_sig (!ens_sig_g)




    fun print_str () = 
	StrSet.app P.print_str (!ens_str)

    fun print_str_g () = 
	StrSet.app P.print_str (!ens_str_g)

    fun get_path ({rpath = InvPath.IPATH rpath, ...}:M.strEntity) = 
	rpath

    fun add_str_use (M.STR {access, rlzn, ...}) region = 
	( case is_accessible access of
	      SOME false => 
	      (*extern*)
	      let val rpath = get_path rlzn
	      in
		  case access of
		      A.PATH (suite, _) =>
		      if is_available_rsl rpath then
			  (* a verifier *)
			  let val (rpath, region) = modify' rpath region in
			      case str_find access of
				  NONE => 
				  add_ext (ExtStr {access = access,
						   usage = ref [loc_reg region]
						  }
					  )
				| SOME {usage, ...} => 
				  usage := loc_reg region :: !usage;
			      add_implicite_uses_ext suite rpath region
			  end
		      else
			  ()
		    | _ => bug "add_str_use0"
	      end
	    | SOME true => 
	      (*local*)
	      let val (rpath, region) = modify' (get_path rlzn) region
		  val new_acc = add_implicite_use access rpath
						  region true
	      in
		  case StrSet.find 
			   (fn {access, ...} => access = new_acc) 
			   (!ens_str)
		   of NONE => 
		      bug ("add_str_use1 " ^ A.prAcc access ^ " " ^ 
			   A.prAcc new_acc)
		    | SOME {usage, ...} => usage := loc_reg region :: !usage
	      end
	    | NONE => 
	      (*NO_ACCESS*) 
	      bug "add_str_use2"
	)
      | add_str_use _ _ = 
	bug "add_str_use"

    fun binding_to_symbolkey b = 
	case b of
	    B.VALbind (VC.VALvar {access, path, ...}) => 
	    (SymPath.last path, Var access)
	  | B.STRbind (M.STR {access, rlzn = {rpath, ...}, ...}) =>
	    (InvPath.last rpath, Str access)
	  | _ => bug "binding_to_symbolkey : non implemente"
          (*| B.CONbind VarCon.datacon => 
	  | B.TYCbind Types.tycon =>
	  | B.SIGbind Modules.Signature =>
	  | B.FSGbind Modules.fctSig =>
	  | B.FCTbind Modules.Functor => 
	  | B.FIXbind Fixity.fixity =>*)
		 
    (*fun symbolkey_to_slotsymbolkey sig_elements (s,k as Var _) = 
	( case List.find (fn (symbol, _) => symbol = s) sig_elements of
	      NONE => bug "symbolkey_to_slotsymbolkey1"
	    | SOME (_, M.VALspec {slot, ...}) =>
	      (slot, s, k)
	    | SOME _ => bug "symbolkey_to_slotsymbolkey1.5"
	)
      | symbolkey_to_slotsymbolkey sig_elements (s,k as Str a) = 
	( case List.find (fn (symbol, _) => symbol = s) sig_elements of
	      NONE => 
	      ((* List.app 
		    (fn (x, _) => print (Symbol.name x ^ ","))
		    sig_elements;
		print "\n";
		bug ("symbolkey_to_slotsymbolkey2 " ^ Symbol.name s ^ 
		     " " ^ A.prAcc a)*)
		    (~1,s,Str a)
	      )
	    | SOME (_, M.STRspec {slot, ...}) =>
	      (slot, s, k)
	    | SOME _ => bug "symbolkey_to_slotsymbolkey2.5"
	)
      | symbolkey_to_slotsymbolkey _ _ = 
	bug "symbolkey_to_slotsymbolkey: non implemente"*)

    (* should work if binding lists are ordered on the slot *)
    fun symbolkeylist_to_slotsymbolkeylist l = 
	let fun trans ((s,k) :: q) n = 
		(n, s, k) :: trans q (n+1)
	      | trans [] _ = []
	in trans l 0
	end
	
    fun slotsymbolkey_to_slotsymbolkey' NONE triplet =	triplet
      | slotsymbolkey_to_slotsymbolkey' (SOME bindings) (slot, s, Var _) =
	( case List.find 
		   ( fn (B.VALbind (VC.VALvar {path, ...})) =>
			SymPath.last path = s
		      | _ => false
		   )
		   bindings
	   of NONE => bug "slotsymbolkey_to_slotsymbolkey'1"
	    | SOME (B.VALbind (VC.VALvar {access, ...})) =>
	      (slot, s, Var access)
	    | SOME _ => bug "slotsymbolkey_to_slotsymbolkey'2"
	)
      | slotsymbolkey_to_slotsymbolkey' (SOME bindings) (slot, s, Str _) =
	( case List.find 
		   (fn (B.STRbind (M.STR {rlzn = {rpath, ...}, ...})) =>
		       InvPath.last rpath = s
		     | _ => false
		   )
		   bindings
	   of NONE => bug "slotsymbolkey_to_slotsymbolkey'3"
	    | SOME (B.STRbind (M.STR {access, ...})) =>
	      (slot, s, Str access)
	    | SOME _ => bug "slotsymbolkey_to_slotsymbolkey'4"
	)
      | slotsymbolkey_to_slotsymbolkey' _ _ = 
	bug "slotsymbolkey_to_slotsymbolkey': non implemente"

    fun add_str_def { name = name_str, 
		      str = 
		      M.STR { sign = (sign as M.SIG {name,stamp,elements,...}),
			      access, ...}, 
		      def = _} 
		    bl1_option
		    bl2
		    region 
		    parent_acc = 
	let 
	    val () = 
		case name of
		    NONE => add_sig_def sign region
		  | SOME s => 
		    if String.sub (Symbol.name s, 0) = #"<" then
			(* anonymous or inferred signature *)
			add_sig_def sign region
		    else
			add_sig_use sign region
	    (*val def_elements = 
		List.map (slotsymbolkey_to_slotsymbolkey' bl1_option o 
			  symbolkey_to_slotsymbolkey elements o
			  binding_to_symbolkey)	
			 bl2*)
	    val def_elements = 
		List.map (slotsymbolkey_to_slotsymbolkey' bl1_option)
		( symbolkeylist_to_slotsymbolkeylist 
		      (List.map binding_to_symbolkey bl2)
		)
 	in
	    (*en profiter pour mettre a jour le champ parent des enfants?*)
	    ens_str := StrSet.add(!ens_str, 
				  { name = name_str,
				    access = access,
				    parent = parent_acc,
				    sign = SOME stamp,
				    def = loc_reg region,
				    elements = Def def_elements,
				    usage = ref []}
				 )
	end
      | add_str_def _ _ _ _ _ = 
	bug "add_str_def"

    fun add_str_alias {name,
		       str = M.STR {sign = M.SIG {stamp, ...}, access, ...},
		       def = _
		      }
		      (rhs as M.STR {access = access', ...})
		      region
		      regionRHS
		      parent_acc = 
	( add_str_use rhs regionRHS;
	  ens_str := StrSet.add(!ens_str, 
				{ name = name, 
				  access = access,
				  parent = parent_acc,
				  sign = SOME stamp,
				  def = loc_reg region,
				  elements = Alias access',
				  usage = ref []}
			       )
	)
      | add_str_alias _ _ _ _ _ = 
	bug "add_str_alias"

    fun add_str_sig_alias 
	    {name, 
	     str = M.STR {sign = M.SIG {stamp, elements, ...}, access, ...}, 
	     def = _}
	    rhs
	    bindinglist
	    regionID
	    regionRHS
	    parent_acc = 
	let val acc = ref A.NO_ACCESS
	    fun get_slot (A.PATH (acc', slot)) = (acc := acc'; slot)
	      | get_slot _ = bug "add_str_sig_alias.get_slot"
	    fun get_slot' (a, b, (Var c | Str c)) = (a, b, get_slot c)
	      | get_slot' _ = bug "add_str_sig_alias.get_slot'"
	    (*val def_elements = 
		List.map (get_slot' o 
			  symbolkey_to_slotsymbolkey elements o
			  binding_to_symbolkey)	
			 bindinglist*)
	    val def_elements = 
		List.map get_slot'
			 ( symbolkeylist_to_slotsymbolkeylist 
			       (List.map binding_to_symbolkey bindinglist)
			 )
	    val elements = 
		case (!acc, rhs) of
		    (A.NO_ACCESS, _) => 
		    (* the rhs structure is empty, how to get
		     * its access ? *)
		    Def []
		  | (rhs_access, M.STR {sign, access, rlzn, prim}) =>
		       let val rhs' = 
			       M.STR { sign = sign, 
				       access = !acc, 
				       rlzn = rlzn, 
				       prim = prim
				     } 
		       in add_str_use rhs' regionRHS;
			  Constraint (def_elements, rhs_access)
		       end 
		  | _ => bug "add_str_sig_alias1"
	in
	    ens_str := StrSet.add(!ens_str, 
				  { name = name, 
				    access = access,
				    parent = parent_acc,
				    sign = SOME stamp,
				    def = loc_reg regionID,
				    elements = elements,
				    usage = ref []}
				 )
	end
      | add_str_sig_alias _ _ _ _ _ _ = 
	bug "add_str_sig_alias"



    fun print_all () = (
	print "****** VAR : \n";print_var ();
	print "****** STR : \n";print_str ();
	print "****** TYP : \n";print_ty ();
	print "****** CON : \n";print_cons ();
	print "****** SIG : \n";print_sig ();
	print "****** EXT : \n";print_ext ()
    )

    fun print_all_g () = (
	print "****** VAR : \n";print_var_g ();
	print "****** STR : \n";print_str_g ();
	print "****** TYP : \n";print_ty_g ();
	print "****** CON : \n";print_cons_g ();
	print "****** SIG : \n";print_sig_g ();
	print "****** EXT : \n";print_ext ();
	print "****** LVA : \n";print_lvars ();
        print "****** PID : \n";print_pids ()
    )


    fun clear () = (
	ens_var := VarSet.empty;
	ens_ty := TySet.empty;
	ens_cons := ConsSet.empty;
	ens_str := StrSet.empty;
	ens_sig := SigSet.empty;
	ens_ext := ExtSet.empty;
	source := "";
	extRefInfo := (fn _ => NONE)
    )

    fun clear_all () = 
	( clear ();
	  clear_lvar ()
	)

    fun get_strings (a, b, c, d, e, f, g, h) = 
	( SerializeDB.varToString     (VarSet.listItems  a),
	  SerializeDB.typeToString    (TySet.listItems   b),
	  SerializeDB.consToString    (ConsSet.listItems c),
	  SerializeDB.strToString     (StrSet.listItems  d),
	  SerializeDB.sigToString     (SigSet.listItems  e),
	  SerializeDB.extToString     (ExtSet.listItems  f),
	  SerializeDB.lvarExtToString (LvarExtSet.listItems g),
	  SerializeDB.pidOptionToString h
	)

    fun get_sets (a, b, c, d, e, f, g, h) = 
	( VarSet.fromList     (UnSerializeDB.stringToVar  a),
	  TySet.fromList      (UnSerializeDB.stringToType b),
	  ConsSet.fromList    (UnSerializeDB.stringToCons c),
	  StrSet.fromList     (UnSerializeDB.stringToStr  d),
	  SigSet.fromList     (UnSerializeDB.stringToSig  e),
	  ExtSet.fromList     (UnSerializeDB.stringToExt  f),
	  LvarExtSet.fromList (UnSerializeDB.stringToLvarExt g),
	  UnSerializeDB.stringToPidOption h
	)
	
    fun get_pickle () = 
	let val (a,b,c,d,e,f,g,h) = 
		get_strings (!ens_var, !ens_ty, !ens_cons, !ens_str,
			     !ens_sig, !ens_ext, !lvar_ext, !pid)
	in String.concat 
	       [a,"\n",b,"\n",c,"\n",d,"\n",e,"\n",f,"\n",g,"\n",h,"\n"]
	end

    fun load_return source = 
	let val os = TextIO.openIn source
	    fun get_val NONE = bug ("sourcefile " ^ source ^ ":unexpected EOF")
	      | get_val (SOME s) = s
	    fun gs () = get_val (TextIO.inputLine os)
	in
	    get_sets (gs(),gs(),gs(),gs(),gs(),gs(),gs(),gs())
	end

    fun get_file e = 
	case PidFileSet.find (fn (pid, _) => pid = e) (!pid_file) of
	    NONE => bug "get_file"
	  | SOME (_, filename) => filename

    fun modify_path (a as A.PATH (A.EXTERN e, _)) lv = 
	( case LvarExtSet.find (fn (ext_acc, _) => ext_acc = a) lv of
	      NONE => bug ("modify_path2 " ^ A.prAcc a)
	    | SOME (_, loc_acc) => (loc_acc, get_file e)
	)
      | modify_path (A.PATH (a, slot)) lv = 
	let val (a', file) = modify_path a lv in
	    (A.PATH (a', slot), file)
	end
      | modify_path _ _ = bug "modify_path1"
		
    fun distribution (va,ty,co,st,si,lv) ext = 
	case ext of
	    ExtVar {access, usage} =>
	    let val (acc, sourcename) = 
		    case modify_path access lv of
			(pair as (A.PATH _, _)) => pair
		      | _ => bug "distribution.ExtVar1"
		val lvar = get_var_lvar_g sourcename acc
	    in case VarSet.find (fn {access, def, ...} => 
				    access = lvar andalso
				    locFile def = sourcename
				) (!va)
		of NONE => bug ("distribution.ExtVar: " ^ A.prAcc access ^ 
				" " ^ sourcename)
		 | SOME {usage = u, name, ...} => 
		   u := !usage @ !u
	    end
	  | ExtStr {access, usage} =>
	    let val (acc, sourcename) = modify_path access lv
		val lvar = get_str_lvar_g sourcename acc
	    in case StrSet.find (fn {access, def, ...} => 
				    access = lvar andalso
				    locFile def = sourcename
				) (!st) of
		   NONE => bug ("distribution.ExtStr " ^ A.prAcc access ^ " " ^
				A.prAcc lvar)
		 | SOME {usage = u, ...} => u := !usage @ !u
	    end
	  | ExtType {stamp, usage} => ()
	    (* case TySet.find () (!ty) of
		  NONE => bug "distribution.ExtType"
		| SOME {usage = u, ...} => u := !usage @ !u
	    *)
	  | ExtCons {stamp, name, usage} => ()
	    (* case ConsSet.find () (!co) of
		  NONE => bug "distribution.ExtCons"
		| SOME {usage = u, ...} => u := !usage @ !u
	    *)
	  | ExtSig {stamp, usage} => ()
	    (* case SigSet.find () (!si) of
		  NONE => bug "distribution.ExtSig"
		| SOME {usage = u, ...} => u := !usage @ !u
	    *)

    fun get_pid file = 
	case PidFileSet.find (fn (_,x) => file = x) (!pid_file) of
	    NONE => bug ("get_pid" ^ file)
	  | SOME (x, _) => x

    fun get_hash (A.EXTERN e) = e
      | get_hash (A.PATH (a, _)) = get_hash a
      | get_hash _ = bug "get_hash"

    fun merge (var,ty,cons,str,sign,ext,lvarext,pid') sourcefile = 
	let val ens_var2 = ref var
	    val ens_ty2 = ref ty
	    val ens_cons2 = ref cons
	    val ens_str2 = ref str
	    val ens_sig2 = ref sign
	    val distrib = distribution (ens_var_g, ens_ty_g, ens_cons_g, 
					ens_str_g, ens_sig_g, !lvar_ext)
	in 
	    ExtSet.app distrib ext;
	    ens_var_g  := VarSet.union  (!ens_var_g,  !ens_var2);
	    ens_ty_g   := TySet.union   (!ens_ty_g,   !ens_ty2);
	    ens_cons_g := ConsSet.union (!ens_cons_g, !ens_cons2);
	    ens_str_g  := StrSet.union  (!ens_str_g,  !ens_str2);
	    ens_sig_g  := SigSet.union  (!ens_sig_g,  !ens_sig2);
	    lvar_ext := LvarExtSet.union (lvarext,!lvar_ext);
            (*ens_ext := ext; should be empty anyway !! not when removing *)
	    pid_file := PidFileSet.add
			    (!pid_file, 
			     case pid' of
				 NONE => bug "merge"
			       | SOME pid'' => (pid'', sourcefile)
			    );
	    let val pid = get_pid sourcefile
		val (to_be_added, others) = 
		    ExtSet.partition 
			(fn ExtStr {access, ...} => get_hash access = pid
			  | ExtVar {access, ...} => get_hash access = pid
			  | _ => false)
			(!ens_ext_removed)
	    in
		ExtSet.app distrib to_be_added;
		ens_ext_removed := others
	    end
	end

    fun merge_pickle sourcefile pickle = 
	case String.tokens (fn x => x = #"\n") pickle of
	    [a,b,c,d,e,f,g,h] => merge (get_sets (a,b,c,d,e,f,g,h)) sourcefile
	  | l => bug ("merge_pickle " ^ Int.toString (List.length l))

    fun load_merge sourcefile = 
	let val sl = String.tokens (fn x => x = #"/") sourcefile
	    fun modi [a] = [".cm","INFO",a]
	      | modi [] = bug "load_merge"
	      | modi  (h :: q) = h :: modi q
	    val sourcefile2 = String.concatWith "/" ("" :: modi sl)
	in
	    merge (load_return sourcefile2) sourcefile
	end
	
    fun find_son access_par access_son file good_key = 
	case StrSet.find 
		 (fn {access = access1, def, ...} => 
		     access1 = access_par andalso 
		     locFile def = file)
		 (!ens_str_g)
	 of NONE => bug "find_son"
	  | SOME {elements = Alias a, ...} =>
	    find_son a access_son file good_key
	  | SOME {elements = Def l, ...} => 
	    ( case List.find 
		       (fn (_, _, k) => good_key (k,access_son))
		       l
	       of NONE => (List.app (fn (_,_,x)=>(P.print_key x;print " ")) l;
			   print "\n";
			   print (A.prAcc access_par ^ " "^A.prAcc access_son);
			   print "\n";
			   bug "find_son2")
		| SOME (i, _, _) => i
	    )
	  | SOME {elements = Constraint _, ...} =>
	    bug "find_son3"
	    
    fun externalize_str access file = 
	let fun find_son2 x y = 
		find_son x y file (fn (Str a,a2) => a = a2 | _ => false)
	    fun str access = 
		case StrSet.find 
			 (fn {access = access1, def, ...} => 
			     access1 = access andalso locFile def = file)
			 (!ens_str_g)
		 of NONE => bug "externalize1"
		  | SOME {parent = SOME access_loc_parent, ...} =>
		    let val access_lvar_parent= get_str_lvar access_loc_parent
			val access_ext_parent = str access_lvar_parent
			val is_alias = 
			    ( case StrSet.find 
				       (fn {access, def, ...} => 
					   access_lvar_parent = access andalso 
					   locFile def = file
				       )
				       (!ens_str_g)
			       of NONE => bug "externalize2"
				| SOME {elements = Alias a, ...} => true
				| _ => false
			    )
		    in
			if is_alias then
			    access_ext_parent
			else
			    let val slot = find_son2 access_lvar_parent access
			    in (A.PATH (access_ext_parent, slot))
			    end
		    end
		  | SOME {parent = NONE, ...} => 
		    let val pid = get_pid file in
		    case LvarExtSet.find 
			     (fn (A.PATH (A.EXTERN e, _), y) => 
				 e = pid andalso
				 y = access
			       | _ => bug "externalize 4"
			     )
			     (!lvar_ext)
		     of
			NONE => bug "externalize5"
		      | SOME (A.PATH (A.EXTERN _, slot), _) =>
			A.PATH(A.EXTERN pid, slot)
		      | SOME _ => bug "externalize6"
		    end
	in
	    str access
	end

    fun externalize_var (A.PATH (access, slot)) file = 
	A.PATH (externalize_str access file, slot)
      | externalize_var (access as A.LVAR _) file =
	let fun find_son2 x y = 
		find_son x y file (fn (Var a, a2) => a = a2 | _ => false)
	in 
	    case VarSet.find (fn {access = access1, def, ...} => 
				 access1 = access andalso 
				 locFile def = file)
			     (!ens_var_g)
	     of NONE => bug "externalize_var"
	      | SOME {parent, ...} => 
		A.PATH (externalize_str parent file, 
			find_son2 (get_str_lvar parent) access)
	end
      | externalize_var _ _ = bug "externalize_var"

    (*********** A FAIRE ***********)
    fun remove file = 
	( VarSet.app
	      ( fn {access, def, usage, ...} => 
		   (if locFile def = file then 
			let val usage = 
				List.filter 
				    (fn (x, _, _) => locFile x <> file)
				    (!usage)
			in
			    if not (List.null usage) then
				ens_ext_removed := 
				ExtSet.add 
				    ( !ens_ext_removed,
				      ExtVar {access = 
					      externalize_var access 
							      file, 
					      usage = ref usage}
				    )
			    else
				()
			end
		    else
			()
		   )
	      )
	      (!ens_var_g);
	  ens_var_g := 
	  VarSet.filter 
	      ( fn {usage, def, ...} => 
		   if locFile def = file then 
		       false
		   else (
		       usage := List.filter 
				    (fn (x, _, _)=> locFile x <> file)
				    (!usage);
		       true
		       )
	      )
	      (!ens_var_g);
	  StrSet.app
	      ( fn {access, def, usage, ...} => 
		   (if locFile def = file then 
			let val usage = 
				List.filter 
				    (fn x => locFile x <> file)
				    (!usage)
			in
			    if not (List.null usage) then
				ens_ext_removed := 
				ExtSet.add 
				    ( !ens_ext_removed,
				      ExtStr {access = 
					      externalize_str access 
							      file, 
					      usage = ref usage}
				    )
			    else
				()
			end
		    else
			()
		   )
	      )
	      (!ens_str_g);
	  ens_str_g := StrSet.filter 
			   ( fn {usage, def, ...} => 
				if locFile def = file then 
				    false
				else (
				    usage := List.filter 
						 (fn x => locFile x <> file)
						 (!usage);
				    true
				    )
			   )
			   (!ens_str_g)
	)
	  
    fun test () =
	let val (a,b,c,d,e,f,g,h) = 
		get_sets ( get_strings (!ens_var, !ens_ty, !ens_cons, !ens_str,
					!ens_sig, !ens_ext, !lvar_ext, !pid)
			 )
	    val () = 
		if VarSet.numItems a <> VarSet.numItems (!ens_var) then
		    bug "test ().var length"
		else
		    ()
	    val () = 
		if TySet.numItems b <> TySet.numItems (!ens_ty) then
		    bug "test ().type length"
		else
		    ()
	    val () = 
		if ConsSet.numItems c <> ConsSet.numItems (!ens_cons) then
		    bug "test ().cons length"
		else
		    ()
	    val () = 
		if StrSet.numItems d <> StrSet.numItems (!ens_str) then
		    bug "test ().str length"
		else
		    ()
	    val () = 
		if SigSet.numItems e <> SigSet.numItems (!ens_sig) then
		    bug "test ().sig length"
		else
		    ()
	    val () = 
		if ExtSet.numItems f <> ExtSet.numItems (!ens_ext) then
		    bug "test ().ext length"
		else
		    ()
	    val () = 
		if LvarExtSet.numItems g <>LvarExtSet.numItems (!lvar_ext) then
		    bug "test ().lvar_ext length"
		else
		    ()
	in
	    VarSet.app P.print_var a;
	    TySet.app P.print_type b;
	    ConsSet.app P.print_cons c;
	    StrSet.app P.print_str d;
	    SigSet.app P.print_sig e;
	    ExtSet.app P.print_ext f;
	    print_lvars ()
	end


end (*end local*)
end (*structure Database *)
