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
    fun bug msg = ErrorMsg.impossible("Bugs in Database: "^msg);

    val source = ref ""
    fun set_source s = source := s

    val pid = ref NONE : PersStamps.persstamp option ref
    fun set_pid pid' = pid := SOME pid'

    fun persstamps_eq c = PersStamps.compare c = EQUAL

    val lvars = ref [] : A.access list ref
    val exts = ref [] : A.access list ref

    fun clear_lvar () = (lvars := []; exts := []; pid := NONE)
    fun add_lvar access = lvars := access :: !lvars 
    fun add_ext_acc access = exts := access :: !exts

    val extRefInfo = ref (fn _ => NONE) : (Symbol.symbol -> string option) ref
    fun set_eri eri = 
	(extRefInfo := eri)


    (* faire un redblacktree autour pour les differents fichiers + serializer*)
    structure OccurrenceKey : ORD_KEY = 
    struct
        type ord_key = occurrence
	fun compare ((_, (_, _, end1)), (_, (_, _, end2))) = 
	    Int.compare (end1,end2)
    end
    structure OccSet = SIRedBlackSetFn (OccurrenceKey)
    val occ_set = ref OccSet.empty
    fun add_occ occ = 
	occ_set := OccSet.add (!occ_set, occ)

    fun print_occ () = 
	OccSet.app P.print_occ (!occ_set)

    structure OccSetKey : ORD_KEY = 
    struct
        type ord_key = OccSet.set * file
	fun compare ((_, f1), (_, f2)) = 
	    String.compare (f1,f2)
    end
    structure OccSetG = RedBlackSetFn (OccSetKey)
    val occ_set_g = ref OccSetG.empty

    fun print_occ_g () = 
	OccSetG.app 
	    (fn (x,y) => 
		(print ("->"^y^" : "); OccSet.app P.print_occ x; print "\n"))
	    (!occ_set_g)

    fun charposToOccurrence (char, file) = 
	case OccSetG.find (fn (_, f) => f = file) (!occ_set_g) of
	    NONE => bug ("charposToOccurrence : " ^ file ^ "is not in the DB")
	  | SOME (set, _) => 
	    ( case OccSet.nextOrEqual (set, (Symbol.varSymbol "", ("",0,char)))
	       of NONE => bug "charposToOccurrence : no next item"
		| SOME occ => occ
	    )

    (******)
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

    fun equal_acc acc1 acc2 =  compare_acc (acc1,acc2) = EQUAL

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

    fun usage_rpath f_usage usage item rpath = 
	case rpath of
	    [] => bug "usage_rpath"
	  | h :: _ => f_usage usage item h

    (******* external values ********)
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
    val ext_set = (ref ExtSet.empty)
    val ext_set_g = (ref ExtSet.empty)

    (*add_ext*) 
    fun ext_def ext_set item name = 
	let val loc = 
		case item of
		    ExtVar {usage = ref [(loc, _, _)], ...} => loc
		  | ExtStr {usage = ref [loc], ...} => loc
		  | ExtType {usage = ref [loc], ...} => loc
		  | ExtCons {usage = ref [(loc, _)], ...} => loc
		  | ExtSig {usage = ref [loc], ...} => loc
		  | _ => bug "ext_def"
	in add_occ (name, loc);
	   ext_set := ExtSet.add (!ext_set, item)
	end

    fun ext_str_pred acc ext_elem = 
	case ext_elem of
	    ExtStr {access, ...} => equal_acc acc access
	  | _ => false

    fun ext_var_pred acc ext_elem = 
	case ext_elem of
	    ExtVar {access, ...} => equal_acc acc access
	  | _ => false

    fun print_ext () = 
	ExtSet.app P.print_ext (!ext_set)

    (********* variables *********)
    structure VarKey : ORD_KEY =
    struct
        type ord_key = var_elem
	fun compare ({access = acc1, def = loc1, ...} : ord_key, 
		     {access = acc2, def = loc2, ...} : ord_key) = 
	    compare_loc_acc (loc1, loc2) (acc1, acc2)
    end (* structure VarKey *)

    (* variable sets *)
    structure VarSet = RedBlackSetFn(VarKey)
    val var_set = (ref VarSet.empty);
    val var_set_g = (ref VarSet.empty);

    fun find_var p = VarSet.find p (!var_set_g)
    fun exists_var p = VarSet.exists p (!var_set_g)
    fun filter_var p = VarSet.listItems (VarSet.filter p (!var_set_g))

    fun var_usage usage (usage_item as (loc,_,_) :varUse) name = 
	( add_occ (name,loc);
	  usage := usage_item :: !usage
	)

    fun var_def var_set (item as {name, def, ...}:var_elem) = 
	( add_occ (name,def);
	  var_set := VarSet.add (!var_set, item)
	)

    fun var_pred (acc,filepath) ({access, def, ...}:var_elem) = 
	filepath = locFile def andalso equal_acc acc access

    fun print_var () = 
	VarSet.app P.print_var (!var_set)
			  
    fun print_var_g () = 
	VarSet.app P.print_var (!var_set_g)

    (******** structures ********)
    structure StrKey : ORD_KEY =
    struct
        type ord_key = str_elem
	fun compare ({access = acc1, def = loc1, ...} : ord_key,
		     {access = acc2, def = loc2, ...} : ord_key) =
	    compare_loc_acc (loc1, loc2) (acc1, acc2)
    end (* structure StrKey *)
    
    (* structure sets *)
    structure StrSet = RedBlackSetFn(StrKey)
    val str_set = (ref StrSet.empty);
    val str_set_g = (ref StrSet.empty);

    fun find_str p = StrSet.find p (!str_set_g)
    fun exists_str p = StrSet.exists p (!str_set_g)
    fun filter_str p = StrSet.listItems (StrSet.filter p (!str_set_g))

    fun str_usage' usage (usage_item as (loc,_,_) :strUse) = 
	usage := usage_item :: !usage

    fun str_usage usage (usage_item as loc :strUse) name = 
	( add_occ (name,loc);
	  str_usage' usage usage_item
	)

    fun str_usage_rpath usage usage_item rpath = 
	case rpath of
	    [] => 
	    (* this branch should not be used for now *)
	    (* made not to record any implicite occurrence like 
	     * structure A = struct val x end; open A; x*)
	    (* A is used when x is used but should not appear in the occurrence
	     * list*)
	    str_usage' usage usage_item 
	  | h :: _ => str_usage usage usage_item h

    fun str_def str_set (item as {name, def = loc, ...}:str_elem) = 
	( add_occ (name,loc);
	  str_set := StrSet.add (!str_set, item)
	)

    fun str_pred (acc,file) ({access, def, ...}:str_elem) = 
	equal_acc acc access andalso locFile def = file

    fun print_str () = 
	StrSet.app P.print_str (!str_set)

    fun print_str_g () = 
	StrSet.app P.print_str (!str_set_g)

    (********** types **********)
    structure TypKey : ORD_KEY =
    struct
        type ord_key = type_elem
	fun compare ({stamp = stamp1, ...}:ord_key, 
		     {stamp = stamp2, ...}:ord_key) =
	    Stamps.compare (stamp1, stamp2)
    end (* structure TypKey *)
    
    (* type sets *)
    structure TypSet = RedBlackSetFn(TypKey)
    val typ_set = (ref TypSet.empty);
    val typ_set_g = (ref TypSet.empty);

    fun find_typ p = TypSet.find p (!typ_set_g)
    fun exists_typ p = TypSet.exists p (!typ_set_g)
    fun filter_typ p = TypSet.listItems (TypSet.filter p (!typ_set_g))

    fun typ_usage usage (usage_item as loc : typeUse) name = 
	( add_occ (name,loc);
	  usage := usage_item :: !usage
	)
	
    fun typ_def typ_set (item as {name, def, ...}:type_elem) = 
	( add_occ (name,def);
	  typ_set := TypSet.add (!typ_set, item)
	)
	
    fun typ_pred stamp' ({stamp, ...}:type_elem) = 
	Stamps.eq (stamp',stamp)

    fun print_ty () = 
	TypSet.app P.print_type (!typ_set)

    fun print_ty_g () = 
	TypSet.app P.print_type (!typ_set_g)

    (********** constructors *********)
    structure ConKey : ORD_KEY =
    struct
        type ord_key = cons_elem
	fun compare ({name = name1, dataty = s1, ...}:ord_key, 
		     {name = name2, dataty = s2, ...}:ord_key) =
	    case Stamps.compare (s1, s2) of
		EQUAL => String.compare (Symbol.name name1, Symbol.name name2)
	      | ord => ord
    end (* structure ConKey *)
    
    (* constructor sets *)
    structure ConSet = RedBlackSetFn(ConKey)
    val con_set = (ref ConSet.empty);
    val con_set_g = (ref ConSet.empty);

    fun find_cons p = ConSet.find p (!con_set_g)
    fun exists_cons p = ConSet.exists p (!con_set_g)
    fun filter_cons p = ConSet.listItems (ConSet.filter p (!con_set_g))

    fun print_cons () = 
	ConSet.app P.print_cons (!con_set)

    fun print_cons_g () = 
	ConSet.app P.print_cons (!con_set_g)

    (********* signature *********)
    structure SigKey : ORD_KEY =
    struct
        type ord_key = sig_elem
	fun compare ({stamp = stamp1, ...}:ord_key, 
		     {stamp = stamp2, ...}:ord_key)=
	    Stamps.compare (stamp1, stamp2)
    end (* structure SigKey *)
    
    (* signature sets *)
    structure SigSet = RedBlackSetFn(SigKey)
    val sig_set = ref SigSet.empty;
    val sig_set_g = ref SigSet.empty;

    fun find_sig p = SigSet.find p (!sig_set_g)
    fun exists_sig p = SigSet.exists p (!sig_set_g)
    fun filter_sig p = SigSet.listItems (SigSet.filter p (!sig_set_g))

    fun sig_usage usage (item as loc : sigUse) name = 
	( add_occ (name, loc);
	  usage := item :: !usage
	)
	
    fun sig_def sig_set (item as {name, def, ...} : sig_elem) = 
	( add_occ (name, def);
	  sig_set := SigSet.add (!sig_set, item)
	)

    fun sig_pred st ({stamp, ...}:sig_elem) = 
	Stamps.eq (st, stamp)

    fun print_sig () = 
	SigSet.app P.print_sig (!sig_set)
	
    fun print_sig_g () = 
	SigSet.app P.print_sig (!sig_set_g)

    (****** end of set definitions *****)

    fun get_lvar0 set file test test2 (acc as A.PATH (a, slot)) = 
	( case get_lvar0 set file test2 test2 a of
	      NONE => NONE
	    | SOME acc' =>
	      case StrSet.find (str_pred (acc', !file)) (!set) of
		  NONE => bug ("get_lvar0: " ^ A.prAcc acc)
		| SOME {elements = Def l, ...} => 
		  ( case List.find (fn (x, _, _) => x = slot) l of
			NONE => bug ("get_lvar02: " ^ A.prAcc acc)
		      | SOME (_, _, key) => 
			( case test key of
			      NONE => bug ("get_lvar03: " ^ A.prAcc acc)
			    | SOME a => get_lvar0 set file test test2 a
			)
		  )
		| SOME {elements = Constraint (l, acc2), ...} => 
		  ( case List.find (fn (x, _, _) => x = slot) l of
			NONE => bug ("get_lvar04: " ^ A.prAcc acc)
		      | SOME (_, _, slot2) => 
			get_lvar0 set file test test2 (A.PATH (acc2,slot2))
		  )
		| SOME {elements = Alias a, ...} => 
		  get_lvar0 set file test test2 (A.PATH (a,slot))
	)
      | get_lvar0 _ _ _ _ (acc as A.LVAR _) = SOME acc
      | get_lvar0 _ _ _ _ (A.EXTERN _) = NONE
      | get_lvar0 _ _ _ _ _ = bug "get_lvar05"
		
    (* acc can be anything and we gave back an lvar *) 
    fun get_lvar a b c d acc = 
	case get_lvar0 a b c d acc of
	    NONE => bug "get_lvar"
	  | SOME access => access

    fun key_str (Str a) = SOME a
      | key_str _ = NONE

    fun key_val (Var a) = SOME a
      | key_val _ = NONE

    val get_str_lvar0       = get_lvar0 str_set   source     key_str key_str
    val get_str_lvar        = get_lvar  str_set   source     key_str key_str
    fun get_str_lvar_g file = get_lvar  str_set_g (ref file) key_str key_str
    val get_var_lvar0       = get_lvar0 str_set   source     key_val key_str
    val get_var_lvar        = get_lvar  str_set   source     key_val key_str
    fun get_var_lvar_g file = get_lvar  str_set_g (ref file) key_val key_str

    (* functions to propagate a region on all the elements of a path *)
    fun modify [] region = 
	([], region)
      | modify (h :: q) (r1, _) = 
	(q, (r1-1-String.size (Symbol.name h), r1-1))

    fun modify' rpath (_, r2) = 
	let (* +1 pour compenser le -1 qui aura lieu dans 
	     * modify *)
	    val region = (r2+1, r2+1)
	 in modify rpath region
	end

    (* when saying  A.x, or open A ... x, the use of A is added here *)
    (* gives back the lvar corresponding to the structure or variable *)
    fun add_implicite_use (A.PATH (access, slot0)) rpath region is_str = 
	let fun add_uses (a as A.LVAR _) _ _ = 
		let val (rpath, region) = modify rpath region in
		    case StrSet.find (str_pred (a, !source)) (!str_set) of
			NONE => print "pb in add_implicite_use1\n"
		      | SOME {usage, ...} => 
			str_usage_rpath usage (loc_reg region) rpath;
		    a
		end
	      | add_uses (a as (A.PATH (a', slot))) rpath region = 
		let val (rpath, region) = modify rpath region
		    val a' = add_uses a' rpath region
		    val a = get_str_lvar (A.PATH (a',slot))
		in
		    case StrSet.find (str_pred (a, !source)) (!str_set) of
			NONE => print "pb in add_implicite_use1\n"
		      | SOME {usage, ...} => 
			str_usage_rpath usage (loc_reg region) rpath;
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

    fun ext_str_find access = 
	case 
	    ExtSet.find 
		(ext_str_pred access)
		(!ext_set)
	 of NONE => NONE
	  | SOME (ExtStr a) => SOME a
	  | SOME _ => bug "ext_str_find"

    fun ext_var_find access = 
	case 
	    ExtSet.find 
		(ext_var_pred access)
		(!ext_set)
	 of NONE => NONE
	  | SOME (ExtVar a) => SOME a
	  | SOME _ => bug "ext_var_find"

    fun add_implicite_uses_ext (A.EXTERN _) _ _ = ()
      | add_implicite_uses_ext (a as A.PATH (a', _)) rpath region =
	let val (rpath', region) = modify rpath region
	in
	    case ext_str_find a of
		NONE => 
		(* maybe this usage should not be recorded *)
		let val item = ExtStr{access = a, usage = ref [loc_reg region]}
		in usage_rpath ext_def ext_set item rpath
		end
	      | SOME {usage, ...} => 
		(* maybe this usage should not be recorded *)
		usage_rpath str_usage usage (loc_reg region) rpath;
	    add_implicite_uses_ext a' rpath' region
	end
      | add_implicite_uses_ext _ _ _ = bug "add_implicite_uses_ext"

    fun add_var_def var 
		    (r1, _)
		    {str = M.STR {access = parent_acc, ...}, def, name} = 
	( case var of
	      (VC.VALvar {path = SymPath.SPATH [S.SYMBOL (_, "it")],...}) => ()
	    | VC.VALvar {access, typ, path = SymPath.SPATH path, ...} =>
	      let val item = {access = access, 
			      parent = parent_acc,
			      typ = ty_to_ty' (!typ),
			      name = List.last path,
			      def= loc_reg (r1,r1+String.size 
						      (Symbol.name 
							   (List.last path))),
			      usage=ref []} 
	      in var_def var_set item
	      end
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
			    let val (rpath', region) = modify' rpath region
				val triplet = 
				    (loc_reg region, get_ty' typ typ', access)
			    in
				case ext_var_find access of
				    NONE =>
				    let val item = ExtVar {access=access,
							   usage=ref [triplet]}
				    in usage_rpath ext_def ext_set item rpath
				    end
				  | SOME {usage, ...} => 
				    usage_rpath var_usage usage triplet rpath;
				add_implicite_uses_ext suite rpath' region
			    end
			else
			    ()
		    end
		  | _ => bug "add_var_use2"
	      )
	    | SOME true =>
	      let val rpath = rev path
		  val (rpath',region) = modify' rpath region
		  val new_acc = add_implicite_use access rpath' region false
	      in
		  case VarSet.find (var_pred (new_acc, !source)) (!var_set) 
		   of NONE => ()
		    | SOME {usage, ...} => 
		      let val item = (loc_reg region, get_ty' typ typ', access)
		       in usage_rpath var_usage usage item rpath
		      end
	      end
	)
      | add_var_use _ _ _ = ()

    fun add_ty_def tycon region = 
	let val (stamp, path, tycon') = tycon_to_tycon' tycon 
	    val item = { tycon = tycon',
			 name = InvPath.last path,
			 stamp = stamp,
			 def = loc_reg region,
			 usage = ref []
		       }
	 in typ_def typ_set item
	end

    fun add_ty_use ty region = 
	let val region = loc_reg region in
	    case ty_to_ty' ty of
		Conty (General (st, rpath as InvPath.IPATH rpath'), _) =>
		( case TypSet.find (typ_pred st) (!typ_set) 
		   of NONE =>  (*probablement un truc primitif, mais peut 
				* etre un type extern *)
		      if is_available rpath then 
			  (* on a le fichier ou le type est defini *)
			  (*considerer les utilisations implicites*)
			  case ExtSet.find 
				   (fn (ExtType {stamp, ...}) => 
				       Stamps.eq (stamp,st)
				     | _ => false
				   )
				   (!ext_set)
			   of NONE => 
			      let val item = ExtType {stamp = st, 
						      usage = ref [region]}
			      in usage_rpath ext_def ext_set item rpath'
			      end
			    | SOME (ExtType {usage, ...}) => 
			      usage_rpath typ_usage usage region rpath'
			    | SOME _ => bug "add_ty_use"
		      else
			  ()
		    | SOME {usage, ...} => 
		      usage_rpath typ_usage usage region rpath'
		)
	      | _ => ()
	end



    fun add_sig_def sign region = 
	let val {name, stamp, inferred, def, elements, usage} = 
		sig_to_elem sign
	    val item = { name = name, 
			 stamp = stamp,
			 inferred = inferred,
			 def = loc_reg region,
			 elements = elements,
			 usage = usage
		       }
	in
	    (* if the rhs is a named signature, then its use should be counted 
	     *)
	    sig_def sig_set item
	end

    fun add_sig_use (M.SIG {stamp, ...}) region = 
	(* one should check where the signature is defined, and either 
	 * ignore it, or insert it in set_sig or insert it in ext_set*)
	( case SigSet.find (sig_pred stamp) (!sig_set)
	   of NONE => bug "add_sig_use"
	    | SOME {usage, name, ...} => 
	      sig_usage usage (loc_reg region) name
	)
      | add_sig_use _ _ = 
	bug "add_sig_use2"
	
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
			  let val (rpath', region) = modify' rpath region in
			      case ext_str_find access of
				  NONE => 
				  let val item = 
					  ExtStr {access = access,
						  usage = ref [loc_reg region]}
				      
				  in usage_rpath ext_def ext_set item rpath
				  end
				| SOME {usage, ...} => 
				  usage_rpath str_usage usage 
					      (loc_reg region) rpath;
			      add_implicite_uses_ext suite rpath' region
			  end
		      else
			  ()
		    | _ => bug "add_str_use0"
	      end
	    | SOME true => 
	      (*local*)
	      let val rpath = get_path rlzn
		  val (rpath', region) = modify' rpath region
		  val new_acc = add_implicite_use access rpath'
						  region true
	      in
		  case StrSet.find (str_pred (new_acc, !source)) (!str_set)
		   of NONE => 
		      bug ("add_str_use1 " ^ A.prAcc access ^ " " ^ 
			   A.prAcc new_acc)
		    | SOME {usage, ...} => 
		      usage_rpath str_usage usage (loc_reg region) rpath
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
	    (* Pb : 
	     * if you say : 
	     *    structure A = Array; 
	     *    structure s = struct open A end
	     * you will think when binding the content of s that it comes
	     * from inside the current file because its access will be : 
	     * PATH (accessA = LVAR _, slot)
	     * But when trying to simplify PATH ... into an LVAR, you will
	     * arrive on an EXTERN _ and raise a bug

	     * solution : try to simplify in every cases and if it doesn't
	     * work, use the access you began with

	     * consequence : in Def elements of structures, accesses are either
	     * LVAR that are defined in the same file as the structure,
	     * or a PATH that ultimately points to an other file in any other 
	     * cases
	     *)

	    let val access' = 
		    case get_var_lvar0 access of
			NONE => access 
		      | SOME access' => access'
	    in
		(SymPath.last path, Var access')
	    end
	  | B.STRbind (M.STR {access, rlzn = {rpath, ...}, ...}) =>
	    let val access' = 
		    case get_str_lvar0 access of
			NONE => access
		      | SOME access' => access'
	    in
		(InvPath.last rpath, Str access')
	    end
	  | _ => bug "binding_to_symbolkey : non implemente"
          (*| B.CONbind VarCon.datacon => 
	  | B.TYCbind Types.tycon =>
	  | B.SIGbind Modules.Signature =>
	  | B.FSGbind Modules.fctSig =>
	  | B.FCTbind Modules.Functor => 
	  | B.FIXbind Fixity.fixity =>*)
		 
    (* assumption : the binding list (and thus the symbolkey list) are ordered 
     * by increasing slot number, starting with 0 and without any gaps *)
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
			S.eq (SymPath.last path, s)
		      | _ => false
		   )
		   bindings
	   of NONE => bug "slotsymbolkey_to_slotsymbolkey'1"
	    | SOME (B.VALbind (VC.VALvar {access, ...})) => (slot,s,Var access)
	    | SOME _ => bug "slotsymbolkey_to_slotsymbolkey'2"
	)
      | slotsymbolkey_to_slotsymbolkey' (SOME bindings) (slot, s, Str _) =
	( case List.find 
		   (fn (B.STRbind (M.STR {rlzn = {rpath, ...}, ...})) =>
		       S.eq (InvPath.last rpath, s)
		     | _ => false
		   )
		   bindings
	   of NONE => bug "slotsymbolkey_to_slotsymbolkey'3"
	    | SOME (B.STRbind (M.STR {access, ...})) => (slot, s, Str access)
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

	    val def_elements = 
		List.map (slotsymbolkey_to_slotsymbolkey' bl1_option)
		( symbolkeylist_to_slotsymbolkeylist 
		      (List.map binding_to_symbolkey bl2)
		)

	    val item = { name = name_str,
			 access = access,
			 parent = parent_acc,
			 sign = SOME stamp,
			 def = loc_reg region,
			 elements = Def def_elements,
			 usage = ref []}
 	in
	    str_def str_set item
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
	let val () = add_str_use rhs regionRHS
	    val item = { name = name, 
			 access = access,
			 parent = parent_acc,
			 sign = SOME stamp,
			 def = loc_reg region,
			 elements = Alias access',
			 usage = ref []}
	in str_def str_set item
	end
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

	    val def_elements = 
		List.map get_slot'
			 ( symbolkeylist_to_slotsymbolkeylist 
			       (List.map binding_to_symbolkey bindinglist)
			 )
	    val elements = 
		case (!acc, rhs) of
		    (A.NO_ACCESS, _) => 
		    (* special case where the structure is match against an 
		     * empty signature
		     * we don't have the access of the rhs signature, so we 
		     * can't defined a Constraint
		     * so, this is considered an empty definition and 
		     * the rhs is not considered as being used *)
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
	    val item = { name = name, 
			 access = access,
			 parent = parent_acc,
			 sign = SOME stamp,
			 def = loc_reg regionID,
			 elements = elements,
			 usage = ref []}
	in
	    str_def str_set item
	end
      | add_str_sig_alias _ _ _ _ _ _ = 
	bug "add_str_sig_alias"



    fun print_all () = (
	print "****** VAR : \n";print_var ();
	print "****** STR : \n";print_str ();
	print "****** TYP : \n";print_ty ();
	print "****** CON : \n";print_cons ();
	print "****** SIG : \n";print_sig ();
	print "****** EXT : \n";print_ext ();
	print "****** OCC : \n";print_occ ()
    )

    fun print_all_g () = (
	print "****** VAR : \n";print_var_g ();
	print "****** STR : \n";print_str_g ();
	print "****** TYP : \n";print_ty_g ();
	print "****** CON : \n";print_cons_g ();
	print "****** SIG : \n";print_sig_g ();
	print "****** EXT : \n";print_ext ();
	print "****** LVA : \n";print_lvars ();
        print "****** PID : \n";print_pids ();
	print "****** OCC : \n";print_occ_g ()
    )


    fun clear () = (
	clear_lvar ();
	var_set := VarSet.empty;
	typ_set := TypSet.empty;
	con_set := ConSet.empty;
	str_set := StrSet.empty;
	sig_set := SigSet.empty;
	ext_set := ExtSet.empty;
	occ_set := OccSet.empty;
	source := "";
	extRefInfo := (fn _ => NONE)
    )

    fun get_strings (a, b, c, d, e, f, g, h, i) = 
	( SerializeDB.varToString  (VarSet.listItems a),
	  SerializeDB.typeToString (TypSet.listItems b),
	  SerializeDB.consToString (ConSet.listItems c),
	  SerializeDB.strToString  (StrSet.listItems d),
	  SerializeDB.sigToString  (SigSet.listItems e),
	  SerializeDB.extToString  (ExtSet.listItems f),
	  SerializeDB.lvarExtToString (LvarExtSet.listItems g),
	  SerializeDB.pidOptionToString h,
	  SerializeDB.occurrenceListToString (OccSet.listItems i)
	)

    fun get_sets (a, b, c, d, e, f, g, h, i) = 
	( VarSet.fromList (UnSerializeDB.stringToVar  a),
	  TypSet.fromList (UnSerializeDB.stringToType b),
	  ConSet.fromList (UnSerializeDB.stringToCons c),
	  StrSet.fromList (UnSerializeDB.stringToStr  d),
	  SigSet.fromList (UnSerializeDB.stringToSig  e),
	  ExtSet.fromList (UnSerializeDB.stringToExt  f),
	  LvarExtSet.fromList (UnSerializeDB.stringToLvarExt g),
	  UnSerializeDB.stringToPidOption h,
	  OccSet.fromList (UnSerializeDB.stringToOccurrenceList i)
	)
	
    fun get_pickle () = 
	let val (a,b,c,d,e,f,g,h,i) = 
		get_strings (!var_set, !typ_set, !con_set, !str_set,
			     !sig_set, !ext_set, !lvar_ext, !pid, !occ_set)
	    fun insert s [] = 
		[]
	      | insert s (h :: q) = 
		h :: s :: insert s q
	in String.concat (insert "\n" [a,b,c,d,e,f,g,h,i])
	end

    fun load_return source = 
	let val os = TextIO.openIn source
	    fun get_val NONE = bug ("sourcefile " ^ source ^ ":unexpected EOF")
	      | get_val (SOME s) = s
	    fun gs () = get_val (TextIO.inputLine os)
	in
	    get_sets (gs(),gs(),gs(),gs(),gs(),gs(),gs(),gs(),gs())
	end

    fun get_file e = 
	case PidFileSet.find 
		 (fn (pid, _) => persstamps_eq (pid,e)) 
		 (!pid_file) 
	 of NONE => bug "get_file"
	  | SOME (_, filename) => filename

    fun modify_path (a as A.PATH (A.EXTERN e, _)) lv = 
	( case LvarExtSet.find (fn (ext_acc, _) => equal_acc a ext_acc) lv of
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
	    in case VarSet.find (var_pred (lvar,sourcename)) (!va)
		of NONE => bug ("distribution.ExtVar: " ^ A.prAcc access ^ 
				" " ^ sourcename)
		 | SOME {usage = u, name, ...} => 
		   u := !usage @ !u
	    end
	  | ExtStr {access, usage} =>
	    let val (acc, sourcename) = modify_path access lv
		val lvar = get_str_lvar_g sourcename acc
	    in case StrSet.find (str_pred (lvar,sourcename)) (!st) of
		   NONE => bug ("distribution.ExtStr " ^ A.prAcc access ^ " " ^
				A.prAcc lvar)
		 | SOME {usage = u, ...} => u := !usage @ !u
	    end
	  | ExtType {stamp, usage} => ()
	    (* case TypSet.find () (!ty) of
		  NONE => bug "distribution.ExtType"
		| SOME {usage = u, ...} => u := !usage @ !u
	    *)
	  | ExtCons {stamp, name, usage} => ()
	    (* case ConSet.find () (!co) of
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

    fun merge (var,ty,cons,str,sign,ext,lvarext,pid',occ) sourcefile = 
	let val var_set2 = ref var
	    val typ_set2 = ref ty
	    val con_set2 = ref cons
	    val str_set2 = ref str
	    val sig_set2 = ref sign
	    val distrib = distribution (var_set_g, typ_set_g, con_set_g, 
					str_set_g, sig_set_g, !lvar_ext)
	in 
	    ExtSet.app distrib ext;
	    (* ICI, IL FAUT SIMPLIFIER STR_SET2 EN REGARDANT LES DEFINITIONS
	     *  EXTERIEURES (C'EST A DIRE PATH (_) CAR LES AUTRES ONT ETE 
	     * SIMPLIFIEES EN LVAR) ET EN LES REMPLACANT SOMEHOW PAR UN COUPLE
	     * (LVAR, FILEPATH) *)
	    occ_set_g := OccSetG.add (!occ_set_g, (occ, sourcefile));
	    var_set_g := VarSet.union (!var_set_g, !var_set2);
	    typ_set_g := TypSet.union (!typ_set_g, !typ_set2);
	    con_set_g := ConSet.union (!con_set_g, !con_set2);
	    str_set_g := StrSet.union (!str_set_g, !str_set2);
	    sig_set_g := SigSet.union (!sig_set_g, !sig_set2);
	    lvar_ext := LvarExtSet.union (lvarext,!lvar_ext);
	    (* ext_set should be empty anyway *)
	    pid_file := PidFileSet.add
			    (!pid_file, 
			     case pid' of
				 NONE => bug "merge"
			       | SOME pid'' => (pid'', sourcefile)
			    );
	    let val pid = get_pid sourcefile
		val (to_be_added, others) = 
		    ExtSet.partition 
			(fn (ExtStr {access, ...}|ExtVar {access, ...}) => 
			    persstamps_eq (get_hash access, pid)
			  | _ => false)
			(!ext_set_g)
	    in
		ExtSet.app distrib to_be_added;
		ext_set_g := others
	    end
	end

    fun merge_pickle sourcefile pickle = 
	case String.tokens (fn x => x = #"\n") pickle of
	    [a,b,c,d,e,f,g,h,i] => 
	    merge (get_sets (a,b,c,d,e,f,g,h,i)) sourcefile
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
	case StrSet.find (str_pred (access_par, file)) (!str_set_g)
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
		find_son x y file (fn(Str a,a2) => equal_acc a a2 | _ => false)
	    fun str access = 
		case StrSet.find (str_pred (access, file)) (!str_set_g)
		 of NONE => bug "externalize1"
		  | SOME {parent = SOME access_loc_parent, ...} =>
		    let val access_lvar_parent= get_str_lvar access_loc_parent
			val access_ext_parent = str access_lvar_parent
			val is_alias = 
			    ( case StrSet.find 
				       (str_pred (access_lvar_parent, file))
				       (!str_set_g)
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
				 persstamps_eq (e,pid) andalso
				 equal_acc y access
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
		find_son x y file (fn (Var a, a2)=> equal_acc a a2 | _ =>false)
	in 
	    case VarSet.find (var_pred (access,file)) (!var_set_g)
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
				ext_set_g := 
				ExtSet.add 
				    ( !ext_set_g,
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
	      (!var_set_g);
	  var_set_g := 
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
	      (!var_set_g);
	  StrSet.app
	      ( fn {access, def, usage, ...} => 
		   (if locFile def = file then 
			let val usage = 
				List.filter 
				    (fn x => locFile x <> file)
				    (!usage)
			in
			    if not (List.null usage) then
				ext_set_g := 
				ExtSet.add 
				    ( !ext_set_g,
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
	      (!str_set_g);
	  str_set_g := StrSet.filter 
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
			   (!str_set_g)
	)
	  
    fun test () =
	let val (a,b,c,d,e,f,g,h,i) = 
		get_sets (get_strings (!var_set, !typ_set, !con_set, !str_set,
				       !sig_set, !ext_set, !lvar_ext, !pid,
				       !occ_set)
			 )
	in
	    if VarSet.numItems a <> VarSet.numItems (!var_set) then
		bug "test ().var length"
	    else
		();
	    if TypSet.numItems b <> TypSet.numItems (!typ_set) then
		bug "test ().type length"
	    else
		();
	    if ConSet.numItems c <> ConSet.numItems (!con_set) then
		bug "test ().cons length"
	    else
		();
	    if StrSet.numItems d <> StrSet.numItems (!str_set) then
		bug "test ().str length"
	    else
		();
	    if SigSet.numItems e <> SigSet.numItems (!sig_set) then
		bug "test ().sig length"
	    else
		();
	    if ExtSet.numItems f <> ExtSet.numItems (!ext_set) then
		bug "test ().ext length"
	    else
		();
	    if LvarExtSet.numItems g <>LvarExtSet.numItems (!lvar_ext) then
		bug "test ().lvar_ext length"
	    else
		();
	    if OccSet.numItems i <>OccSet.numItems (!occ_set) then
		bug "test ().lvar_ext length"
	    else
		();
	    VarSet.app P.print_var a;
	    TypSet.app P.print_type b;
	    ConSet.app P.print_cons c;
	    StrSet.app P.print_str d;
	    SigSet.app P.print_sig e;
	    ExtSet.app P.print_ext f;
	    print_lvars ();
	    OccSet.app P.print_occ i
	end


end (*end local*)
end (*structure Database *)
