structure Ens_var :> ENS_VAR = 
struct

local 
    structure EM = ErrorMsg
    structure A = Access
    structure S = Symbol
    structure T = Types
    structure TU = TypesUtil
    structure PP = PrettyPrintNew
    structure VC = VarCon
    structure M = Modules
    structure EP = Ens_print
in 

   fun bug msg = EM.impossible("Bugs in Ens_var: "^msg);

   type file = EP.file
   type location = EP.location
   type var_elem = EP.var_elem
   type type_elem = EP.type_elem
   type cons_elem = EP.cons_elem
   type str_elem = EP.str_elem
   type sig_elem = EP.sig_elem

   fun locFile ((f,_,_) : location) = f

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






   fun compare_var (VC.VALvar {access = access1, ...}, 
		    VC.VALvar {access = access2, ...}) = 
       compare_acc (access1, access2)
     | compare_var _ = bug "compare_var"

   (* variables *)
   structure VarKey : ORD_KEY =
   struct
     type ord_key = var_elem
     fun compare ({var = var1, def = loc1, ...} : ord_key, 
		  {var = var2, def = loc2, ...} : ord_key) = 
	 case String.compare (locFile loc1, locFile loc2) of
	     EQUAL => compare_var (var1, var2)
	   | ord => ord 
   end (* structure VarKey *)

   (* variable sets *)
   structure VarSet = RedBlackSetFn(VarKey)





   fun compare_type (T.DEFtyc tycon, T.DEFtyc tycon2) = 
       Stamps.compare (#stamp tycon, #stamp tycon2)
     | compare_type 
	   (T.GENtyc ({stamp = stamp,  
		       kind = T.DATATYPE{index = index,  ...}, ...}),
	    T.GENtyc ({stamp = stamp2, 
		       kind = T.DATATYPE{index = index2, ...}, ...})) = 
       ( case Stamps.compare (stamp, stamp2) of 
	     EQUAL => Int.compare (index,  index2)
	   | ord => ord
       )
     | compare_type (T.DEFtyc _, _) = GREATER
     | compare_type _ = LESS

   (* type *)
   structure TypeKey : ORD_KEY =
   struct
      type ord_key = type_elem
      val compare = 
       fn (x:ord_key, y:ord_key) => compare_type (#tycon x, #tycon y)
   end (* structure TypeKey *)
   
   (* type sets *)
   structure TypeSet = RedBlackSetFn(TypeKey)






   fun get_family cons = 
       let 
	   val T.DATACON {typ = ty, ...} = cons

	   fun get_tycon (T.GENtyc (t as {kind = T.DATATYPE _, ...})) = 
	       SOME t
	     | get_tycon _ = NONE
	   and get_ty (T.CONty (tycon, tyl)) = 
	       if List.length tyl <> 2 then
		   get_tycon tycon
	       else
		   get_ty (List.last tyl)
	     | get_ty (T.POLYty {tyfun = T.TYFUN {body, ...}, ...}) = 
	       get_ty body
	     | get_ty _ = NONE
       in
	   get_ty ty
       end

   fun get_type_stamp cons = 
       case get_family cons of
	   SOME {kind = T.DATATYPE{index, stamps, ...}, ...} => 
	   SOME (Vector.sub (stamps, index))
	 | _ => NONE

   fun compare_cons_type cons1 cons2 = 
       case (get_type_stamp cons1, get_type_stamp cons2) of
	   (SOME stamp, SOME stamp2) => Stamps.compare (stamp, stamp2)
	 | _ => LESS
		
   fun compare_cons_name (T.DATACON {name = S.SYMBOL (_, str)  , ...}, 
			  T.DATACON {name = S.SYMBOL (_, str2) , ...}) =
       String.compare (str, str2)

   fun compare_cons pair = 
       case compare_cons_name pair of
	   EQUAL => compare_cons_type (#1 pair) (#2 pair)
	 | ord => ord

   fun cons_eq x y = compare_cons (x, y) = EQUAL

   (* constructors *)
   structure ConsKey : ORD_KEY =
   struct
      type ord_key = cons_elem
      fun compare ({cons = cons, ...}:ord_key, {cons = cons2, ...}:ord_key) =
	  compare_cons (cons, cons2)
   end (* structure ConsKey *)
   
   (* constructor sets *)
   structure ConsSet = RedBlackSetFn(ConsKey)





   (* structures *)
   structure StrKey : ORD_KEY =
   struct
      type ord_key = str_elem
      fun compare ({str = M.STR {access = access1, ...}, 
		    def = loc1, ...} : ord_key,
		   {str = M.STR {access = access2, ...}, 
		    def = loc2, ...} : ord_key) = 
	  ( case String.compare(locFile loc1, locFile loc2)
	     of EQUAL => compare_acc (access1, access2)
	      | ord => ord
	  )
	| compare _ = bug "StrKey.compare"
   end (* structure StrKey *)
   
   (* structure sets *)
   structure StrSet = RedBlackSetFn(StrKey)





   fun compare_sig (M.SIG {stamp = stamp1, ...} : M.Signature,
		    M.SIG {stamp = stamp2, ...} : M.Signature) = 
       Stamps.compare (stamp1, stamp2)  
     | compare_sig _ = bug "StrKey.compare"

   (*signature*)
   structure SigKey : ORD_KEY =
   struct
      type ord_key = sig_elem
      fun compare ({sign = sign1, ...}:ord_key, {sign = sign2, ...}:ord_key)=
	  compare_sig (sign1, sign2)
   end (* structure SigKey *)
   
   (* signature sets *)
   structure SigSet = RedBlackSetFn(SigKey)





   (*the environment iself*)
   val ens_var = (ref VarSet.empty);   (*contains def and use of variables,
					*and functions*)
   val ens_type = (ref TypeSet.empty); (*contains type and datatype def and
					*explicite use*)
   val ens_cons = (ref ConsSet.empty); (*contains type constructors use*)
   val ens_str = (ref StrSet.empty);
   val ens_sig = (ref SigSet.empty);

   val temp_map = (ref []):(A.access * ((int * A.access) list ref)) list ref;
   val source = ref ""
   val extRefInfo = ref (fn x => NONE) : ((S.symbol -> string option) ref)
   val pid = ref NONE : PersStamps.persstamp option ref
   val lvar_ext = ref [] : (A.access * A.access) list ref


   (* getting the object in database with the given access *)
   fun find_str (a as (A.LVAR _ | A.PATH (A.EXTERN _, _))) = 
       StrSet.find 
	   (fn {str = M.STR {access, ...}, ...} => a = access | _ => false)
	   (!ens_str)
     | find_str (A.PATH (a, slot)) =
       ( case find_str_path a slot of
	     NONE => NONE
	   | SOME (_, access') => find_str access'
       )
     | find_str _ = bug "find_str"

   and find_str_path a slot =
       case find_str a of
	   NONE => NONE
	 | SOME {map, ...} => List.find (fn (x, _) => x = slot) (!map)

   fun find_var' (a as (A.LVAR _ | A.PATH (A.EXTERN _, _))) = 
       VarSet.find 
	   (fn {var = VC.VALvar {access, ...}, ...} => a=access | _ => false)
	   (!ens_var)
     | find_var' (A.PATH (a, slot)) = 
       ( case find_str_path a slot of
	   NONE => NONE
	 | SOME (_, access') => find_var' access'
       )
     | find_var' _ = bug "find_var'"

   fun find_var a = 
       case find_var' a of
	   NONE => 
	   VarSet.find 
	       (fn {var=VC.VALvar {access, ...}, ...} =>a=access | _ =>false)
	       (!ens_var)
	 | v => v

   val rtoS = EP.rtoS
   val stoS = EP.stoS
   val ptoS = EP.ptoS
   val rptoS = EP.rptoS

   (*********************************************************************)
   (**************************part concerning signatures ****************)
   (*********************************************************************)

   fun add_sig_def sign (r1, r2) =
       ens_sig := SigSet.add (!ens_sig, {sign = sign, 
					 def = (!source, r1, r2), 
					 alias = ref [], 
					 usage = ref []})

   fun add_sig_use name' sign (r1, r2) = 
       case SigSet.find 
		(fn x => compare_sig (#sign x, sign) = EQUAL) 
		(!ens_sig)
	of NONE => ()
	 | SOME {usage, ...} => 
	   usage := ((!source, r1, r2), name')  :: (!usage)

   fun add_sig_alias name' (sign as M.SIG {name, ...}) (r1, r2) = 
       ( case SigSet.find 
		  (fn x => compare_sig (#sign x, sign) = EQUAL)
		  (!ens_sig)
	  of NONE => ()
	   | SOME {alias, def = (loc, r1', r2'), usage, sign = sign'} => 
	     if name <> SOME name' orelse not (r1<r1' andalso r2'<r2) then
		 alias := ((!source, r1, r2), name')  :: (!alias)
	     else (*on definit le s de signature s = sig ... end*)
		 ens_sig := SigSet.add (!ens_sig, 
					{sign = sign', 
					 def = (loc, r1, r2), 
					 usage = usage, 
					 alias = alias})
       )
     | add_sig_alias _ _ _ = ()


   (************************************************************************)
   (********************part concerning structure content*******************)
   (************************************************************************)

   fun add_mapping str_access var_slot var_access = 
       case List.find (fn x => #1 x = str_access) (!temp_map) of
	   NONE => 
	   temp_map := (str_access, ref [(var_slot, var_access)]) 
		       :: (!temp_map)
	 | SOME (_, l) => l := (var_slot, var_access) :: (!l);

   fun add_str_def str (r1, r2) access = 
       let 
	   val struc = {str = str, 
			def = (!source, r1, r2), 
			usage = ref [], 
			map = ref []}
       in
	   case List.find (fn x => #1 x = access) (!temp_map) of
	       NONE => ()
	     | SOME (_ , l) => #map struc := (!l);
	   ens_str := StrSet.add (!ens_str, struc)
       end;

   (*Pour les signatures : il faut probablement rajouter qqchose ici?*)
   fun add_str_bnd str old_access new_access (r1, r2) = 
       case StrSet.find 
		(fn {str = M.STR {access, ...}, ...} => access = old_access 
		  | _ => bug "add_str_bnd") (!ens_str)
	of NONE => ()(*print "Pas de binding\n" A FAIRE APRES*)
	 | SOME {map, str = M.STR _, ...} => 
	   ens_str := StrSet.add (!ens_str, {str = str, 
					     def = (!source, r1, r2), 
					     usage = ref [], 
					     map = map})
	 | _ => bug "add_str_bnd"
		    
   local
       fun incl (access as A.PATH (access2, _)) old_access = 
	   access = old_access orelse incl access2 old_access
 	 | incl access old_access = access = old_access
			 
       fun incl2 (access as A.PATH (access2, slot)) old_access new_access = 
	   if access = old_access then 
	       new_access 
	   else  
	       A.PATH (incl2 access2 old_access new_access, slot) 
	 (*verifier que ca fait bien ce qu'il faut ici en dessous*)	
	 | incl2 access _ new_access= new_access 
   
   in
       local
	   fun pred_1 old_access {var = (VC.VALvar {access, ...}), ...} =
	       incl access old_access
	     | pred_1 _ (_:VarSet.item) = false

	   fun modi old_access new_access 
		    {var = VC.VALvar {access, typ, prim, path}, def, usage} =
	       {var = VC.VALvar {access = incl2 access old_access new_access,
				 typ = typ, 
				 prim = prim, 
				 path = path}, 
		def = def, 
		usage = usage
	       }
	     | modi _ _ _ = bug "modi"
       in
           fun change_access_var old_access new_access =        
	       let val (to_be_changed, no_change) = 
		       VarSet.partition (pred_1 old_access) (!ens_var)
		   val changed = 
		       VarSet.map (modi old_access new_access) to_be_changed
	       in
		   ens_var := VarSet.union (changed, no_change)
	       end
       end

       local 
	   fun pred_2 old_access {str = M.STR {access, ...}, ...} = 
	       incl access old_access
	     | pred_2 _ (_:StrSet.item) = false

	   fun modi2 old_access 
		     new_access 
		     {str=M.STR{access, sign, rlzn, prim}, def, usage, map} =
	       {str = M.STR {access = incl2 access old_access new_access, 
			     sign = sign, 
			     rlzn = rlzn, 
			     prim = prim},
		def = def,
		usage = usage,
		map = map
	       }
	     | modi2 _ _ _= bug "modi2"
       in
           fun change_access_str old_access new_access = 
	       let val (to_be_changed, no_change) = 
		       StrSet.partition (pred_2 old_access) (!ens_str)
		   val changed = 
		       StrSet.map (modi2 old_access new_access) to_be_changed
	       in
		   ens_str := StrSet.union (changed, no_change)
	       end
       end
   end

   (************************************************************************)
   (********************part concerning structures *************************)
   (************************************************************************)

   fun add_str_use (str as M.STR {access, 
				  rlzn = {rpath = InvPath.IPATH rpath, ...},
				  ...}:M.Structure)
		   (r1, r2) = 
       ( case find_str access of
	     NONE => ( case (!extRefInfo) (List.last rpath) of
			   NONE => ()
			 | SOME file => 
			   ens_str := 
			   StrSet.add 
			       (!ens_str, 
				{str = str, 
				 def = (file, ~1, ~1), 
				 usage = ref[((!source, r1, r2), T.UNDEFty)],
				 map = ref []
				}
			       )
		     )
	   | SOME {usage, ...} => 
	     usage := ((!source, r1, r2), T.UNDEFty) :: (!usage)
       )
     | add_str_use _ _ = ()

   
   (************************************************************************)
   (*******functions about variable bindings and uses in expressions********)
   (************************************************************************)
       
   (*modifications of the environment when variables are defined, 
    *or used in expressions*)
   fun add_var_def var (r1, r2) =
       case var of
	   (VC.VALvar {path = SymPath.SPATH [S.SYMBOL (_, "it")], ...}) => ()
	 | _ => ens_var := 
		VarSet.add(!ens_var, 
			   {var=var, def=(!source, r1, r2), usage=ref []})

   fun add_var_use (v as VC.VALvar {access, path = SymPath.SPATH path, ...}) 
		   (r1, r2) = 
       ( case find_var access of
	     NONE => ( case (!extRefInfo) (List.hd path) of 
			   SOME str =>
			   ens_var := 
			   VarSet.add
			       (!ens_var, 
				{var=v,
				 def=(str, ~1, ~1), 
				 usage=ref [((!source, r1, r2), T.UNDEFty)]
				}
			       )
			 | _ =>
			   ()
		     )
	   | SOME {usage, ...} => 
	     usage := ((!source, r1, r2), T.UNDEFty) :: (!usage)
       )
     | add_var_use _ _ = bug "add_var_use"
       
   (*add an instantiated type to the list of types of the variable with 
    *the given access*)
   fun add_var_inst (ty:T.ty) (acc:A.access) = 
       case find_var acc of
	   NONE => ()
	 | SOME {usage, ...} =>
	   let
	       fun last nil _ = NONE
		 | last (t::q) n = ( case (last q (n+1), t) of 
					 (NONE, (_, T.UNDEFty)) => SOME n
				       | (NONE, _) => NONE
				       | (s, _) => s
				   )
	       val n = last (!usage) 0
	       fun tail ((loc, _)::q) 0 = (loc, ty)::q
		 | tail l n = hd l :: tail (tl l) (n-1)
	   in
	       case n of 
		   NONE => (*bug "add_var_inst.last"*)()
		 | SOME n => usage := tail (!usage) n
	   end

     
     
   (************************************************************************)
   (******functions about type declarations and explicit type uses**********)
   (************************************************************************)

   (*add a type definition in the list*)
   fun add_type_def tycon (r1, r2) = 
       ens_type := 
       TypeSet.add 
	   (!ens_type, 
	    {tycon = tycon, def = (!source, r1, r2), usage = ref []});

   fun add_type_use (ty as T.CONty (tycon, _)) (r1, r2) = 
       ( case TypeSet.find 
		  (fn x => compare_type (#tycon x, tycon) = EQUAL) 
		  (!ens_type)
	  of NONE => ()
	   | SOME {usage, ...} => 
	     usage := ((!source, r1, r2), ty) :: (!usage)
       )
     | add_type_use _ _ = ()
			  

   (************************************************************************)
   (*************functions about data constructors**************************)
   (************************************************************************)

   (*give back the region where is defined the datatype that has the 
    *constructor, or ("", -1, -1) if not present
    *)
   fun find_reg_def cons = 
       let
	   fun default (SOME {def, ...} : type_elem option) = def
	     | default NONE = ("", ~1, ~1)
       in
	   case get_family cons of 
	       SOME tycon =>
	       default (TypeSet.find 
			    (fn x => compare_type (T.GENtyc tycon, #tycon x)
				     = EQUAL) 
			    (!ens_type)
		       )
	     | _ => default NONE
       end
       
   (*add a type constructor use to the list*)
   fun add_cons_use datac (r1, r2) = 
       case ConsSet.find (fn x => cons_eq datac (#cons x)) (!ens_cons) of
	   NONE => ( case find_reg_def datac of
			 ("", ~1, ~1) => ()
		       | loc => 
			 ens_cons := 
			 ConsSet.add 
			     (!ens_cons, 
			      {cons = datac, 
			       def = loc, 
			       usage = ref [((!source, r1, r2), T.UNDEFty)]
			      }
			     )
		   )
	 | SOME {usage, ...} => 
	   usage := ((!source, r1, r2), T.UNDEFty) :: (!usage)
       
   (*add the type of a constructor type use*)
   fun add_cons_inst cons ty = 
       case ConsSet.find (fn x => cons_eq cons (#cons x)) (!ens_cons) of
	   NONE => ()(*bug "add_cons_inst1"*)
	 | SOME {usage = usage as ref (h::q), ...} => 
	   usage := (#1 h, ty) :: q 
	 (*A MODIFIER, IL FAUT FAIRE COMME POUR LES VARIABLES*)
	 | _ => bug "add_cons_inst2"
	   

   (**********************************************************************)
   (**************************** print  **********************************)
   (**********************************************************************)

   fun print_var () = 
       VarSet.app EP.print_var (!ens_var)
   fun print_types () =
       TypeSet.app EP.print_type (!ens_type)
   fun print_cons () =
       ConsSet.app EP.print_cons (!ens_cons)
   fun print_str () =
       StrSet.app EP.print_str (!ens_str)
   fun print_sig () =
       SigSet.app EP.print_sig (!ens_sig)
   fun print_all () = (
       print_var ();print "\n";
       print_types ();print "\n";
       print_cons ();print "\n";
       print_str ();print "\n";
       print_sig ();print "\n"
   )

   fun print_ext () = (
       case pid of 
	   ref (SOME pid) => print (PersStamps.toHex pid ^ "\n")
	 | _ => ();
       List.app (fn (x, y) => print (A.prAcc x ^ "->" ^ A.prAcc y ^ ", ")) (!lvar_ext);
       print "\n"
   )

   (*********************************************************************)
   (************************** others ***********************************)
   (*********************************************************************)

   fun set_source src = 
       source := src

   fun set_eri eri = 
       (extRefInfo := eri)

   fun set_pid pid' = 
       pid := SOME pid'

   fun add_lvar_ext lvar ext = 
       lvar_ext := (lvar, ext) :: (!lvar_ext)

   (*initialization of the environment*)
   fun clear () = ( ens_var := VarSet.empty;
		    ens_type := TypeSet.empty;
		    ens_cons := ConsSet.empty;
		    ens_str := StrSet.empty;
		    ens_sig := SigSet.empty;
		    temp_map := []
		  )

			      
   fun give_all () = 
       (VarSet.listItems (!ens_var),
	TypeSet.listItems (!ens_type),
	ConsSet.listItems (!ens_cons),
	StrSet.listItems (!ens_str),
	SigSet.listItems (!ens_sig))


end (*local*)

end (*structure Ens_var*)
