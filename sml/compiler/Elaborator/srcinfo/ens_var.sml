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
in 

   fun bug msg = EM.impossible("Bugs in Ens_var: "^msg);

   (*type 'a elem = {elem : 'a, def : int * int, usage : (int * int) list ref, instance : T.ty list ref};*)

   type file = string
   type location = file * int * int

   fun locFile ((f,_,_) : location) = f

   fun varLvar (VC.VALvar{access,...}) = A.accLvar access
     | varLvar _ = NONE

   (* variables *)
   structure VarKey : ORD_KEY =
   struct
     type var = {var : VC.var, def : location, usage : (location * T.ty) list ref}
     fun compare ({var=v1,location=loc1,...}, {var=v2,location=loc2,...}) = 
	 case String.compare(locFile loc1, locFile loc2)
	  of EQUAL =>
	      (case (varLvar v1, valLvar v2)
	         of (SOME l1, SOME l2) => Int.compare (l1,l2)
		  | _ => bug "VarKey.compare")
	   | ord => ord
   end (* structure VarKey *)

   (* variable sets *)
   structure VarSet = RedBlackSetFn(VarKey)

   type env_var = var env;
   type ty = {tycon : T.tycon, def : int * int, usage : (int * int) list ref, instance : T.ty list ref};
   type env_ty = ty env;
   type cons = {cons : T.datacon, def: int * int, usage : (int * int) list ref, instance : T.ty list ref};
   type env_cons = cons env;
   type str = {str : M.Structure, def: int * int, usage : (int * int) list ref, instance : T.ty list ref,
	       map : (int * A.access) list ref}
   type env_str = str env;

   (*the environment iself*)
   val ens_var = (ref VarSet.empty):env_var;       (*contains def and use of variables, and functions*)
   val ens_types = (ref TypeSet.empty):env_ty;      (*contains type and datatype def and explicite use*)
   val ens_cons = (ref ConsSet.empty):env_cons;     (*contains type constructors use*)
   val ens_str = (ref StrSet.empty):env_str;
   val temp_map = (ref []):(A.access * ((int * A.access) list ref)) list ref;

   val stat_env = ref (StaticEnv.empty);     (*used only for printing without `?.' everywhere*)




   (*****************************************************************************************************)
   (***********************************part concerning structures****************************************)
   (*****************************************************************************************************)

   fun add_mapping str_access var_slot var_access = 
       case List.find (fn x => #1 x = str_access) (!temp_map) of
	   NONE => temp_map := (str_access, ref [(var_slot, var_access)]) :: (!temp_map)
	 | SOME (_, l) => l := (var_slot, var_access) :: (!l);

   fun add_str_def str region access = 
       let 
	   val struc = {str = str, def = region, usage = ref [], instance = ref [], map = ref []}
       in
	   case List.find (fn x => #1 x = access) (!temp_map) of
	       NONE => (print "No export?\n")
	     | SOME (_ , l) => #map struc := (!l);
	   ens_str := struc :: (!ens_str)
       end;

   (*on verra apres pour les signatures et ce qu'il faut probablement rajouter ici*)
   fun add_str_bnd str old_access new_access = 
       case List.find (fn {str = M.STR {access, ...}, ...} => access = old_access) (!ens_str) of
	   NONE => print "Pas de binding\n"
	 | SOME {map, ...} => ens_str := {str = str, def = (~1, ~1), usage = ref [], instance = ref [], map = map} :: (!ens_str);
       
   local
       fun incl (access as A.PATH (access2, _)) old_access = access = old_access orelse incl access2 old_access
 	 | incl access old_access = access = old_access
			 
       fun incl2 (access as A.PATH (access2, slot)) old_access new_access = 
	   if access = old_access then 
	       new_access 
	   else  
	       A.PATH (incl2 access2 old_access new_access, slot) 
	 | incl2 access _ new_access= new_access (*verifier que ca fait bien ce qu'il faut ici*)
			  
       fun pred_1 {var = (VC.VALvar {access, ...}), ...} old_access = incl access old_access
	 | pred_1 (_:var) _ = false

       fun pred_2 {str = M.STR {access, ...}, ...} old_access = incl access old_access
	 | pred_2 (_:str) _ = false

       fun change p ens old_access = List.partition (fn x => p x old_access) ens

       fun modi {var = VC.VALvar {access, typ, prim, path}, def, usage, instance} old_access new_access=
	   {var = VC.VALvar {access = incl2 access old_access new_access, typ = typ, prim = prim, path = path}, 
	    def = def, 
	    usage = usage,  
	    instance = instance 
	   }

       fun modi2 {str = M.STR {access, sign, rlzn, prim}, def, usage, instance, map} old_access new_access = 
	   {str = M.STR {access = incl2 access old_access new_access, sign = sign, rlzn = rlzn, prim = prim},
	    def = def,
	    usage = usage,
	    instance = instance,
	    map = map
	   }

       fun change_access_var old_access new_access =        
	   let val (to_be_changed, no_change) = change pred_1 (!ens_var) old_access 
	       val changed = List.map (fn x => modi x old_access new_access) to_be_changed
	   in
	       ens_var := changed @ no_change
	   end

       fun change_access_str old_access new_access = 
	   let val (to_be_changed, no_change) = change pred_2 (!ens_str) old_access 
	       val changed = List.map (fn x => modi2 x old_access new_access) to_be_changed
	   in
	       ens_str := changed @ no_change
	   end
   in
       fun change_access old_access new_access = (
	   change_access_var old_access new_access;
	   change_access_str old_access new_access
       )
   end
   
   (************************************************************************************************************)
   (*************************functions about variable bindings and uses in expressions**************************)
   (************************************************************************************************************)
       
   (*modifications of the environment when variables are defined, or used in expressions*)
   fun add_var_def var region =
       case var of
	   (VC.VALvar {path = SymPath.SPATH [S.SYMBOL (_, "it")], ...}) => ()
	 | _ => ens_var := {var=var, def=region, usage=ref [], instance = ref []} :: (!ens_var);
       
   (*gives you the LVAR option corresponding to the access you're giving*)	   
   fun findpath a = 
       let
	   
	   (*fun pr l = 
		 List.app (fn (x, _) => print (Int.toString x ^ ", ")) l;*)
	   
	   fun find_path (a as (A.LVAR _ | A.PATH (A.EXTERN _, _))) = 
	       ( case List.find (fn {str = M.STR {access, ...}, ...} => access = a | _ => false) (!ens_str) of
		     NONE => []
		   | SOME ({map, ...}:str) => !map
	       )
	     | find_path (A.PATH (a, slot)) =
	       ( case List.find (fn (x, _) => x = slot) (find_path a) of
		     NONE => []
		   | SOME (p as (_, a')) => ( case find_path a' of
						  [] => [p]
						| r => r
					    )
	       )
       in
	   case a of
	       (A.LVAR _ | A.PATH (A.EXTERN _, _)) => SOME a (*it won't work with module usages*)
	     (*il suffit de faire un if qui regarde si c'est un module*)
	     (*en meme temps, si c'est un module, on fait rien avec? foncteur!*)
	     | _ => (
 	       case find_path a of
		   [(_, acc)] => SOME acc
		 | _ => NONE
	       )
       end
       
   fun add_var_use access region = 	   
       let
	   fun add_not_path l access =
	       let 
		   fun valid {var = VC.VALvar{access = access2, ...}, ...} = access = access2
		     | valid (_:var) = false
	       in
		   case List.find valid l of
		       NONE => ()
		     | SOME {usage = l, ...} => l := region :: (!l)
	       end    
	   in
	   case findpath access of
	       NONE => ()
	     | SOME acc => add_not_path (!ens_var) acc
       end
       
   (*add an instantiated type to the list of types of the variable with the given access*)
   fun add_var_inst (ty:T.ty, acc:A.access) = 
       let
	   fun add_inst (x:T.ty * A.access) = 
	       let 
		   fun find (nil:var list) = ()
		     | find ({var = VC.VALvar {access, ...}, instance, ...}::q) = 
		       if access = #2 x then 
			       instance := (#1 x) :: (!instance)
		       else 
			   find q
		     | find (t::q) = (print "Problem in Ens_var.add_var_inst\n"; find q)
	       in
		   find (!ens_var)
	       end
       in
	   case findpath acc of
	       NONE => ()
	     | SOME access => add_inst (ty, access)
       end
       
     
     
   (****************************************************************************************************************)
   (****************************functions about type declarations and explicit type uses****************************)
   (****************************************************************************************************************)

   (*add a type definition in the list*)
   fun add_type_def tycon region = 
       ens_types := {tycon = tycon, def = region, usage = ref [], instance = ref []} :: (!ens_types);

   fun add_type_use ( typ as (T.CONty (T.DEFtyc tyc, tyl))) region = 
       let
	   fun find (nil:ty list) = ()
	     | find ({tycon=T.DEFtyc tycon, def, usage, instance}::q) = 
	       if Stamps.eq (#stamp tyc, #stamp tycon) then (
		   usage := region :: (!usage);
		   instance := typ :: (!instance)
		   ) else 
	       find q
	     | find (_::q) = find q
       in
	   find (!ens_types)
       end
     | add_type_use ( typ as (T.CONty (T.GENtyc (tyc as {kind=T.DATATYPE {index, ...}, ...}), tyl))) region = 
       let
	   fun find (nil:ty list) = ()
	     | find ({tycon=T.GENtyc (tycon as {kind=T.DATATYPE{index=index2, ...}, ...}), def, usage, instance}::q) = 
	       if Stamps.eq (#stamp tyc, #stamp tycon) andalso index = index2 then (
		   usage := region :: (!usage);
		   instance := typ :: (!instance)
		   ) else 
	       find q
	     | find (_::q) = find q
       in
	   find (!ens_types)
       end
     | add_type_use _ _ = ();(*should not happen*)



   (****************************************************************************************************************)
   (****************************functions about types construtors***************************************************)
   (****************************************************************************************************************)

   (*give back the region where is defined the datatype that has the constructor symbol, or (-1, -1) if not present*)
   fun find_cons symbol = 
       let 
	   fun find (nil:ty list) = (*(print "Ens_var : constructor undefined\n";*) (~1, ~1)
	     | find ({tycon = T.GENtyc{kind=T.DATATYPE {index, family={members, ...}, ...}, ...}, def, ...}::q) =
	       let 
		   val list_cons = #dcons (Vector.sub (members, index))
		   fun find2 (nil:T.dconDesc list) = (~1, ~1)
		     | find2 ({name, ...}::q) = if name = symbol then def else find2 q
		   val region = find2 list_cons
	       in
		   if region = (~1, ~1) then find q else region
	       end
	     | find (t::q) = find q
	       
       in
	   find (!ens_types)
       end

   (*add a type constructor use to the list*)
   fun add_cons_use (T.DATACON cons) region = 
       let 
	   fun add (nil:cons list) = 
	       ens_cons := {cons=T.DATACON cons, def = find_cons (#name cons), usage=ref [region], instance=ref []} :: (!ens_cons)
	     | add ({cons = T.DATACON cons2, usage, ...}::q) = 
	       if #name cons = #name cons2 then (*EST-CE BON?*)
		   usage := region :: (!usage)
	       else
		   add q
       in
	   add (!ens_cons)
       end;

   (*add the type of a constructor type use*)
   fun add_cons_inst (T.DATACON cons) ty = 
       let 
	   fun add (nil:cons list) = ()
	     | add ({cons=T.DATACON cons2, instance, ...}::q) = 
	       if #name cons = #name cons2 then
		   instance := ty :: (!instance)
	       else
		   add q
       in
	   add (!ens_cons)
       end; 



   (************************************************************************************************************)
   (***********************************functions to print environments******************************************)
   (************************************************************************************************************)
   local
       (*tranform a region in a string*)
       fun rtoS (int1, int2) = 
	   "("^Int.toString(int1)^","^Int.toString(int2)^")";

       fun stoS symbol = let val S.SYMBOL(_, str) = symbol in str end
			 
       fun ptoS nil  = ""
	 | ptoS [s] = stoS s
	 | ptoS (t::q) = stoS t^"."^ptoS q

       (*print a type with an environment*)
       fun printer0 ty env = 
	   (
	    (
	     (PP.with_default_pp (fn ppstrm => (PPType.resetPPType(); PPType.ppType env ppstrm ty)))
	     handle _ => print "fail to print anything"
	    )
	   )
	   
       (*print a type with the environment of the structure*)
       fun printer ty = printer0 ty (!stat_env)

       (*print the usage and instance of the environments*)
       fun print_instance usage instance = (
	   print " is used at :";
	   List.app (fn (x, y) => (print ("\n\t"^(rtoS x)^" with type "); printer y) ) (ListPair.zipEq (!usage,!instance));
	   print "\n"
       )

       fun print_ens2 {var = VC.VALvar {access=access, typ, path, ...}, def, usage, instance} = (
	   print ( (A.prAcc access)^": \""^ (SymPath.toString path)^"\" "^(rtoS def)^" has type ");
	   printer (!typ);
	   print " and";
	   print_instance usage instance
       )
	 | print_ens2 _ = print "Problem in print_ens2\n"
			  
   in			
       (*print the whole environment*)
       fun print_ens () = 
	   List.app print_ens2 (!ens_var)

       (*print the different type and datatype definitions and explicit uses*)
       fun print_types () = 
	   let
	       fun p (x:ty) =
		   case #tycon x of 
		       (T.DEFtyc {tyfun = T.TYFUN {arity, body}, path=InvPath.IPATH path, ...}) => 
		       (
			print ((ptoS (List.rev path))^" (arity "^(Int.toString arity)^") "^ rtoS (#def x) ^" : ");
			printer (body);
			print_instance (#usage x) (#instance x)
		       )
		     | (T.GENtyc {kind = T.DATATYPE {index, family, ...}, ...}) =>
		       let 
			   fun temp (y:T.dtmember) = 
			       List.app (fn (x:T.dconDesc) => (
					    print (stoS (#name x));
					    case #domain x of
						NONE => ()
					      | SOME ty => (print " of "; printer ty);
					    print ", "
					    )
					) 
					(#dcons y) 
			   val sub = Vector.sub (#members family,index)
		       in
			   print (stoS (#tycname sub)^" (arity "^ Int.toString (#arity sub) ^") "^ rtoS (#def x) ^" : ");
			   temp sub;
			   print_instance (#usage x) (#instance x)
		       end
		     | _ => ();
	   in
	       List.app p (!ens_types)
	   end;

       (*print the different type constructors and uses*)
       fun print_cons () = 
	   let 
	       fun pr (x:cons) = 
		   let val T.DATACON {typ, name = S.SYMBOL (_, str), ...} = #cons x in
		       print (str ^ " " ^ rtoS (#def x) ^ " has type ");
		       printer typ;
		       print " and";
		       print_instance (#usage x) (#instance x)
		   end
	   in
	       List.app pr (!ens_cons)  
	   end;
	   
       fun print_str () = 
	   let 
	       fun prr (a, b) = 
		   "(" ^ Int.toString a ^ "," ^ A.prAcc b ^") "

	       fun pr (x as {str = M.STR {access, ...}, ...} : str) = (
		   print ("str ("^A.prAcc access^") : ");
		   List.app (print o prr) (!(#map x));
		   print "\n"
	       )
	   in
	       List.app pr (!ens_str)
	   end

   end;


   (*************************************************************************************************************)
   (***************************************** others ************************************************************)
   (*************************************************************************************************************)

   (*up to date local environment, for display purpose only (env is used only in the printer function)*)
   fun maj e = stat_env := e;

   (*initialization of the environment*)
   fun clear () = ( ens_var := [];
		    ens_types := [];
		    ens_cons := [];
		    ens_str := [];
		    temp_map := []
		  )

end (*local*)

end (*structure Ens_var*)
