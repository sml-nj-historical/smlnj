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
   type 'a env = 'a list ref;

   type var = {var : VC.var, def : int * int, usage : (int * int) list ref, instance : T.ty list ref}
   type env_var = var env;
   type ty = {tycon : T.tycon, def : int * int, usage : (int * int) list ref, instance : T.ty list ref};
   type env_ty = ty env;
   type cons = {cons : T.datacon, def: int * int, usage : (int * int) list ref, instance : T.ty list ref};
   type env_cons = cons env;

   (*the environment iself*)
   val ens_var = (ref []):env_var;       (*contains def and use of variables, and functions*)
   val ens_types = (ref []):env_ty;      (*contains type and datatype def and explicite use*)
   val ens_cons = (ref []):env_cons;     (*contains type constructors use*)

   val stat_env = ref (StaticEnv.empty);     (*used only for printing without `?.' everywhere*)

   fun change_access old_access new_access = 
       let
	   fun incl (access as A.PATH (access2, _)) = access = old_access orelse incl access2
	     | incl access = access = old_access
			     
	   fun incl2 (access as A.PATH (access2, slot)) = if access = old_access then 
							      new_access 
							  else 
							      A.PATH (incl2 access2, slot)
	     | incl2 access = new_access (*verifier que ca fait bien ce qu'il faut ici*)

	   fun pred_1 {var = (VC.VALvar {access, ...}), ...} = incl access
	     | pred_1 (_:var) = false
	   val (to_be_changed, no_change) = List.partition pred_1 (!ens_var)
	   fun modi {var = VC.VALvar {access, typ, prim, path}, def, usage, instance} =
	       {var = VC.VALvar {access = incl2 access, typ = typ, prim = prim, path = path}, 
		def = def, 
		usage = usage,  
		instance = instance 
	       }
	   val changed = List.map modi to_be_changed
       in
	   ens_var := changed @ no_change
       end
	   
   (************************************************************************************************************)
   (*************************functions about variable bindings and uses in expressions**************************)
   (************************************************************************************************************)

   (*modifications of the environment when variables are defined, or used in expressions*)
   fun add_var_def var region =
       ens_var := {var=var, def=region, usage=ref [], instance = ref []} :: (!ens_var);
       
   fun add_var_use access region = 
       let
	   fun add_var (nil:var list) access region = ()
	     | add_var ({var = VC.VALvar{access = access2, ...}, usage=l, ...}::q) access region = 
                   if access=access2 then
		       l := region :: (!l)
		   else
		       add_var q access region
	     | add_var (t::q) access region = (print "Problem in Ens_var.add_var_use\n"; add_var q access region)
       in
	   add_var (!ens_var) access region
       end;

   (*add an instantiated type to the list of types of the variable with the given access*)
   fun add_var_inst (x:T.ty * A.access) = 
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
       end;

   (*keep in the environment ens_var only the variable that are accessible, ie, those who access contains an EXTERN*)
   fun clear_intern () = (
       let
	   fun is_extern (A.EXTERN _)  = true
	     | is_extern (A.PATH (a, _)) = is_extern a
	     | is_extern _ = false

	   fun add l = 
	       let 
		   fun filt {var = VC.VALvar {access, ...}, ...} = is_extern access 
		     | filt (_:var) = false
	       in
		   ens_var := (List.filter filt l)
	       end
       in
	   add (!ens_var)
       end
   );

   (*initialization of the environment*)
   fun clear () = ens_var := [];
     
   (*delete all the "it" that are declared in the interactive toplevel*)
   fun clear_it () =
       let fun filt {var = VC.VALvar {path, ...}, ...} = SymPath.toString path <> "it" 
	     | filt (_:var) = true
       in
	   ens_var := List.filter filt (!ens_var)
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
	   fun find (nil:ty list) = (print "Ens_var : constructor undefined\n"; (~1, ~1))
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
   end;


   (*************************************************************************************************************)
   (***************************************** others ************************************************************)
   (*************************************************************************************************************)

   (*up to date local environment, for display purpose only (env is used only in the printer function)*)
   fun maj e = stat_env := e;

   (*datatype link = Slot of int | Access of A.access;
   datatype graph = Graph of (link * graph) list; (*for the moment, all the nodes are empty*)

   val graph = ref (Graph []);
   val current_state = graph;

   fun add_access access = 
       let 
	   fun add_slot slot (Graph l) = 
	       Graph ((Slot slot, Graph []) :: l)

	   fun add_s (A.PATH (a, slot)) graph = 
		      add_slot slot (add_s a graph)
	     | add_s a (Graph l) = 
	       Graph ((Access a, Graph []) :: l)
       in
	   current_state := add_s access (!current_state)
       end

   fun add_link new_access old_access =
       let 
	   fun find (A.PATH (a, slot)) (graph:graph) = (
	       case find a graph of
		   NONE => (NONE:graph option)
		 | SOME (_, graph2) =>
		   case List.find (fn x => Slot slot = #1 x) graph2 of
		       NONE => (NONE:graph option)
		     | SOME (_, graph3) => SOME graph3
	   )
	     | find access graph = 
	       case List.find (fn x => Access access = #1 x) graph of
		   NONE => (NONE:graph option)
		 | SOME (_, graph2) => SOME graph2

       in
	   case find old_access (!current_state) of
	       NONE => print (A.prAcc old_access ^ " not available\n")
	     | SOME g => 
	       let val ref (Graph l) = current_state in
		   current_state := (Access new_access, g) :: l
	       end
       end*)
	   

end (*local*)

end (*structure Ens_var*)
