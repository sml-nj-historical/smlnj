signature ENS_PRINT = 
sig
   type file
   type location
   type var_elem
   type type_elem
   type cons_elem
   type str_elem
   type sig_elem
   type all

   val maj : StaticEnv.staticEnv -> unit

   val rtoS : location -> string
   val stoS : Symbol.symbol -> string
   val ptoS : Symbol.symbol list -> string
   val rptoS : InvPath.path -> string

   val print_var : var_elem -> unit
   val print_type : type_elem -> unit
   val print_cons : cons_elem -> unit
   val print_str : str_elem -> unit
   val print_sig : sig_elem -> unit
   val print_all : all -> unit

end (* signature ENS_PRINT*)

structure Ens_print : ENS_PRINT = 
struct

local 
    structure A = Access
    structure S = Symbol
    structure T = Types
    structure PP = PrettyPrintNew
    structure VC = VarCon
    structure M = Modules
in 

   fun bug msg = ErrorMsg.impossible("Bugs in Ens_print: "^msg);

   val stat_env = ref (StaticEnv.empty);
   fun maj e = stat_env := e;

   type file = string
   type location = file * int * int
   type var_elem = {var : VC.var, def : location, usage : (location * T.ty) list ref}
   type type_elem = {tycon : T.tycon, def : location, usage : (location * T.ty) list ref}
   type cons_elem = {cons : T.datacon, def : location, usage : (location * T.ty) list ref}
   type str_elem = {str : M.Structure, def : location, usage : (location * T.ty) list ref, 
		    map : (int * A.access) list ref}
   type sig_elem = {sign : M.Signature, def : location, alias : (location * S.symbol) list ref, 
		    usage : (location * S.symbol) list ref}
   type all = var_elem list * type_elem list * cons_elem list *  str_elem list * sig_elem list


   (*tranform a region in a string*)
   fun rtoS (filename, int1, int2) = 
       "(" ^ filename ^ "," ^ Int.toString int1 ^ "," ^ Int.toString int2 ^ ")";
       
   (*tranform symbol to string*)
   fun stoS symbol = let val S.SYMBOL(_, str) = symbol in str end
		     
   (*transform list of symbol to string*)
   fun ptoS nil  = ""
     | ptoS [s] = stoS s
     | ptoS (t::q) = stoS t ^ "." ^ ptoS q

   (* rpath to string *)
   fun rptoS (InvPath.IPATH p) = 
       ptoS (rev p)




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
   fun print_instance usage = (
       print " is used at :";
       List.app (fn (x, y) => (print ("\n\t" ^ rtoS x ^ " with type "); printer y)) (!usage);
       print "\n"
   )
			      
   fun print_var {var, def, usage} = (
       case var of 
	   VC.VALvar {access, typ, path, ...} => (
	   print (A.prAcc access ^ ": \"" ^ SymPath.toString path ^ "\" " ^ rtoS def ^ " has type ");
	   printer (!typ)
	   )
	 | VC.ERRORvar => print ("ERRORvar : " ^ rtoS def)
	 | VC.OVLDvar _ => print ("OVLDvar : " ^ rtoS def);
       print " and";
       print_instance usage
   )
       
   (*print the different type and datatype definitions and explicit uses*)
   fun print_type ({tycon, def, usage}:type_elem) =
       case tycon of 
	   (T.DEFtyc {tyfun = T.TYFUN {arity, body}, path, ...}) => 
	   (
	    print (rptoS path ^ " (arity " ^ Int.toString arity ^") "^ rtoS def ^" : ");
	    printer (body);
	    print_instance usage
	   )
	 | (T.GENtyc {kind = T.DATATYPE {index, family, ...}, ...}) =>
	   let 
	       fun temp ({dcons, ...}:T.dtmember) = 
		   List.app (fn ({name, domain, ...}:T.dconDesc) => (
				print (stoS name);
				case domain of
				    NONE => ()
				  | SOME ty => (print " of "; printer ty);
				print ", "
				)
			    ) 
			    dcons
	       val (sub as {tycname, arity, ...}) = Vector.sub (#members family,index)
	   in
	       print (stoS tycname ^ " (arity "^ Int.toString arity ^") "^ rtoS def ^ " : ");
	       temp sub;
	       print_instance usage
	   end
	 | _ => (
	   print ("other type : " ^ rtoS def);
	   print_instance usage
	   )
       
   (*print the different type constructors and uses*)
   fun print_cons ({cons, def, usage}:cons_elem) = 
       let 
	   val (dc as T.DATACON {typ, name = S.SYMBOL (_, str), ...}) = cons
       in
	   print (str ^ " " ^ rtoS def ^ " has type ");
	   printer typ;
	   print " and";
	   print_instance usage
       end

   local       
       fun prr (a, b) = 
	   "(" ^ Int.toString a ^ "," ^ A.prAcc b ^") "
	   
       fun print_map map = 
	   List.app (fn x => print ("\n\t"^prr x)) map;
	   
   in
       fun print_str ({str, def, usage, map} : str_elem) =
	   (
	    case str of 
		M.STR {access, rlzn = {rpath, ...}, ...} =>
		print ("("^A.prAcc access^") " ^ rptoS rpath ^ " " ^ rtoS def ^ " has slots")
	      | M.ERRORstr => print ("ERRORstr" ^ rtoS def ^ " has slots")
	      | M.STRSIG _ => print ("STRSIG" ^ rtoS def ^ " has slots");
	    print_map (!map);
	    print " and";
	    print_instance usage
	   )
   end

   fun print_sig {sign, def, usage, alias} =
       let
	   fun print_so name = 
	       case name of
		   NONE => print "<anonymous>"
		 | SOME name' => print (stoS name');
	       
	   fun print_inst usage = (
	       print " and is used at :";
	       List.app (fn (x, y) => (print ("\n\t"^(rtoS x)^" with name "); print (stoS y))) (!usage);
	       print "\n"
	   )
       in
	   case sign of 
	       M.SIG {name, ...} => print_so name
	     | _ => ();
	   print (" : "^rtoS def);
	   List.app (fn (x, S.SYMBOL (_, str)) => print ("\n\thas alias "^ str ^ " " ^ (rtoS x))) (!alias);
	   print_inst usage
       end

   fun print_all (a, b, c, d, e) = (
       List.app print_var a;
       List.app print_type b;
       List.app print_cons c;
       List.app print_str d;
       List.app print_sig e
   )
end
end (* structure Ens_print *)
