signature ENS_PRINT2 = 
sig
   val maj : StaticEnv.staticEnv -> unit

   val rtoS : Ens_types2.location -> string
   val stoS : Symbol.symbol -> string
   val ptoS : Symbol.symbol list -> string
   val rptoS : InvPath.path -> string

   val print_var : Ens_types2.var_elem -> unit
   val print_type : Ens_types2.type_elem -> unit
   val print_cons : Ens_types2.cons_elem -> unit
   val print_str : Ens_types2.str_elem -> unit
   val print_sig : Ens_types2.sig_elem -> unit
   val print_all : Ens_types2.all -> unit

end (* signature ENS_PRINT*)

structure Ens_print2 : ENS_PRINT2 = 
struct

local 
    structure A = Access
    structure S = Symbol
    structure T = Types
    structure PP = PrettyPrintNew
    structure VC = VarCon
    structure M = Modules
    open Ens_types2
in 

   fun bug msg = ErrorMsg.impossible("Bugs in Ens_print: "^msg);

   val stat_env = ref (StaticEnv.empty);
   fun maj e = stat_env := e;


   (*tranform a region in a string*)
   fun rtoS (filename, int1, int2) = 
       "(" ^ filename ^ "," ^ Int.toString int1 ^ ","^Int.toString int2 ^ ")";
       
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
	 (PP.with_default_pp 
	      (fn ppstrm => 
		  (PPType.resetPPType(); PPType.ppType env ppstrm ty)))
	 handle _ => print "fail to print anything"
	)
       )
       
   (*print a type with the environment of the structure*)
   fun printer ty = printer0 ty (!stat_env)
		    
   (*print the usage and instance of the environments*)
   fun print_instance usage = (
       print " is used at :";
       List.app 
	   (fn (x, y) => (print ("\n\t" ^ rtoS x ^ " with type "); printer y))
	   (!usage);
       print "\n"
   )
			      
   fun print_var ({access, name, parent, typ, def, usage}:var_elem) = (
       print (A.prAcc access ^ ": \"" ^ stoS name ^ 
	      "\" " ^ rtoS def ^ " has type ");
       printer typ;
       print (", is defined in " ^ A.prAcc parent ^ " and");
       print " is used at :";
       List.app 
	   ( fn (x, y, z) => 
		( print ("\n\t" ^ rtoS x ^ " with type "); 
		  printer y; 
		  print (", access " ^ A.prAcc z)
		)
	   )
	   (!usage);
       print "\n"
   )
       
   (*print the different type and datatype definitions and explicit uses*)
   fun print_type ({tycon, def, usage}:type_elem) =
       case tycon of 
	   (T.DEFtyc {tyfun = T.TYFUN {arity, body}, path, ...}) => 
	   (
	    print (rptoS path ^ " (arity " ^ Int.toString arity ^") "^ 
		   rtoS def ^" : ");
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
	       val (sub as {tycname, arity, ...}) = 
		   Vector.sub (#members family,index)
	   in
	       print (stoS tycname ^ " (arity "^ Int.toString arity ^ 
		      ") "^ rtoS def ^ " : ");
	       temp sub;
	       print_instance usage
	   end
	 | _ => (
	   print ("other type : " ^ rtoS def);
	   print_instance usage
	   )
       
   (*print the different type constructors and uses*)
   fun print_cons ({name, typ, gen_typ, def, usage}:cons_elem) = (
       print (stoS name ^ " " ^ rtoS def ^ " has type ");
       printer typ;
       print " and";
       print_instance usage
   )

   fun print_str ({name, access, parent, sign, def, elements, usage}:str_elem)=
	let 
	    fun print_key k = 
		case k of 
		    (Var a|Str a) => Access.prAcc a
		  | _ => "others"
	in
	    print ("(" ^ A.prAcc access ^ ") " ^ stoS name ^ 
		   " " ^ rtoS def ^ " defined in ");
	    case parent of 
		NONE => print "the toplevel"
	      | SOME parent' => print (A.prAcc parent');
	    case elements of
		Def el => (
		print " contains ";
		List.app ( fn (x, y, z)=> 
			      print ("\n\t(" ^ Int.toString x ^ "," ^
				     stoS y ^ "," ^ print_key z ^ ")") 
			 ) el
		)
	      | Constraint (el, a) => 
		( print (" constrains " ^ A.prAcc a ^ " : ");
		  List.app ( fn (x, y, z) => 
				print ("\n\t(" ^ Int.toString x ^ "," ^
				       stoS y ^ ","^Int.toString z ^ ")") 
			   ) el
		)
	      | Alias a => print (" aliases " ^ A.prAcc a);
	    print " and is used at : ";
	    List.app (fn x => print ("\n\t" ^ rtoS x)) (!usage);
	    print "\n"
	end

   fun print_sig ({name, stamp, def, parent, elements, usage, alias}:sig_elem)=
       let
	   fun print_inst usage = (
	       print " and is used at :";
	       List.app 
		   (fn (x, y) => print ("\n\t"^(rtoS x)^" with name "^stoS y))
		   (!usage);
	       print "\n"
	   )
       in
	   print (stoS name ^ " : " ^ rtoS def);
	   List.app 
	       (fn (x, S.SYMBOL (_, str)) => 
		   print ("\n\thas alias "^ str ^ " " ^ (rtoS x))) 
	       (!alias);
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
