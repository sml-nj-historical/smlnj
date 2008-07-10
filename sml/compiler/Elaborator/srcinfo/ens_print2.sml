signature ENS_PRINT2 = 
sig
   val maj : StaticEnv.staticEnv -> unit

   val rtoS : Ens_types2.location -> string
   val stoS : Symbol.symbol -> string
   val ptoS : Symbol.symbol list -> string
   val rptoS : InvPath.path -> string

   val print_ty'  : Ens_types2.ty' -> string
   val print_ty'' : Ens_types2.ty' -> unit
   val printer : Types.ty -> unit
   val scanty' : string -> Ens_types2.ty'

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

   fun bug msg = ErrorMsg.impossible("Bugs in Ens_print2: "^msg);

   val stat_env = ref (StaticEnv.empty);
   fun maj e = stat_env := e;


   (*tranform a region in a string*)
   fun rtoS (filename, int1, int2) = 
       "(" ^ filename ^ "," ^ Int.toString int1 ^ ","^Int.toString int2 ^ ")";
       
   (*tranform symbol to string*)
   fun stoS symbol = S.name symbol
		     
   (*transform list of symbol to string*)
   fun ptoS nil  = ""
     | ptoS [s] = stoS s
     | ptoS (t::q) = stoS t ^ "." ^ ptoS q

   (* rpath to string *)
   fun rptoS (InvPath.IPATH p) = 
       ptoS (rev p)




   (*fun print_ty (ty:T.ty) = 
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
	print ("_" ^ stoS (TypesUtil.tycName tyc))
    )*)
   fun pr_list pr l sep = 
       let
	   fun pr_list' [] = ""
	     | pr_list' [h] = pr h
	     | pr_list' (h::q) = (pr h ^ sep ^ pr_list' q)
       in
	   pr_list' l
       end

   fun pr_path (InvPath.IPATH path) = 
       pr_list Symbol.symbolToString (rev path) "."

   fun print_ty' ty = 
       case ty of
	   Conty (General (stamp, path), tyl) => 
	   let fun conv s = 
		   Stamps.Case
		       (Stamps.newConverter ())
		       s 
		       { fresh = fn x => "fresh " ^ Int.toString x,
			 global = fn {pid, cnt} => 
				     "global " ^ PersStamps.toHex pid ^
				     " " ^ Int.toString cnt,
			 special = fn x => "special " ^ x 
		       }
	   in
	       "Conty General " ^ conv stamp ^ " " ^ pr_path path(*rptoS path*) ^ " ( " ^ 
	       pr_list print_ty' tyl " " ^ " )"
	   end
	 | Conty (Record ll, tyl) => 
	   "Conty Record ( " ^ pr_list Symbol.symbolToString ll " " ^ " ) ( " ^ pr_list print_ty' tyl " " ^ " )"
	 | Ibound i => "Ibound " ^ Int.toString i

   fun get_int s = 
       Option.valOf (Int.fromString s)

   fun scan_stamp sl = 
       case sl of 
	   "fresh" :: x :: sl' => 
	   (Stamps.fresh' (get_int x), sl')
	 | "global" :: x :: y :: sl' =>
	   (Stamps.global {pid = Option.valOf (PersStamps.fromHex x), 
			   cnt = get_int y}, sl')
	 | "special" :: x :: sl' =>
	   (Stamps.special x, sl')
	 | h :: _ => bug ("scan_stamp: " ^ h)
	 | [] => bug "scan_stamp: []"
     
   fun get_symbol s = 
       case String.tokens (fn c => c = #"$") s of
	  ["VAL",  name] => S.varSymbol  name
	| ["SIG",  name] => S.sigSymbol  name
	| ["STR",  name] => S.strSymbol  name
	| ["FSIG", name] => S.fsigSymbol name
	| ["FCT",  name] => S.fctSymbol  name
	| ["TYC",  name] => S.tycSymbol  name
	| ["LAB",  name] => S.labSymbol  name
	| ["TYV",  name] => S.tyvSymbol  name
	| ["FIX",  name] => S.fixSymbol  name
	| _ => bug ("get_symbol: " ^ s)

   fun scan_symbol (sl:string list) = 
       case sl of
	   h :: sl' => (get_symbol h, sl')
	 | [] => bug "scan_symbol: []"

   fun scan_path sl = 
       case sl of
	   h :: sl' => 
	   let val sl2 = String.tokens (fn c => c = #".") h
	       val path : InvPath.path= List.foldl 
			      (fn (y,x) => InvPath.extend (x, get_symbol y)) 
			      InvPath.empty
			      sl2
	   in
	       (path, sl')
	   end
	 | [] => bug "scan_path: []"

   fun scan_list sc sl = 
       case sl of
	   "(" :: sl' => 
	   let
	       fun scan (")"::sl') = ([], sl')
		 | scan sl' = 
		   let
		       val (h', sl'') = sc sl'
		       val (q', sl''') = scan sl''
		   in
		       (h' :: q', sl''')
		   end
	   in
	       scan sl' 
	   end
	 | h :: _ => bug ("scan_list: " ^ h)
	 | [] => bug "scan_list: []"

   fun scan_ty' sl : (ty' * string list) =
       case sl of
	   "Conty" :: "General" :: sl' => 
	   let 
	       val (s, sl'') = scan_stamp sl'; 
	       val (p, sl''') = scan_path sl''; 
	       val (tyl : ty' list, sl'''') = scan_list scan_ty' sl'''
	   in
	       (Conty (General (s, p), tyl), sl''')
	   end
	 | "Conty" :: "Record" :: sl' => 
	   let 
	       val (ll, sl'') = scan_list scan_symbol sl'
	       val (tyl, sl''') = scan_list scan_ty' sl''
	   in
	       (Conty (Record ll, tyl), sl''')
	   end
	 | "Ibound" :: x :: sl' => 
	   (Ibound (get_int x), sl')
	 | sl => bug ("scan_ty': " ^ String.concatWith " " sl)
   
   fun scanty' s = 
       #1 (scan_ty' (String.tokens (fn c => #" " = c) s))

   fun print_tyc' tyc =  
       case tyc of  
	   Datatype (b, sl) => 
	   ( print ("datatype " ^ Bool.toString b ^ " ( ");
	     print (pr_list Symbol.symbolToString sl " ");
	     print " )"
	   )
	 | Abstract sl => 
	   ( print ("abstract ( ");
	     print (pr_list Symbol.symbolToString sl " ");
	     print " )"
	   )
	 | Deftyc => print "deftyc" 
	 | Primtyc b => print ("primtyc "  ^ Bool.toString b)
			
   fun print_ty'' ty = 
       case ty of
	   Conty (Record [], []) => print "unit"
	 | Conty (Record (ll as h::_), tyl) =>
	   if stoS h = "1" then
	       let fun p [] = ErrorMsg.impossible "Ens_var2: print_ty''.1"
		     | p [x] = print_ty'' x
		     | p (x::y) = (print_ty'' x; print " * "; p y)
	       in
		   p tyl
	       end
	   else
	       ( print "{";
		 List.app 
		     (fn (x, y) => 
			 (print (stoS x ^ ":"); print_ty'' y; print ", "))
		     (ListPair.zip (ll, tyl)); 
		 print "}"
	       )
	 | Conty (General (_, path), []) => 
	   print (rptoS path)
	 | Conty (General (_, path), [t]) => 
	   ( print_ty'' t; 
	     print " "; 
	     print (rptoS path)
	   )
	 | Conty (General (_, path), [t1, t2]) => 
	   ( print_ty'' t1; 
	     print " "; 
	     print (rptoS path); 
	     print " "; 
	     print_ty'' t2
	   )
	 | Conty _ => 
	   ErrorMsg.impossible "Ens_var2: print_ty''.2"
	 | Ibound i => 
	   print ("'" ^ str (Char.chr (Char.ord #"a" + i)))
	   
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
       print_ty'' typ;
       print (", is defined in " ^ A.prAcc parent ^ " and");
       print " is used at :";
       List.app 
	   ( fn (x, y, z) => 
		( print ("\n\t" ^ rtoS x ^ " with type "); 
		  print_ty'' y; 
		  print (", access " ^ A.prAcc z)
		)
	   )
	   (!usage);
       print "\n"
   )
       
   (*print the different type and datatype definitions and explicit uses*)
   fun print_type ({tycon, stamp, name, def, usage} : type_elem) = 
       ( print_tyc' tycon;
	 print " ";
	 print (stoS name);
	 print " ";
	 print (rtoS def);
	 print " is used at: ";
	 List.app 
	     (fn x => print ("\n\t" ^ rtoS x))
	     (!usage);
	 print "\n"
       )
       
   (*print the different type constructors and uses*)
   fun print_cons ({name, ty, dataty, def, usage} : cons_elem) = (
       print (stoS name);
       print " ";
       print_ty'' ty;
       print " ";
       print (rtoS def);
       List.app 
	   (fn (x, y)=>(print ("\n\t" ^ rtoS x ^ " with type "); print_ty'' y))
	   (!usage);
       print "\n"
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
	       (fn (x, symb) => 
		   print ("\n\thas alias "^ stoS symb ^ " " ^ (rtoS x))) 
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
