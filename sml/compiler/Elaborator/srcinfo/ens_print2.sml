signature ENS_PRINT2 = 
sig
   val maj : StaticEnv.staticEnv -> unit

   val rtoS : Ens_types2.location -> string
   val stoS : Symbol.symbol -> string
   val ptoS : Symbol.symbol list -> string
   val rptoS : InvPath.path -> string

   val print_ty' : Ens_types2.ty' -> unit
   val print_tycon' : Ens_types2.tycon' -> unit
   val printer : Types.ty -> unit

   val print_var : Ens_types2.var_elem -> unit
   val print_type : Ens_types2.type_elem -> unit
   val print_cons : Ens_types2.cons_elem -> unit
   val print_str : Ens_types2.str_elem -> unit
   val print_sig : Ens_types2.sig_elem -> unit
   val print_ext : Ens_types2.ext_elem -> unit

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

   fun print_tycon' tyc =  
       case tyc of  
	   Datatype (b, sl) => 
	   ( print ("datatype " ^ (if b then "(eq)" else "") ^ " (");
	     print (String.concatWith " " (List.map Symbol.name sl));
	     print ")"
	   )
	 | Abstract sl => 
	   ( print ("abstract (");
	     print (String.concatWith " " (List.map Symbol.name sl));
	     print ")"
	   )
	 | Deftyc => print "deftyc" 
	 | Primtyc b => print ("primtyc "  ^ (if b then "(eq)" else ""))
			
   fun print_ty' ty = 
       case ty of
	   Conty (Record [], []) => print "unit"
	 | Conty (Record (ll as h::_), tyl) =>
	   if stoS h = "1" then
	       let fun p [] = ErrorMsg.impossible "Ens_var2: print_ty'.1"
		     | p [x] = print_ty' x
		     | p (x::y) = (print_ty' x; print " * "; p y)
	       in
		   p tyl
	       end
	   else
	       ( print "{";
		 List.app 
		     (fn (x, y) => 
			 (print (stoS x ^ ":"); print_ty' y; print ", "))
		     (ListPair.zipEq (ll, tyl)); 
		 print "}"
	       )
	 | Conty (General (_, path), []) => 
	   print (rptoS path)
	 | Conty (General (_, path), [t]) => 
	   ( print_ty' t; 
	     print " "; 
	     print (rptoS path)
	   )
	 | Conty (General (_, path), [t1, t2]) => 
	   ( print_ty' t1; 
	     print " "; 
	     print (rptoS path); 
	     print " "; 
	     print_ty' t2
	   )
	 | Conty _ => 
	   ErrorMsg.impossible "Ens_var2: print_ty'.2"
	 | Ibound index =>
	   print ("'" ^ str (Char.chr (Char.ord #"a" + index)))
	 | Lbound {depth, index} => 
	   print ("'" ^ str (Char.chr (Char.ord #"A" + index)) ^ 
		  Int.toString depth)
	 | Ubound s => print (stoS s)
	 | Poly {body, arity} => print_ty' body
	   
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
	
   fun print_var_usage usage = 
       ( print " is used at :";
	 List.app 
	     ( fn (x, y, z) => 
		  ( print ("\n\t" ^ rtoS x ^ " with type "); 
		    print_ty' y; 
		    print (", access " ^ A.prAcc z)
		  )
	     )
	     (!usage);
	 print "\n"
       )
	      
   fun print_var ({access, name, parent, typ, def, usage}:var_elem) = (
       print (A.prAcc access ^ ": \"" ^ stoS name ^ 
	      "\" " ^ rtoS def ^ " has type ");
       print_ty' typ;
       print (", is defined in " ^ A.prAcc parent ^ " and");
       print_var_usage usage
   )
    
   fun print_type_usage usage = 
       ( print " is used at: ";
	 List.app 
	     (fn x => print ("\n\t" ^ rtoS x))
	     (!usage);
	 print "\n"
       )

   (*print the different type and datatype definitions and explicit uses*)
   fun print_type ({tycon, stamp, name, def, usage} : type_elem) = 
       ( print (Stamps.toString stamp ^ " ");
	 print_tycon' tycon;
	 print " ";
	 print (stoS name);
	 print " ";
	 print (rtoS def);
	 print_type_usage usage
       )
       
   fun print_cons_usage usage = 
       ( List.app 
	     (fn (x, y) => 
		 (print ("\n\t" ^ rtoS x ^ " with type "); print_ty' y))
	     (!usage);
	 print "\n"
       )

   (*print the different type constructors and uses*)
   fun print_cons ({name, ty, dataty, def, usage} : cons_elem) = (
       print (Stamps.toString dataty ^ " ");
       print (stoS name);
       print " ";
       print_ty' ty;
       print " ";
       print (rtoS def);
       print_cons_usage usage
   )
       
   fun print_str_usage usage = 
       ( print " and is used at : ";
	 List.app (fn x => print ("\n\t" ^ rtoS x)) (!usage);
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
	    print_str_usage usage
	end

   fun print_sig_usage usage = 
       ( print " and is used at :";
	 List.app 
	     (fn (x, y) => print ("\n\t"^(rtoS x)^" with name "^stoS y))
	     (!usage);
	 print "\n"
       )

   fun print_sig ({name,stamp,inferred,def,elements,usage,alias} : sig_elem) =
       let
	   fun print_elem l pref = 
	       let fun print_symbol_spec (s, spec) = 
		       let fun print_spec (Typ tycon') = 
			       (print "typ:"; print_tycon' tycon')
			     | print_spec (Val ty') = 
			       (print "val:"; print_ty' ty')
			     | print_spec (Exception ty') = 
			       (print "exn:"; print_ty' ty')
			     | print_spec (NamedStr (symb, stamp)) = 
			       print ("named:"^Symbol.name symb)
			     | print_spec (InlineStr l) = 
			       print_elem l (pref ^ "   ")
		       in print (Symbol.name s ^ " : "); 
			  print_spec spec 
		       end
	       in
		   List.app 
		       (fn x => ( print ("\n" ^ pref ^ " ");
				  print_symbol_spec x)
		       ) 
		       l
	       end
       in
	   print (Stamps.toString stamp ^ " " ^ stoS name ^ 
		  (if inferred then " (inferred)" else "") 
		  ^ " : " ^ rtoS def);
	   print_elem elements "   ";
	   List.app 
	       (fn (x, symb) => 
		   print ("\n\thas alias "^ stoS symb ^ " " ^ (rtoS x))) 
	       (!alias);
	   print_sig_usage usage
       end

   fun print_ext ext = 
       case ext of
	   ExtVar {access, usage} =>
	   ( print ("ExtVar (" ^ A.prAcc access ^ ")");
	     print_var_usage usage
	   )
	 | ExtStr {access, usage} =>
	   ( print ("ExtStr (" ^ A.prAcc access ^ ")");
	     print_str_usage usage
	   )
	 | ExtType {stamp, usage} =>
	   ( print ("ExtType " ^ Stamps.toString stamp);
	     print_type_usage usage
	   )
	 | ExtCons {stamp, usage, name} =>
	   ( print ("ExtCons " ^ Symbol.name name ^ " " ^ 
		    Stamps.toString stamp);
	     print_cons_usage usage
	   )
	 | ExtSig {stamp, usage} =>
	   ( print ("ExtSig " ^ Stamps.toString stamp);
	     print_sig_usage usage
	   )
       

end
end (* structure Ens_print *)
