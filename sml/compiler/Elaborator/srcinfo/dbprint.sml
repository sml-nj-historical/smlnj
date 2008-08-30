structure DBPrint : DBPRINT =
struct

local
    structure A = Access
    open DBTypes
in

   fun bug msg = ErrorMsg.impossible("DBPrint: "^msg);

   (*tranform a region in a string*)
   fun rtoS (filename, int1, int2) =
       "(" ^ filename ^ "," ^ Int.toString int1 ^ ","^Int.toString int2 ^ ")";

   (*tranform symbol to string*)
   fun stoS symbol = Symbol.name symbol

   (*transform list of symbol to string*)
   fun ptoS nil  = ""
     | ptoS [s] = stoS s
     | ptoS (t::q) = stoS t ^ "." ^ ptoS q

   (* rpath to string *)
   fun rptoS (InvPath.IPATH p) =
       ptoS (rev p)


   (* print an occurrence *)
   fun print_occ (symbol, location) =
       print (Symbol.symbolToString symbol ^ rtoS location ^ " ")

   (* print a tycon' *)
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

   (* print a ty' *)
   fun print_ty' ty =
       case ty of
	   Conty (Record [], []) => print "unit"
	 | Conty (Record (ll as h::_), tyl) =>
	   if stoS h = "1" then
	       let fun p [] = bug "print_ty'.1"
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
	   bug "print_ty'.2"
	 | Ibound index =>
	   print ("'" ^ str (Char.chr (Char.ord #"a" + index)))
	 | Lbound {depth, index} =>
	   print ("'" ^ str (Char.chr (Char.ord #"A" + index)) ^
		  Int.toString depth)
	 | Ubound s => print (stoS s)
	 | Poly {body, arity} => print_ty' body

   (* print a varUse list ref *)
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

   (* print a var_elem *)
   fun print_var ({access, name, parent, typ, def, usage}:var_elem) = (
       print (A.prAcc access ^ ": \"" ^ stoS name ^
	      "\" " ^ rtoS def ^ " has type ");
       print_ty' typ;
       print (", is defined in " ^ A.prAcc parent ^ " and");
       print_var_usage usage
   )

   (* print a typeUse list ref *)
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

   (* print a consUse list ref *)
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

   (* print a strUse list ref *)
   fun print_str_usage usage =
       ( print " and is used at : ";
	 List.app (fn x => print ("\n\t" ^ rtoS x)) (!usage);
	 print "\n"
       )

   (* print a key *)
   fun print_key k =
       case k of
	   Var a => "Var " ^ Access.prAcc a
	 | Str a => "Str " ^ Access.prAcc a
	 | _ => "others"

   (* print a str_elem *)
   fun print_str ({name, access, parent, sign, def, elements, usage}:str_elem)=
       ( print ("(" ^ A.prAcc access ^ ") " ^ stoS name ^
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
	     ( print (" is a constraint of " ^ A.prAcc a ^ " : ");
		  List.app ( fn (x, y, z) =>
				print ("\n\t(" ^ Int.toString x ^ "," ^
				       stoS y ^ ","^Int.toString z ^ ")")
			   ) el
	     )
	   | Alias a => print (" aliases " ^ A.prAcc a);
	 print_str_usage usage
       )

   (* print a sigUse list ref*)
   fun print_sig_usage usage =
       ( print " and is used at :";
	 List.app
	     (fn x => print ("\n\t"^(rtoS x)))
	     (!usage);
	 print "\n"
       )

   (* print a sig_elem *)
   fun print_sig ({name,stamp,inferred,def,elements,usage} : sig_elem) =
       let
	   fun print_defsig l pref =
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
			       print_defsig l (pref ^ "   ")
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

	   fun print_elem (DefSig l) pref =
	       print_defsig l pref
	     | print_elem (AliasSig st) pref =
	       print ("\n" ^ pref ^ " is an alias of " ^ Stamps.toString st);
       in
	   print (Stamps.toString stamp ^ " " ^ stoS name ^
		  (if inferred then " (inferred)" else "")
		  ^ " : " ^ rtoS def);
	   print_elem elements "   ";
	   print_sig_usage usage
       end

   (* print a ext_elem *)
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
end (* structure DBPrint *)
