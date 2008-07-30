(* query.sml *)

(* Queries

query structure
   - how to represent queries as a data structure
   - these query structures will be interpreted with respect to the database to generate "answers"
   - potentially queries for a language, with a set of primitive queries and possibly ways of 
      combining queries into compound queries (e.g. boolean combinations -- conjunctions and disjunctions)
   - query refinement: adding criteria to narrow the possible answers to a query
   - answers will be derived from (sets of) database elements (records, or table entries)

   - Alternately (to start with) provide a menu of predefined queries covering the common
     cases.

example queries:

   describe variable named "x"   (context?, globally or current file/module)
   describe the tycon "t"
   describe the data constructor "C"
   describe the signature "S"
   describe the structure "A"
   find the unique location of the definition of occurrence "x"
   list the locations of the definitions of symbol "x"
   list the locations where variable "x" is used (all applied occurrences of "x")
   list the locations where variable "x" is used (all applied occurrences of "x") and their occurrence types
   what module is "x" defined in?
   what module is "x" used in (imported into)?
   what source file is "x" defined in?
   is x local or exported from its parent module?
   what is the type of "x" at location "L"?
   what is the type of "x" as an element of structure "A"
   what structures import structure "A"?
   what structure contains location "L"?
   what function is variable x defined in (assuming it is a local variable)
   find function definitions containing variable "x"
   what is the nested hierarchy of functions containing this occurrence/location
      (sequence of nested named function definitions)
   list all the symbols defined in this compilation unit
   list symbols are exported from this structure
   list the functions defined in a file (or module)
      primary (i.e. at "top-level" within the file/module
      seconday (i.e. nested within higher-level functions)
   explore the hierarchial tree of function definitions (e.g. parent function)
   what is the symbol (or token?) "at" character position n in a file
     "at" == first symbol (in text order) whose terminal position exceeds n

   Suggested by Lars:
   Related to browsing and nagivation features
     - All structures in this CM package(?)  (CM library or group?)
     - Members of a given structure
     - For this structure or member, what file and line is it defined at

   Related to code editing
     - What is the type of the token at this location in the file (for tooltips on mouseover,
         goto-definition, and function argument hints)
     - Given a type, what are its members (supports completion lists after ".")
     - List of the members and types "in scope" at a location in the file
         (supports completion on naked symbols)

Characteristics of a query : 
    The "abstract" subject (e.g. an "abstract" bound variable (over values, tycons, structures, etc.))
     basic designation + qualifiers
     1. basic designation thing of interest (subject).  E.g. a symbol (name and namespace)
     2. qualifications (narrowing scope of possible subjects). E.g. defining occurrence

    Attributes of the subject that we want to know:
        (e.g. type, defn location, use locations, relations with modules, functions, ...)

     basic designation is typically a symbol, or a location
     qualifications?
	name space (if we don't view that as part of the basic designation)

    An "occurrence" of a symbol uniquely determines an abstract variable (value, tycon, str, etc.).
    An "occurrence" is a location of a single symbol.  It can be an "applied occurrence",
      i.e. a use of the variable or structure name, etc., or a "defining occurrence" or
      "binding occurrence", i.e. the occurrence where the variable is introduced or bound.

If we want to make a query with respect to a plain, nonlocated, symbol, we may either have
multiple answers (since the symbol may be bound multiple times), or we can provide some
context to disambiguate which binding of that symbol is intended.  The context could be
a file name (designating the compilation unit), or possibly a CM library.

*)

structure Queries =
struct

structure S = Symbol

open DBTypes Database

(* type occurrence = S.symbol * location -- now defined in DBTypes *)
(*
type subject = OCC of occurrence | SYM of symbol | ???

(* a crude first attempt *)
datatype query = 
  Q of subjet * qualifier list * attributes
*)

(* 
needed basic utility functions: 

   eqLocation : location * location -> bool

needed database functions:

   findVar : (var_elem -> bool) -> var_elem option
     -- finds first or unique element of ens_var (variable table) satisfying predicate
*)
fun bug s = raise Fail s
fun error () = raise Fail "error"

(* query utility functions *)

(* test whether an occurrence is a use of a variable given by a var_elem 
 * -- this assumes that the occurrence argument is an applied occurrence *)
fun occIsVar (occ as (sym,loc): occurrence) ({name,usage,...} : var_elem) : bool =
    S.eq (sym,name) andalso List.exists (fn (loc',_,_) => eqLocation(loc', loc)) (!usage)

fun findUse(loc: location, usage: varUse list) : varUse option =
    List.find (fn (loc',ty,acc) => eqLocation(loc,loc')) usage


fun toVarOcc (s, loc) = 
    ((Symbol.varSymbol s), loc)

(* sample query functions *)

(* find defining occurrence for a (variable?) x occurrence *)
fun varDefOcc (occ : occurrence) : location option = 
    case find_var (occIsVar occ) 
     of SOME{def,...} => SOME def
      | NONE => NONE

fun varRecordOcc (occ : occurrence) = 
    find_var (occIsVar occ) 

val varDefOcc' = varDefOcc o toVarOcc
val varDefOcc'' = Option.map #def o varRecordOcc o toVarOcc

(* find type of a (variable?) applied occurrence *)
fun varTypOcc (occ as (sym,loc): occurrence) : ty' option = 
    case find_var (occIsVar occ)
     of SOME{def,usage, ...} =>
	( case findUse(loc,!usage) 
	   of SOME (_,ty,_) => SOME ty
	    | NONE => NONE
	)
      | NONE => NONE

val varTypOcc' = varTypOcc o toVarOcc

type str_context = str_elem -> bool

fun search(sym, []) = NONE
  | search(sym, (_,sym',key)::rest) = 
    if S.eq(sym,sym') then SOME key else search(sym,rest)

fun str_has_key (acc,filepath) ({access,...}:str_elem) = 
    case compare_acc (acc,access)
     of EQUAL => true
      | _ => false

fun var_has_key (acc,filepath) ({access,...}:var_elem) = 
    case compare_acc (acc,access)
     of EQUAL => true
      | _ => false

fun type_has_key s ({stamp,...}: type_elem) = 
    Stamps.eq (s,stamp)

fun cons_has_key (stamp,sym) ({name,dataty,...}:cons_elem) = 
    Stamps.eq (stamp,dataty) andalso S.eq (sym,name)

fun sig_has_key s ({stamp,...}: sig_elem) = 
    Stamps.eq (s, stamp)

fun fileOf ({def,...}: str_elem) = locFile def

fun getSlotElements (Def elems, slot) : key =
      #3(List.nth(elems,slot))
  | getSlotElements (Constraint(elems,acc), slot) =
      let val (_,_,slot') = List.nth(elems,slot)
      in case find_str(str_has_key (acc,"???"))
	  of SOME {elements,...} => getSlotElements(elements,slot')
	   | NONE => bug ""
      end
  | getSlotElements (Alias acc, slot) =
    (case find_str(str_has_key (acc,"???"))
      of SOME {elements,...} => getSlotElements(elements,slot)
       | NONE => bug "")

fun find(strelem,rest,filepath) : location option =
    let val {elements,def,...} = strelem
    in  case rest
	 of [] => SOME def
	  | [last] => 
	    (case elements
	      of Def elems =>
		 (case search(last,elems)
		   of SOME key =>
		      (case key
			of Str acc =>
			   (case find_str(str_has_key (acc, "???"))
			     of SOME{def,...} => SOME def
			      | NONE => NONE)
			 | Var acc =>
			   (case find_var(var_has_key (acc, "???"))
			     of SOME{def,...} => SOME def
			      | NONE => NONE)
			 | Type stamp =>
			   (case find_typ(type_has_key (stamp))
			     of SOME{def,...} => SOME def
			      | NONE => NONE)
			 | Cons (stamp,name) =>
			   (case find_cons(cons_has_key (stamp,name))
			     of SOME{def,...} => SOME def
			      | NONE => NONE)
			 | Sig stamp =>
			   (case find_sig(sig_has_key (stamp))
			     of SOME{def,...} => SOME def
			      | NONE => NONE)
		      )
		    | NONE => NONE)
	       | Constraint _ => NONE
	       | Alias _ => NONE)
	  | next::rest' =>
	    (case elements
	      of Def elems => 
		 (case search(next,elems)
		   of SOME (Str acc) => 
		      (case find_str(str_has_key (acc,filepath))
			of SOME strelem' =>
			   find (strelem',rest',"???")
			 | NONE => error())
		    | _ => bug "")
	       | Constraint (elems,acc) => 
		 (case search(next,elems)
		   of SOME slot =>
		      (case find_str(str_has_key (acc,"???"))
			of SOME{elements,...} =>
			   let val Str acc' = getSlotElements (elements,slot)
			   in (case find_str(str_has_key (acc',filepath))
				of SOME strelem' =>
				   find (strelem',rest',"???")
				 | NONE => error())
			   end
			 | NONE => NONE)
		    | NONE => NONE)
	       | Alias acc => 
		 (case find_str(str_has_key (acc,filepath))
		   of SOME strelem'' => find(strelem'',rest,"???")
		    | NONE => error()))
    end

fun getStrElemName(sym: S.symbol, context: str_context) =
    filter_str (fn (elem as {name,...}: str_elem) => S.eq(sym,name) andalso context elem)

(* defining occurrence of a variable designated by a path
 * e.g. for A.x, where A has a simple structure definition,
 * the defining occurrence would be the binding occurrence
 * of the x variable in the body of A *)
fun pathDefOcc(path: S.symbol list, context) : location option =
    let val strname::rest = path  (* a symbol *)
     in case getStrElemName(strname,context)
	  of [] => NONE  (* no structure for head of path *)
	   | x::y::rest => NONE (* ambiguous interpretation of head *)
           | [strelem] => find(strelem,rest,fileOf(strelem))
    end

end (* structure Queries *)
