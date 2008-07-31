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

structure Query =
struct

structure S = Symbol

open DBTypes Database QueryUtil

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

end (* structure Query *)
