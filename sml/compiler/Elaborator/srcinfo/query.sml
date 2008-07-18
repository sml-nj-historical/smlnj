(* query.sml *)

(* query structure
 *   how to represent queries as a data structure
 *   these query structures will be interpreted with respect to the database to generate "answers"
 *   potentially queries for a language, with a set of primitive queries and possibly ways of 
 *      combining queries into compound queries (e.g. boolean combinations -- conjunctions and disjunctions)
 *   query refinement: adding criteria to narrow the possible answers to a query
 *   answers will be derived from (sets of) database elements (records, or table entries)
 *)

(* example queries:

   describe variable named "x"   (no context?)
   describe the tycon "t"
   function definitions containing variable "x"
   find the location of the definition of "x"
   find the locations where "x" is used
   what module is "x" defined in?
   what module is "x" used in (imported into)?
   what source file is "x" defined in?
   is x local or exported from its parent module?
   what is the type of "x" at location "L"?
   what is the type of "x" as an element of structure "A"
   what structures import structure "A"?
   what structure contains location "L"?
   what function is variable x defined in (assuming it is a local variable)
   what is the nested hierarchy of functions containing this occurrence/location
   what are all the symbols defined in this compilation unit
   what symbols are exported from this structure
   what are the functions defined in a file (or module)
      primary (i.e. at "top-level" within the file/module
      seconday (i.e. nested within higher-level functions)
   explore the hierarchial tree of function definitions
   what is the symbol (or token?) at character position n in a file
*)

(* query : 
    The "abstract" subject (e.g. an "abstract" variable)
     basic designation + qualifiers
     1. basic designation thing of interest (subject).  E.g. a symbol (name and namespace)
     2. qualifications (narrowing scope of possible subjects), e.g. defining occurrence

    Attributes of the subject that we want to know:
        (e.g. type, defn location, use locations, relations with ...

     basic designation is typically a symbol, or a location
     qualifications?
	name space (if we don't view that as part of the basic designation)

    An "occurrence" of a symbol uniquely determines an abstract variable.
    An "occurrence" is a location of a single symbol.
*)

type occurrence = location (?)
type subject = OCC of occurrence | SYM of symbol | ???

(* a crude first attempt *)
datatype query = 
  Q of subjet * qualifier list * attributes
