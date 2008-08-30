structure DBTypes =
struct

   (* Remarks :
    * modifications to be done :

    * stamps are not sufficient to distinguish two types or signatures because
    * only exported objects have their stamps transformed into global
    * stamps, so the comparison also need to use the file of definition

    * in many places, symbols are used, and it should not be the case because
    * symbols contain namespaces, but these namespaces do not match the
    * different sets defined here
    * almost everywhere, strings should be used in place of symbols (see the
    * serializer : in most places, symbols are serialized as strings and
    * reconstructed as symbols, meaning the namespace information of a symbol
    * is useless)
    * a new symbol should be created for some cases : occurrence set, definition
    * of structures and signatures where we want to be able to tell the (new)
    * namespace of a symbol

    * should be added :
    * sets for exceptions and functors
    *)


    (* representation of files, since CM's one is not available
     * the string is /global/path/cmfile.cm:sourcecode.sml *)
    type file = string

    (* location = (file,pos_beginning,pos_end) *)
    type location = file * int * int
    fun locFile ((f,_,_) : location) = f
    fun eqLocation (l1 : location, l2) = l1 = l2

    (* exception for Database.charposToOccurrence function *)
    exception NoSuchFile of string
    exception NoMoreSymbol of (int * string)

    (*simplified versions of the internal compiler types*)
    datatype stub_tycon
      = General of Stamps.stamp * InvPath.path
      | Record of Symbol.symbol list
      | Path of InvPath.path (* translation of PATHtyc; appears only in sigs *)

    datatype ty'
      = Conty of stub_tycon * ty' list
      | Ibound of int
      (* references to poly-bound type variables within the body of the
       * polytype *)
      | Lbound of {index: int, depth: int}
      (* type variables bound by implicit type abstractions in expressions *)
      | Ubound of Symbol.symbol
      (* Residual user-introduced type variables. These will rarely occur,
       * since where they occur in the type of a definiens, they must be
       * generalized, and hence translated into IBOUND in polytypes and LBOUND
       * in expression types.
       * However, they may occur in the anomolous cases where they don't get
       * generalized, such as "val x = ([]: 'a list, 3);" *)
      | Poly of {arity: int, body: ty'}

    (* descriptive info for tycon records *)
    datatype tycon'
      = Datatype of bool * Symbol.symbol list (* eq * cons list *)
      | Abstract of Symbol.symbol list
      (* cons list for representation datatype (why?)
       * it does seem to be useless *)
      | Deftyc
      | Primtyc of bool (* : eq *)


    type varUse = location * ty' * Access.access
    (* location is the position
     * ty' is the type in this occurrence
     * access is the access that is used to talk about x (useful when functors
     * are added and a source definition may correspond to multiple runtime
     * values) *)

    (*the record containing def locations ...*)
    type var_elem
      = { access : Access.access, (* access of definition => LVAR *)
	  name : Symbol.symbol,
	  parent : Access.access,
          (* access of parent structure *)
          (* should be an option because :
           * local
           *   val x = 1
           * in
           * structure s =
           *   struct
           *     ...
           *   end
           * in this case, x has no parent
           * concevably, x's parent could be an EXTERN but the same convention
           * should be used for strusture
           *)
	  typ : ty',
	  def : location,
	  usage : varUse list ref
	}

    type typeUse = location
    (* we should probably record types here for parameterized types like
     * type 'a t = ('a,string)  *)

    type type_elem
      = { tycon : tycon',
	  stamp : Stamps.stamp,
          name : Symbol.symbol,
	  def : location,
	  usage : typeUse list ref
	}

    type consUse = location * ty'

    type cons_elem
      = { name : Symbol.symbol,
	  dataty : Stamps.stamp, (* stamp of the type of the constructor's
                                  * family*)
	  def : location,
	  ty : ty',
	  usage : consUse list ref
	}

    (* the information needed to identify an object (assuming we know the file
     * of definition for local definition)
     * ex1 : Var (LVAR _) is assumed to be defined in the same file as the
     * structure
     * ex2 : Var (PATH(EXTERN_, _)) is external and contains enough information
     * to indentify the variable *)
    datatype key
      = Var of Access.access
      | Str of Access.access
      | Type of Stamps.stamp
      | Cons of Stamps.stamp * Symbol.symbol
      | Sig of Stamps.stamp

    (* three case for defining a structure :
     * struct ... end -> Def ( (slot,symbol,key) list)
       (here the symbol should be a new type (see first comment))
     * structure s : sig = s2 ->
       Constraint ((slot_s, symbol, slot_s2) list, access_s2)
       (same remark for symbols)
     * structure s = s2 -> Alias (access_s2)
     *)
    datatype elements
      = Def of (int * Symbol.symbol * key) list
      | Constraint of (int * Symbol.symbol * int) list * Access.access
      | Alias of Access.access

    type strUse = location

    type str_elem
      = { name : Symbol.symbol,
	  access : Access.access,
	  parent : Access.access option,
          (* parent structure if current structure is not toplevel *)
	  sign : Stamps.stamp option,
          (* stamp of the signature *)
          (* I don't remember why that should be an option *)
	  def : location,
	  elements : elements,
	  usage : strUse list ref
	}

    type sigUse = location

    (* right part of a signature element
     * type t = tycon' -> tycon'
     * val v : ty' -> ty'
     * exception e = ty' -> ty'
     * structure A : s -> (s, stamp of s)
     * structure A : sig content end -> content *)
    datatype spec_sig
      = Typ of tycon'
      | Val of ty'
      | Exception of ty'
      | NamedStr of Symbol.symbol * Stamps.stamp
      | InlineStr of def_sig

    (* symbol is the name on the left of signatures elements*)
    withtype def_sig = (Symbol.symbol * spec_sig) list

    datatype elements_sig
      = AliasSig of Stamps.stamp (* when saying signature s = s2 *)
      | DefSig of def_sig (* when saying signature s = sig ... end *)

    type sig_elem
      = { name : Symbol.symbol,
	  stamp : Stamps.stamp,
	  inferred : bool,
	  def : location,
	  elements : elements_sig,
	  usage : sigUse list ref
	}

    (* element used to record usage in a file of symbol defined in other files*)
    datatype ext_elem
      = ExtVar of
	  { access : Access.access,
	    usage : varUse list ref
	  }
      | ExtStr of
	  { access : Access.access,
	    usage : strUse list ref
	  }
      | ExtType of
	  { stamp : Stamps.stamp,
	    usage : typeUse list ref
	  }
      | ExtCons of
	  { stamp : Stamps.stamp,
	    name : Symbol.symbol,
	    usage : consUse list ref
	  }
      | ExtSig of
	  { stamp : Stamps.stamp,
	    usage : sigUse list ref
	  }

    (* element recording each occurrence in the file with the name of namespace
     *  for quick look up*)
    type occurrence = Symbol.symbol * location

end (* structure DBTypes *)

