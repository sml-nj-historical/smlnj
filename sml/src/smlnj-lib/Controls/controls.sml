(* controls.sml
 *
 * COPYRIGHT (c) 2002 Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume
 *)
structure Controls :> CONTROLS = struct

    structure M = RedBlackMapFn (type ord_key = string
				 val compare = String.compare)

    exception NoSuchControl
    exception FormatError of { t: string, s: string }

    type 'a var = { get : unit -> 'a, set : 'a -> unit }
    type svar = string var
    type control = { rname: string, priority: int list, obscurity: int,
		     name : string, descr : string, svar : svar }

    type 'a tinfo = { tname : string,
		      fromString : string -> 'a option,
		      toString : 'a -> string }

    datatype registry =
	NOCONFIG
      | REGISTRY of { name : string, priority : int list, obscurity : int,
		      prefix : string,
		      default_suffix : string option,
		      mk_ename : (string -> string) option }

    type 'a group =
	 { new : { stem : string, descr : string, fallback : 'a } -> 'a ref,
	   reg : { stem : string, descr : string, cell : 'a ref } -> unit,
	   acc : string -> 'a ref,
	   sacc : string -> svar }
	 
    val noconfig = NOCONFIG
    val registry = REGISTRY

    val configurers : (unit -> unit) list ref = ref []
    val controls : control M.map ref = ref M.empty

    fun ref2var r = { get = fn () => !r, set = fn x => r := x }

    fun group NOCONFIG { tname, fromString, toString } =
	let val m = ref M.empty
	    fun cvt s =
		case fromString s of
		    SOME x => x
		  | NONE => raise FormatError { t = tname, s = s }
	    fun new { stem, descr, fallback } =
		case M.find (!m, stem) of
		    SOME r => r
		  | NONE => let
			val r = ref fallback
		    in
			m := M.insert (!m, stem, r);
			r
		    end
	    fun reg { stem, descr, cell } =
		case M.find (!m, stem) of
		    SOME _ => raise Fail (concat ["Controls.register: ",
						  stem, " already registered\n"])
		  | NONE => m := M.insert (!m, stem, cell)
	    fun acc stem =
		case M.find (!m, stem) of
		    SOME r => r
		  | NONE => raise NoSuchControl
	    fun sacc stem = let
		val { get, set } = ref2var (acc stem)
	    in
		{ get = toString o get, set = set o cvt }
	    end
	in
	    { new = new, reg = reg, acc = acc, sacc = sacc }
	end
      | group (REGISTRY r) { tname, fromString, toString } = let
	    val { name = rname, priority, obscurity,
		  prefix, default_suffix, mk_ename } = r
	    fun cvt s =
		case fromString s of
		    SOME x => x
		  | NONE => raise FormatError { t = tname, s = s }
	    fun var2svar { get, set } =
		{ get = toString o get, set = set o cvt }
	    fun upcase_underscore s =
		String.map (fn #"-" => #"_" | c => Char.toUpper c) s
	    val mken = getOpt (mk_ename, upcase_underscore)
	    val m = ref M.empty
	    fun getUsing looker = Option.map cvt o looker
	    val getEnv = getUsing OS.Process.getEnv
	    fun mk (mkcell, stem, descr, fallback) =
		case M.find (!m, stem) of
		    SOME r => r
		  | NONE => let
			val name = prefix ^ stem
			val default =
			    Option.map (fn s => mken (name ^ s)) default_suffix
			val ename = mken name
			val initial =
			    case Option.join (Option.map getEnv default) of
				SOME v => v
			      | NONE => getOpt (getEnv ename, fallback)
			val r = mkcell initial
			val var as { get, set } = ref2var r
			fun configure () = Option.app set (getEnv ename)
			val control =
			    { rname = rname,
			      priority = priority, obscurity = obscurity,
			      name = name, descr = descr, svar = var2svar var }
		    in
			controls := M.insert (!controls, name, control);
			configurers := configure :: !configurers;
			m := M.insert (!m, stem, r);
			r
		    end
	    fun new { stem, descr, fallback } = mk (ref, stem, descr, fallback)
	    fun reg { stem, descr, cell = cell as ref fallback } =
		ignore (mk (fn v => (cell := v; cell), stem, descr, fallback))
	    fun acc stem =
		case M.find (!m, stem) of
		    SOME r => r
		  | NONE => raise NoSuchControl
	in
	    { new = new, reg = reg, acc = acc, sacc = var2svar o ref2var o acc }
	end

    fun new (r : 'a group) = #new r
    fun reg (r : 'a group) = #reg r
    fun acc (r : 'a group) = #acc r
    fun sacc (r : 'a group) = #sacc r

    fun control name =
	case M.find (!controls, name) of
	    NONE => raise NoSuchControl
	  | SOME c => c

    val controls =
	fn oopt =>
	   let val notobscure =
		   case oopt of
		       NONE => (fn _ => true)
		     | SOME x => (fn (c: control) => #obscurity c <= x)
	       val all = M.listItems (!controls)
	       val unobscure = List.filter notobscure all
	       fun clcmp (c: control, c': control) =
		   case List.collate Int.compare (#priority c, #priority c') of
		       EQUAL => String.compare (#name c, #name c')
		     | unequal => unequal
	       fun gt (c, c') = clcmp (c, c') = GREATER
	   in
	       ListMergeSort.sort gt unobscure
	   end

    fun init () = app (fn cnf => cnf ()) (!configurers)

    val bool = { tname = "bool",
		 fromString = Bool.fromString, toString = Bool.toString }
    val int = { tname = "int",
		fromString = Int.fromString, toString = Int.toString }
    val real = { tname = "real",
		 fromString = Real.fromString, toString = Real.toString }
    val string = { tname = "string",
		   fromString = SOME, toString = fn x => x }
    val stringList =
	{ tname = "string list",
	  fromString = SOME o String.tokens Char.isSpace,
	  toString = concat o foldr (fn (s, r) => " " :: s :: r) [] }
end
