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
    type control = { mname: string, priority: int list, obscurity: int,
		     name : string, descr : string, svar : svar }

    type 'a tinfo =
	 { tname : string, parse : string -> 'a option, show : 'a -> string }

    datatype module =
	NOCONFIG
      | MODULE of { name : string, priority : int list, obscurity : int,
		    prefix : string,
		    default_suffix : string option,
		    mk_ename : (string -> string) option }

    type 'a registry =
	 { new : { stem : string, descr : string, fallback : 'a } -> 'a ref,
	   acc : string -> 'a ref,
	   sacc : string -> svar }
	 
    val noconfig = NOCONFIG
    val module = MODULE

    val configurers : (unit -> unit) list ref = ref []
    val controls : control M.map ref = ref M.empty

    fun r2v r = { get = fn () => !r, set = fn x => r := x }

    fun registry NOCONFIG { tname, parse, show } =
	let val reg = ref M.empty
	    fun cvt s =
		case parse s of
		    SOME x => x
		  | NONE => raise FormatError { t = tname, s = s }
	    fun new { stem, descr, fallback } =
		case M.find (!reg, stem) of
		    SOME r => r
		  | NONE => let
			val r = ref fallback
		    in
			reg := M.insert (!reg, stem, r);
			r
		    end
	    fun acc stem =
		case M.find (!reg, stem) of
		    SOME r => r
		  | NONE => raise NoSuchControl
	    fun sacc stem = let
		val { get, set } = r2v (acc stem)
	    in
		{ get = show o get, set = set o cvt }
	    end
	in
	    { new = new, acc = acc, sacc = sacc }
	end
      | registry (MODULE m) { tname, parse, show } = let
	    val { name = mname, priority, obscurity,
		  prefix, default_suffix, mk_ename } = m
	    fun cvt s =
		case parse s of
		    SOME x => x
		  | NONE => raise FormatError { t = tname, s = s }
	    fun var2svar { get, set } = { get = show o get, set = set o cvt }
	    fun upcase_underscore s =
		let fun tr #"-" = "_"
		      | tr c = String.str (Char.toUpper c)
		in String.translate tr s
		end
	    val mken = getOpt (mk_ename, upcase_underscore)
	    val reg = ref M.empty
	    fun getUsing looker = Option.map cvt o looker
	    val getEnv = getUsing OS.Process.getEnv
	    fun new { stem, descr, fallback } =
		case M.find (!reg, stem) of
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
			val r = ref initial
			val var as { get, set } = r2v r
			fun configure () = Option.app set (getEnv ename)
			val control =
			    { mname = mname,
			      priority = priority, obscurity = obscurity,
			      name = name, descr = descr, svar = var2svar var }
		    in
			controls := M.insert (!controls, name, control);
			configurers := configure :: !configurers;
			reg := M.insert (!reg, stem, r);
			r
		    end
	    fun acc stem =
		case M.find (!reg, stem) of
		    SOME r => r
		  | NONE => raise NoSuchControl
	in
	    { new = new, acc = acc, sacc = var2svar o r2v o acc }
	end

    fun new_ref (r : 'a registry) = #new r
    fun acc_ref (r : 'a registry) = #acc r
    fun new r = r2v o new_ref r
    fun acc r = r2v o acc_ref r
    fun sacc (r : 'a registry) = #sacc r

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

    val bool =
	{ tname = "bool", parse = Bool.fromString, show = Bool.toString }
    val int =
	{ tname = "int", parse = Int.fromString, show = Int.toString }
    val real =
	{ tname = "real", parse = Real.fromString, show = Real.toString }
    val string =
	{ tname = "string", parse = SOME, show = fn x => x }
    val stringList =
	{ tname = "string list",
	  parse = SOME o String.tokens Char.isSpace,
	  show = concat o foldr (fn (s, r) => " " :: s :: r) [] }
end
