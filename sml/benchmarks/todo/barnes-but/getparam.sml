(* getparam.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *)

structure GetParam : sig

    exception EOF

    val initParam : (string list * string list) -> unit
    val getParam : string -> string
    val getIParam : string -> int
    val getRParam : string -> real
    val getBParam : string -> bool

  end = struct

    exception EOF

    val defaults = ref ([] : string list)

  (* ignore arg vector, remember defaults. *)
    fun initParam (argv, defl) = defaults := defl

    fun prompt (fmt, items) = (
	  IO.output(IO.std_out, Format.format fmt items);
	  IO.flush_out std_out)

  (* export version prompts user for value. *)
    fun getParam name = let
	  fun scanBind [] = raise Fail(name ^ " unknown")
	    | scanBind (s::r) = if (StringUtil.isPrefix(name, s, 0))
		then SOME(StringUtil.suffix (s, size name + 1))
		  handle _ => NONE
		else scanBind r
	  fun get default = (case (IO.input_line IO.std_in)
		 of "" => raise EOF
		  | "\n" => default
		  | s => substring(s, 0, size s - 1)
		(* end case *))
	  in
	    if (null (! defaults))
	      then raise Fail "getParam called before initParam"
	      else ();
	    case (scanBind (! defaults))
	     of (SOME s) => (
		  prompt ("enter %s [%s]: ", [Format.STR name, Format.STR s]);
		  get s)
	      | NONE => (prompt ("enter %s: ", [Format.STR name]); get "")
	    (* end case *)
	  end

    local
      fun cvt (scanFn, projFn) = let
	    fun cvt' name = let
		  fun get () = (case getParam name of "" => get () | s => s)
		  val param = get ()
		  in
		    (projFn (scanFn param)) handle _ => (cvt' name)
		  end
	    in
	      cvt'
	    end
    in
  (* get integer parameter *)
    val getIParam = cvt (Format.scan "%d", fn [Format.INT n] => n)
  (* get real parameter *)
    val getRParam = cvt (Format.scan "%f", fn [Format.REAL r] => r)
  (* get bool parameter *)
    val getBParam = cvt (Format.scan "%b", fn [Format.BOOL b] => b)
    end (* local *)

  end; (* GetParam *)
