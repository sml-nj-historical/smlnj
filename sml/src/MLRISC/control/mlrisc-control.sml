signature MLRISC_CONTROL =
sig

    type cpu_time = {gc:Time.time,usr:Time.time,sys:Time.time}

    val mlrisc        : bool ref               (* use the MLRISC optimizer? *)
    val mlrisc_phases : string list ref        (* the optimization phases *)
    val debug_stream  : TextIO.outstream ref   (* debugging output goes here *)

    type 'a entry = { stem: string, descr: string, cell: 'a ref }

        (* Flags and counters *)
    val counters    : int entry list ref
    val ints        : int entry list ref
    val flags       : bool entry list ref
    val reals       : real entry list ref
    val strings     : string entry list ref
    val stringLists : string list entry list ref
    val timings     : cpu_time entry list ref

    val mkCounter    : string * string -> int ref
    val mkInt        : string * string -> int ref
    val mkFlag       : string * string -> bool ref
    val mkReal       : string * string -> real ref
    val mkString     : string * string -> string ref
    val mkStringList : string * string -> string list ref
    val mkTiming     : string * string -> cpu_time ref

    val counter      : string -> int ref
    val int          : string -> int ref
    val flag         : string -> bool ref
    val real         : string -> real ref
    val string       : string -> string ref
    val stringList   : string -> string list ref
    val timing       : string -> cpu_time ref

    (* The following is the old interface.  Its use is deprecated
     * since it does not provide documentation strings. *)
    val getCounter    : string -> int ref
    val getInt	      : string -> int ref
    val getFlag       : string -> bool ref
    val getReal       : string -> real ref
    val getString     : string -> string ref
    val getStringList : string -> string list ref
    val getTiming     : string -> cpu_time ref

end

structure MLRiscControl : MLRISC_CONTROL = struct
    type cpu_time = {gc:Time.time,usr:Time.time,sys:Time.time}

    val mlrisc        = ref false
    val mlrisc_phases = ref [] : string list ref
    val debug_stream  = ref TextIO.stdOut

    type 'a entry = { stem: string, descr: string, cell: 'a ref }

    val counters      = ref [] : int entry list ref
    val ints          = ref [] : int entry list ref
    val flags         = ref [{ stem = "mlrisc", descr = "?", cell = mlrisc }]
    val reals         = ref [] : real entry list ref
    val strings       = ref [] : string entry list ref
    val stringLists   = ref [{ stem = "phases", descr = "MLRISC Phases",
			       cell = mlrisc_phases }]
    val timings       = ref [] : cpu_time entry list ref
    local
	fun mk (list, fallback) (stem' : string, descr) = let
	    fun loop [] =
		let val cell = ref fallback
		in
		    list := { stem = stem', descr = descr, cell = cell }
			    :: !list;
	            cell
		end
	      | loop ({ stem, descr, cell } :: t) =
		if stem = stem' then cell else loop t
	in
	    loop (!list)
	end
    in
        fun mkCounter x = mk (counters, 0) x
	fun mkInt x = mk (ints, 0) x
	fun mkFlag x = mk (flags, false) x
	fun mkReal x = mk (reals, 0.0) x
	fun mkString x = mk (strings, "") x
	fun mkStringList x = mk (stringLists, []) x
	fun mkTiming x = mk (timings, {gc =Time.zeroTime,
                                       usr=Time.zeroTime,
                                       sys=Time.zeroTime}) x
    end

    local
	fun find list stem' = let
	    fun loop [] =
		raise Fail ("Control.MLRISC: no such control: " ^ stem')
	      | loop ({ stem, descr, cell } :: t) =
		if stem = stem' then cell else loop t
	in
	    loop (!list)
	end
    in
        val counter = find counters
	val int = find ints
	val flag = find flags
	val real = find reals
	val string = find strings
	val stringList = find stringLists
	val timing = find timings
    end

    local
	fun old_for mkFoo s = mkFoo (s, s ^ " setting")
    in
        val getCounter = old_for mkCounter
        val getInt = old_for mkInt
        val getFlag = old_for mkFlag
        val getReal = old_for mkReal
        val getString = old_for mkString
        val getStringList = old_for mkStringList
        val getTiming = old_for mkTiming
    end
end
