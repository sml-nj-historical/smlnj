signature MLRISC_CONTROL =
sig

    type cpu_time = {gc:Time.time,usr:Time.time,sys:Time.time}

    val mlrisc        : bool ref               (* use the MLRISC optimizer? *)
    val mlrisc_phases : string list ref        (* the optimization phases *)
    val debug_stream  : TextIO.outstream ref   (* debugging output goes here *)

(*
        (* Flags and counters *)
    val counters      : (string * int ref) list ref
    val ints          : (string * int ref) list ref
    val flags         : (string * bool ref) list ref
    val reals         : (string * real ref) list ref
    val strings       : (string * string ref) list ref
    val stringLists   : (string * string list ref) list ref
    val timings       : (string * cpu_time ref) list ref

*)
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

structure MLRiscControl : MLRISC_CONTROL =
struct
   type cpu_time = {gc:Time.time,usr:Time.time,sys:Time.time}

   val mlrisc        = ref false
   val mlrisc_phases = ref [] : string list ref
   val debug_stream  = ref TextIO.stdOut

(*
   val counters      = ref [] : (string * int ref) list ref
   val ints          = ref [] : (string * int ref) list ref
   val flags         = ref [("mlrisc",mlrisc)] : (string * bool ref) list ref
   val reals         = ref [] : (string * real ref) list ref
   val strings       = ref [] : (string * string ref) list ref
   val stringLists   = ref [("mlrisc-phases",mlrisc_phases)] 
                         : (string * string list ref) list ref
   val timings       = ref [] : (string * cpu_time ref) list ref
   local
      fun get(list,name : string,[],default) = 
             let val r = ref default in list := (name,r) :: !list; r end
        | get(list,name,(n,r)::rest,default) = 
             if name = n then r else get(list,name,rest,default)      
   in
      fun getCounter name = get(counters,name,!counters,0)
      fun getInt name     = get(ints,name,!ints,0)
      fun getFlag name    = get(flags,name,!flags,false)
      fun getReal name    = get(reals,name,!reals,0.0)
      fun getString name  = get(strings,name,!strings,"")
      fun getStringList name  = get(stringLists,name,!stringLists,[])
      fun getTiming name  = get(timings,name,!timings,
                                 {gc =Time.zeroTime,
                                  usr=Time.zeroTime,
                                  sys=Time.zeroTime})
   end
*)

    structure C = Controls

    val m0 = C.noconfig
    val m = C.module { name = "MLRISC",
		       priority = [10, 3],
		       obscurity = 3,
		       prefix = "mlrisc-",
		       default_suffix = SOME "-default",
		       mk_ename = NONE }

    val counter_r = C.registry m0 C.int
    val int_r =	C.registry m C.int 
    val flag_r = C.registry m C.bool
    val real_r = C.registry m C.real
    val string_r = C.registry m C.string
    val stringList_r = C.registry m C.stringList
    val timing_r =
	C.registry m0 { tname = "timing",
			parse = fn _ => (NONE : cpu_time option),
			show = fn _ => "<timing>" }

    fun mkCounter (stem, descr) =
	C.new_ref counter_r { stem = stem, descr = descr, fallback = 0 }
    fun mkInt (stem, descr) =
	C.new_ref int_r { stem = stem, descr = descr, fallback = 0 }
    fun mkFlag (stem, descr) =
	C.new_ref flag_r { stem = stem, descr = descr, fallback = false }
    fun mkReal (stem, descr) =
	C.new_ref real_r { stem = stem, descr = descr, fallback = 0.0 }
    fun mkString (stem, descr) =
	C.new_ref string_r { stem = stem, descr = descr, fallback = "" }
    fun mkStringList (stem, descr) =
	C.new_ref stringList_r { stem = stem, descr = descr, fallback = [] }
    fun mkTiming (stem, descr) =
	C.new_ref timing_r { stem = stem, descr = descr,
			     fallback = { gc = Time.zeroTime,
					  usr = Time.zeroTime,
					  sys = Time.zeroTime } }


    val counter = C.acc_ref counter_r
    val int = C.acc_ref int_r
    val flag = C.acc_ref flag_r
    val real = C.acc_ref real_r
    val string = C.acc_ref string_r
    val stringList = C.acc_ref stringList_r
    val timing = C.acc_ref timing_r

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
