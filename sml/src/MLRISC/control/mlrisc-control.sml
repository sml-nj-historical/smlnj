signature MLRISC_CONTROL =
sig

    type cpu_time = {gc:Time.time,usr:Time.time,sys:Time.time}

    val mlrisc        : bool ref               (* use the MLRISC optimizer? *)
    val mlrisc_phases : string list ref        (* the optimization phases *)
    val debug_stream  : TextIO.outstream ref   (* debugging output goes here *)

        (* Flags and counters *)
    val counters      : (string * int ref) list ref
    val ints          : (string * int ref) list ref
    val flags         : (string * bool ref) list ref
    val reals         : (string * real ref) list ref
    val strings       : (string * string ref) list ref
    val stringLists   : (string * string list ref) list ref
    val timings       : (string * cpu_time ref) list ref

        (* Functions to get these *)
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

end
