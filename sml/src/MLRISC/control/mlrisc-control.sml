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
    val strings       : (string * string ref) list ref
    val stringLists   : (string * string list ref) list ref
    val timings       : (string * cpu_time ref) list ref

        (* Functions to get these *)
    val getCounter    : string -> int ref
    val getInt	      : string -> int ref
    val getFlag       : string -> bool ref
    val getString     : string -> string ref
    val getStringList : string -> string list ref
    val getTiming     : string -> cpu_time ref
end

structure MLRISC_Control : MLRISC_CONTROL =
struct
   type cpu_time = {gc:Time.time,usr:Time.time,sys:Time.time}

   val mlrisc        = ref false
   val mlrisc_phases = ref [] : string list ref
   val debug_stream  = ref TextIO.stdOut

   val counters      = ref [] : (string * int ref) list ref
   val ints          = ref [] : (string * int ref) list ref
   val flags         = ref [("mlrisc",mlrisc)] : (string * bool ref) list ref
   val strings       = ref [] : (string * string ref) list ref
   val stringLists   = ref [("mlrisc-phases",mlrisc_phases)] 
                         : (string * string list ref) list ref
   val timings       = ref [] : (string * cpu_time ref) list ref
   local
      fun get(list,name : string,[],new) = 
             let val r = new() in list := (name,r) :: !list; r end
        | get(list,name,(n,r)::rest,new) = 
             if name = n then r else get(list,name,rest,new)      
   in
      fun getCounter name = get(counters,name,!counters,fn() => ref 0)
      fun getInt name     = get(ints,name,!ints,fn() => ref 0)
      fun getFlag name    = get(flags,name,!flags,fn() => ref false) 
      fun getString name  = get(strings,name,!strings,fn() => ref "")
      fun getStringList name  = get(stringLists,name,!stringLists,
                                    fn() => ref [])
      fun getTiming name  = get(timings,name,!timings,
                                fn() => ref {gc =Time.zeroTime,
                                             usr=Time.zeroTime,
                                             sys=Time.zeroTime})
   end

end
