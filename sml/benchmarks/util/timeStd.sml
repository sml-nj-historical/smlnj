(* Copyright 1996 by Yale FLINT Project *)
(* util/timeStd.sml *)

(* a signature template for all std benchmarks *)
signature BMARK =
sig
  val doit : unit -> unit
  val testit : TextIO.outstream -> unit
end

signature TIMING =
sig
  type filename = string
  type hint = string
  type outstream = TextIO.outstream

  val timeComp : filename -> unit         
  val timeRun  : (unit -> unit) -> unit   (* run three times *)
  val timeIt   : (unit -> unit) -> unit   (* run once *)

  val timeRunF  : outstream * (unit -> unit) -> unit
  val timeCompF : outstream * filename -> unit 

  (* 
   * Timing the compilation of a particular benchmark;
   *   Arg #1 is the output file.
   *   Arg #2 is the "hint" field, used by the Latex option only.
   *     It is not pretty; the data processing to build latex table
   *     should simply be separated out from this script in the long run.
   *
   *   Arg #3 for timeRun is the number of times each bench should be run.
   *     Notice each run already consists of three repetitions.
   *   Arg #4 for timeRun is the actual loader for the benchmark.
   *
   *   Arg #3 for timeComp is just the benchmark filename.
   *)
  
  val timeRunK        : outstream * hint * int * (unit -> unit) -> unit
  val timeRunKJgraph  : outstream * hint * int * (unit -> unit) -> unit
  val timeRunKLatex   : outstream * hint * int * (unit -> unit) -> unit

  val timeCompK       : outstream * hint * filename -> unit
  val timeCompKJgraph : outstream * hint * filename -> unit
  val timeCompKLatex  : outstream * hint * filename -> unit

end (* end of signature TIMING *)


(***************************************************************************
 *                           The Timing Structure                          *
 ***************************************************************************)
structure Timing : TIMING =
struct

local structure T = Time
      structure Tm = Timer
in 

type filename = string
type hint = string
type outstream = TextIO.outstream
type time = T.time
type timing = {usr:time, gc:time, sys:time, real:time, alloc:int}

val timeofday : unit -> T.time = T.now
val start_timer = Tm.startCPUTimer
val check_timer = Tm.checkCPUTimer

val gcCtl : ((string * int ref) list -> unit) =
  Unsafe.CInterface.c_function "SMLNJ-RunT" "gcControl"
fun doGC n = gcCtl [("DoGC", ref n)]

(* convert a time value to a integer *)
fun timeToInt t = LargeInt.toInt (T.toMilliseconds t) 

val pad10 = "          "
fun pad (s, n) = 
  let val l = size s
   in if (n <= l) then s
      else if ((n-l) >= 10)
	   then pad (pad10^s, n)
	   else substring(pad10, 0, n-l) ^ s
  end

fun start () = (doGC 2; 
                (* System.Runtime.gc 2;
	           System.Runtime.allocsize := 0;
                   System.Runtime.spaceusage := 0;
                 *)
	        {realt = timeofday(), timer = start_timer()})

fun stop {realt=rt, timer=t} = 
  let val t' = check_timer t
      val tc = #usr t'
      val ts = #sys t'
      val tg = #gc t'
      val rt' = T.-(timeofday(),rt)
      val _ = doGC 2
      (*    val _ = System.Runtime.gc 2 *)
      val alloc = 0 (* !allocsize *)
      val spaceuse = 0 (* !spaceusage *)
   in (timeToInt tc, timeToInt tg, timeToInt ts,
       timeToInt rt', alloc, spaceuse)
  end

(** this function requires clean-up *)
fun getCodeSize(s) =
  let fun h(x::r,s) = 
           if (Char.>=(x,#"0") andalso Char.<=(x,#"9")) then
             (let val k = Char.ord(x) - Char.ord(#"0")
               in h(r, k+(s*10)) 
              end)
           else h(r,s)
        | h([],s) = s
   in h((explode s),0)
  end

(* Time one compilation of the benchmark *)
(* TODO: add breakdown summary figures to the compile time list *)
fun compileIt (fname) = 
  let (* val _ = OS.FileSys.chDir fname *)
      val spfile = "/tmp/.codesize3"
      val tmpstream = TextIO.openOut(spfile)
      val _  = Compiler.Stats.reset() 
      val t0 = start()
      val _ = use fname
      val res = stop t0

      val sss = fn s => TextIO.output(tmpstream, s)
      val fff = fn () => TextIO.flushOut(tmpstream)
      val tmp = !Compiler.Control.Print.out
      val _ = (Compiler.Control.Print.out := {flush=fff, say=sss})
      val _ = Compiler.Stats.summary()
      val _ = (Compiler.Control.Print.out := tmp)
      val _ = (fff(); TextIO.closeOut tmpstream)

      val tmpstream = TextIO.openIn(spfile)
      val line = TextIO.inputLine(tmpstream)
      val sz = getCodeSize(line)
      val _ = TextIO.closeIn(tmpstream)
      val _ = OS.FileSys.remove spfile
(*
      val _ = print ("** " ^ line)
      val _ = print ("**, " ^ (Int.toString sz) ^ " ** \n")
*)
   in (#1 res, #2 res, #3 res, #4 res, #5 res, sz)
  end

val maxConst = 536870912

fun loop(b : int,n,s,f) =
  if b > n then ()
  else (f(b); loop(b+s,n,s,f))

fun accum(b : int, n, s, init, merge, f) =
  if b > n then init
  else (merge(f(b),accum(b+s,n,s,init,merge,f)))

fun runItOnce doit = 
  let val t0 = start()
   in doit(); stop t0
  end

(* run it k times, get the average *)
fun runIt (doit, k) = 
  let val result = Array.array(k+1,(0,0,0,0,0,0))
      val _ = loop(1,k,1,fn j => (Array.update(result,j,runItOnce doit)))
      val init = (0,0,0,0,0,0)
      fun merge((x1,x2,x3,x4,x5,x6),(y1,y2,y3,y4,y5,y6)) = 
            (x1+y1+0,x2+y2+0,x3+y3+0,x4+y4+0,x5+y5+0,x6+y6+0)
      fun f(j) = Array.sub(result,j)
      val (x1,x2,x3,x4,x5,x6) = accum(1,k,1,init,merge,f)
      val h = Int.div
   in (h(x1,k), h(x2,k), h(x3,k), h(x4,k), h(x5,k), h(x6,k))
  end

val numRep = 3

(* run it k times, pick the best run *)
fun runItK (k, doit) =
  let val result = Array.array(k+1,(0,0,0,0,0,0))
      val _ = loop(1,k,1,fn j => (Array.update(result,j,runIt(doit, numRep))))
      val init = (maxConst,0,0,maxConst,0,0)
      fun merge(xx as (x1,x2,x3,x4 : int,x5,x6), yy as (y1,y2,y3,y4,y5,y6)) = 
            if (x4 > y4) then yy else xx
       (*   if (x1 > y1) then yy else xx *)
      fun f(j) = Array.sub(result,j)
   in accum(1,k,1,init,merge,f)
  end

(***************************************************************************
 *                     OUTPUT AND FORMATTING FUNCTIONS                     *
 ***************************************************************************)
(* convert a time value to a string, padded on the left to 8 characters *)
(*
fun timeToStr (TIME{sec, usec}) = 
  let val tenMS = (usec + 5000) quot 10000
      val str = let val s = makestring tenMS
		 in if (tenMS < 10) then ".0"^s else "."^s
		end
      val s = (makestring sec) ^ str
   in pad (s, 6)
  end
*)
val timeToStr = T.toString

(* convert an integer number of Kilobytes to a string *)
fun allocStr n = 
  let val n = if n > 10000 then n + 512 else n
      val tenth = Int.quot((10*(Int.rem(n,1024))),1024)
      val mb = Int.div(n,1024)
   in pad (concat[Int.toString mb, ".", Int.toString tenth],7)
  end

(* convert a integer to a time string, padded on the left to 8 characters *)
fun IntToStr i = 
  let val tenMS = Int.quot((i mod 1000),10)
      val sec = Int.quot(i, 1000)
      val str = let val s = Int.toString tenMS
		 in if (tenMS < 10) then ".0"^s else "."^s
		end
      val s = (Int.toString sec) ^ str
   in pad (s, 6)
  end

(* output the performance data *)
fun output (strm, (x1,x2,x3,x4,x5,x6)) =
   TextIO.output (strm, concat["  usr= ",   IntToStr x1,
  	                       ", sys= ",   IntToStr x2,
	                       ", gc= ",    IntToStr x3,
	                       ", real= ",  IntToStr x4,
	                       ", alloc= ", allocStr x5,
                               ", space= ", allocStr x6, 
                               " \n"])

(* output the performance data for future latex use *)
fun outputLatex(strm, fname, (x1,x2,x3,x4,x5,x6)) = 
     TextIO.output (strm, concat[fname, 
                                 "\t&",IntToStr x1,
	                         "\t&",IntToStr x2,
	                         "\t&",IntToStr x3,
	                         "\t&",IntToStr x4,
	                         "\t&",allocStr x5, 
	                         "\t&",allocStr x6, 
                                 "\t\\\\  \\hline \n"])

(* output the performance data for future jgraph use *)
fun outputJgraph(strm, fname, (x1,x2,x3,x4,x5,x6)) = 
     TextIO.output (strm, concat[fname,
                                 "\t",IntToStr x1,
	                         "\t",IntToStr x2,
	                         "\t",IntToStr x3,
	                         "\t",IntToStr x4,
	                         "\t",allocStr x5, 
                                 "\t",allocStr x6,
                                 "\n"])


(***************************************************************************
 *                           MAIN FUNCTIONS                                *
 ***************************************************************************)
fun timeComp fname = output(TextIO.stdOut, compileIt fname)
fun timeRun doit = output(TextIO.stdOut, runIt(doit, numRep))
fun timeIt doit = output(TextIO.stdOut, runIt(doit, 1))

fun timeCompF (outstrm, fname) = output(outstrm, compileIt fname)
fun timeRunF (outstrm, doit) = output(outstrm, runIt(doit, numRep))


fun timeCompK(outstrm, prop, fname) = 
      output(outstrm, compileIt(fname))
fun timeCompKLatex(outstrm, prop, fname) = 
      outputLatex(outstrm, prop, compileIt(fname)) 
fun timeCompKJgraph(outstrm, prop, fname) = 
      outputJgraph(outstrm, prop, compileIt(fname)) 

fun timeRunK(outstrm, fname, k, doit) = 
      output(outstrm, runItK(k,doit))
fun timeRunKLatex(outstrm, fname, k, doit) = 
      outputLatex(outstrm, fname, runItK(k,doit)) 
fun timeRunKJgraph(outstrm, fname, k, doit) = 
      outputJgraph(outstrm, fname, runItK(k,doit))

end (* toplevel local *)
end (* structure Timing *)

