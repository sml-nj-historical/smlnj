signature BMARK =
  sig
    val doit : unit -> unit
    val testit : TextIO.outstream -> unit
  end;

structure Timing = struct
    structure TR = Timer
    structure T = Time
    type timing = {usr:T.time, gc:T.time, sys:T.time, real:T.time}
    val pad10 = "          "
    fun pad (s, n) = let
	  val l = size s
	  in
	    if (n <= l)
	      then s
	    else if ((n-l) >= 10)
	      then pad (pad10^s, n)
	      else substring(pad10, 0, n-l) ^ s
	  end
    fun start () = 
      (SMLofNJ.Internals.GC.doGC 1000;
       {realt = TR.startRealTimer(), timer = TR.startCPUTimer()}) 

    fun stop {realt, timer} = let
          val rt = TR.checkRealTimer realt 
          val {gc, sys, usr} = TR.checkCPUTimer timer
	  in {usr=usr, gc=gc, sys=sys, real=rt}
	  end

    (* convert a time value to a string, padded on the left to 8 characters *)
    fun timeToStr time = pad (Time.toString time, 6)

    fun output (strm, {usr, gc, sys, real} : timing) = let
	  val str = 
	    concat[
		    "\t{usr = ", timeToStr usr,
		    ", sys = ", timeToStr sys,
		    ", gc = ", timeToStr gc,
		    ", real = ", timeToStr real, "}"
		  ]
	  in
	    TextIO.output (strm, str)
	  end


  (* Time one run of the benchmark *)
    fun timeIt (outstrm, run, doit) = let
	    val t0 = start()
	  in
	    doit();
	    output (outstrm, stop t0); 
	    TextIO.flushOut outstrm
	  end

  (* Time n runs of the benchmark *)
    fun time (n, outstrm, doit) = let
	  fun loop 0 = ()
	    | loop 1 = timeIt(outstrm, 1, doit)
	    | loop i = 
	        (timeIt(outstrm, i, doit); 
		 TextIO.output(outstrm, ",\n");
		 loop(i-1))
	  in
	    TextIO.output(outstrm, "    Runs=[\n");
	    loop n;
	    TextIO.output(outstrm, "\t]")
	  end

end 


