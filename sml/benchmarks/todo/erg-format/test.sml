(* test.sml
 *)

structure Main : BMARK =
  struct

    open Format

    fun app f = let
	  fun app' [] = () | app' (x::r) = (f x; app' r)
	  in
	    app'
	  end

    val out1 = [Int 0, Int ~123, Int 5930, Int ~12120000, Int 9945]
    val out2 = [Re 0.0, Re ~1.0E~12, Re 0.99999999, Re 3.14159, Re ~0.00001]

    fun f11 () = format "11: %d %d %d %d %d\n" out1
    fun f12 () = format "12: %#8x %#8x %#8x %#8x %#8x\n" out1
    fun f13 () = format "13: %-6d %-6d %-6d %-6d %-6d\n" out1
    fun f21 () = format "21: %f %f %f %f %f\n" out2
    fun f22 () = format "22: %g %g %g %g %g\n" out2
    fun f23 () = format "23: %e %e %e %e %e\n" out2
    fun f24 () = format "24: %7.3f %7.3f %7.3f %7.3f %7.3f\n" out2
    fun f25 () = format "25: %7.3g %7.3g %7.3g %7.3g %7.3g\n" out2
    fun f26 () = format "26: %7.3e %7.3e %7.3e %7.3e %7.3e\n" out2

    val funs = [f11, f12, f13, f21, f22, f23, f24, f25, f26]

    fun testit strm = let
	  val pr = IO.outputc strm
	  in
	    app (fn f => pr(f())) funs
	  end

    fun doit () = let
	  fun lp 0 = ()
	    | lp i = (app (fn f => f()) funs; lp(i-1))
	  in
	    lp 1000
	  end

  end (* Main *)
