(* real64-values.sml
 *
 * Specific Real64 values that we need both in the real64.sml and num-scan.sml
 * files.
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Real64Values : sig

  (* maximum finite 64-bit real value *)
    val maxFinite : real
  (* minimum normalized positive real value *)
    val minNormalPos : real
  (* minimum positive real value (denormalized) *)
    val minPos : real
  (* positive infinity *)
    val posInf : real
  (* negative infinity *)
    val negInf : real

  end = struct

  (* The next three values are computed laboriously, partly to
   * avoid problems with inaccurate string->float conversions
   * in the compiler itself.
   *)
    val maxFinite = let
	  fun f (x, i) = if i = 1023 then x else f(x*2.0, i + 1)
	  val y = f(1.0, 0)
	  fun g (z, y, 0) = z
	    | g (z, y, i) = g (z+y, y*0.5, i - 1)
	  in
	    g (0.0, y, 53)
	  end

    val minNormalPos = let
	  fun isNormal x = (case Assembly.A.logb x
		 of ~1023 => false	(* 0.0 or subnormal *)
		  | 1024 => false	(* inf or nan *)
		  | _ => true
		(* end case *))
	  fun f(x) = let
		val y = x * 0.5
		in
		  if isNormal y then f y else x
		end
	  in
	    f 1.0
	  end

    local
      (* The x86 uses extended precision (80 bits) internally, therefore
       * it is necessary to write out the result of r * 0.5 to get
       * 64 bit precision.
       *)
      val mem = InlineT.PolyArray.array(1, minNormalPos)
      val update = InlineT.PolyArray.update
      val subscript = InlineT.PolyArray.chkSub
      fun f () = let
	    val r = subscript(mem, 0)
	    val y = r * 0.5
	    in
	       update(mem, 0, y);
	       if InlineT.Real64.==(subscript(mem, 0), 0.0) then r else f ()
	    end
    in
    val minPos = f()
    end (* local *)

    val posInf = maxFinite * maxFinite
    val negInf = ~posInf

  end
