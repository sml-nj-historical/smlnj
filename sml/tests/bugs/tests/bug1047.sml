(* bug1047.sml *)
(* lib-base-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

signature LIB_BASE =
  sig

    exception Unimplemented of string
	(* raised to report unimplemented features *)
    exception Impossible of string
	(* raised to report internal errors *)

    exception NotFound
	(* raised by searching operations *)

    val failure : {module : string, func : string, msg : string} -> 'a
	(* raise the exception Fail with a standard format message. *)

    val version : {date : string, system : string, version_id : int list}
    val banner : string

  end (* LIB_BASE *);


(* lib-base.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

structure LibBase : LIB_BASE =
  struct

  (* raised to report unimplemented features *)
    exception Unimplemented of string

  (* raised to report internal errors *)
    exception Impossible of string

  (* raised by searching operations *)
    exception NotFound

  (* raise the exception Fail with a standard format message. *)
    fun failure {module, func, msg} =
	  raise (Fail(concat[module, ".", func, ": ", msg]))

    val version = {
	    date = "June 1, 1996", 
	    system = "SML/NJ Library",
	    version_id = [1, 0]
	  }

    fun f ([], l) = l
      | f ([x : int], l) = (Int.toString x)::l
      | f (x::r, l) = (Int.toString x) :: "." :: f(r, l)

    val banner = concat (
	    #system version :: ", Version " ::
	    f (#version_id version, [", ", #date version]))

  end (* LibBase *);


signature RAND =
  sig

    val randMin : real
    val randMax : real
    val random : real -> real
      (* Given seed, return value randMin <= v <= randMax
       * Iteratively using the value returned by random as the
       * next seed to random will produce a sequence of pseudo-random
       * numbers.
       *)

    val mkRandom : real -> unit -> real
      (* Given seed, return function generating a sequence of
       * random numbers randMin <= v <= randMax
       *)

    val norm : real -> real
      (* r -> r / (randMax + 1.0) *)

    val range : (int * int) -> real -> int 
      (* Map v, randMin <= v <= randMax to integer range [i,j]
       * Exception -
       *   BadArg if j < i
       *)

  end (* RAND *);

structure Rand : RAND =
  struct

  (* real number version for systems with 46-bit mantissas *)
    val a = 48271.0  and  m = 2147483647.0

    val randMin = 1.0
    val randMax = m - 1.0

    fun random seed = let 
          val t = a*seed
          in
            t - m * real(floor(t/m))  
          end

    fun mkRandom seed = let
          val seed = ref seed
          in
            fn () => (seed := random (!seed); !seed)
          end

    fun norm r = r / m

    fun range (i,j) = 
          if j < i 
            then LibBase.failure{module="Random",func="range",msg="hi < lo"}
            else let 
              val R = real(j - i + 1)
              in
                fn r => i + floor(R*(r/m))
              end handle _ => let
                val ri = real i
                val R = (real j)-ri+1.0
                in
                  fn r => floor(ri + R*(r/m))
                end

  end (* Rand *);


Rand.random((Rand.randMax + Rand.randMin)/2.0);
Rand.random it;
Rand.random it;
