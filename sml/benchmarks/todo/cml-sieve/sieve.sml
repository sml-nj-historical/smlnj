(* sieve.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A simple stream-style program to generate primes.
 *)

structure Main : BMARK =
  struct
    open CML

    fun counter ch = let
	  fun count i = (send(ch, i); count(i+1))
	  in
	    count 2
	  end

    fun filter (p, inCh, outCh) = let
	  fun loop () = let
		val i = accept inCh
		in
		  if ((i mod p) <> 0) then send (outCh, i) else ();
		  loop ()
		end
	  in
	    spawn (loop)
	  end

    fun sieve () = let
          val primes = channel ()
          val ch = channel ()
          fun loop ch = let
	        val p = accept ch
	        val ch' = channel ()
	        in
	          send (primes, p);
	          filter (p, ch, ch');
	          loop ch'
	        end
          in
	    spawn (fn () => (counter ch));
	    spawn (fn () => (loop ch));
	    primes
          end

    fun primes pr n = let
	  val ch = sieve()
          fun loop 0 = ()
	    | loop i = (
		pr (makestring(accept ch) ^ "\n");
		loop(i-1))
          in
	    loop n
          end

    fun nthPrime n = let
          val ch = sieve ()
          fun loop 1 = (accept ch)
	    | loop i = (accept ch; loop(i-1))
          in
	    loop n
          end

    fun testit outstrm =
	  RunCML.doit((fn () => primes (outputc outstrm) 1000), SOME 40)
    (*fun doit () = RunCML.doit(fn () => (nthPrime 1000; RunCML.shutdown()), SOME 40)*)
    fun doit () = RunCML.doit(fn () => (nthPrime 1000; RunCML.shutdown()), NONE)

  end (* Main *)

