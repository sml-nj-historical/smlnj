(* univariate-stats.sml
 *
 *   Some statistical functions on unweighted univariate samples.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure UnivariateStats :> sig

    (* We distinguish between two kinds of samples.  Only the "heavy"
     * kind permits calculation of average deviation and median.
     * It is also considerably more expensive because it keeps an
     * array of all points while the "light" variety is constant-size. *)
    type light type heavy

    type 'a sample		(* light or heavy *)
    type 'a evaluation		(* light or heavy *)

    (* Samples are built incrementally by adding points to an initially
     * empty sample: *)
    val lempty :         light sample
    val hempty : unit -> heavy sample
    val ladd   : real * light sample -> light sample (* constant *)
    val hadd   : real * heavy sample -> heavy sample (* amortized constant *)

    (* Evaluate the sample; this completes all the expensive work except
     * for things that depend on "heavy" samples: *)
    val evaluate : 'a sample -> 'a evaluation (* constant *)

    (* extracting of "cheap" information (constant-time): *)
    val N                 : 'a evaluation -> int
    val n                 : 'a evaluation -> real (* N as real *)
    val mean              : 'a evaluation -> real
    val variance          : 'a evaluation -> real
    val standardDeviation : 'a evaluation -> real
    val skew              : 'a evaluation -> real
    val kurtosis          : 'a evaluation -> real

    (* extracting of "expensive" information: *)
    val median            : heavy evaluation -> real (* randomized linear *)
    val averageDeviation  : heavy evaluation -> real (* linear *)

end = struct

    infix 8 $  val op $ = Unsafe.Array.sub
    infix 3 <- fun (a, i) <- x = Unsafe.Array.update (a, i, x)

    type light = unit
    type heavy = real array * int
    type 'a sample = ('a * int * real * real * real * real)
    type 'a evaluation = ('a * int * real * real * real * real * real * real)

    fun insert (x, n, (a, sz)) =
	let val (a, sz) =
		if n<sz then (a, sz)
		else let val sz = sz+sz
			 val b=Array.tabulate(sz,fn i=>if i<n then a$i else 0.0)
		     in (b, sz) end
	in (a,n)<-x; (a, sz) end

    val SZ = 1024		(* minimum allocated size of heavy array *)
    val lempty = ((), 0, 0.0, 0.0, 0.0, 0.0)
    fun hempty () = ((Array.array (SZ, 0.0), SZ), 0, 0.0, 0.0, 0.0, 0.0)

    fun ladd (x:real, ((), n, sx4, sx3, sx2, sx1)) =
	let val x2 = x*x val (x3, x4) = (x2*x, x2*x2)
	in ((), n+1, sx4+x4, sx3+x3, sx2+x2, sx1+x) end

    fun hadd (x:real, ((a, sz), n, sx4, sx3, sx2, sx1)) =
	let val x2 = x*x val (x3, x4) = (x2*x, x2*x2)
	    val (a, sz) =
		if n < sz then (a, sz)
		else let val sz = sz+sz
			 val b = Array.tabulate
				     (sz, fn i => if i<n then a$i else 0.0)
		     in (b, sz) end
	in (a,n)<-x;
	   ((a, sz), n+1, sx4+x4, sx3+x3, sx2+x2, sx1+x)
	end

    fun evaluate (state, ni, sx4, sx3, sx2, sx1) =
	let val n = real ni         val n' = n - 1.0
	    val m = sx1/n           val m2 = m*m         val m3 = m2*m
	    val sd2 = (sx2 + m*(n*m-2.0*sx1))/n'
	    val sd = Math.sqrt sd2  val (sd3, sd4) = (sd*sd2, sd2*sd2)
	    val sk = (sx3-m*(3.0*(sx2-sx1*m)+n*m2))/(n*sd3)
	    val k = ((sx4+m*(6.0*sx2*m-4.0*(sx3+sx1*m2)+n*m3))/(n*sd4))-3.0
	in (state, ni, n, m, sd2, sd, sk, k) end

    fun N (state, ni, nr, m, sd2, sd, sk, k) = ni
    fun n (state, ni, nr, m, sd2, sd, sk, k) = nr
    fun mean (state, ni, nr, m, sd2, sd, sk, k) = m
    fun variance (state, ni, nr, m, sd2, sd, sk, k) = sd2
    fun standardDeviation (state, ni, nr, m, sd2, sd, sk, k) = sd
    fun skew (state, ni, nr, m, sd2, sd, sk, k) = sk
    fun kurtosis (state, ni, nr, m, sd2, sd, sk, k) = k

    fun median ((a, sz), ni, nr, m, sd2, sd, sk, k) =
	RealOrderStats.median' (ArraySlice.slice (a, 0, SOME ni))

    fun averageDeviation ((a, sz), ni, nr, m, sd2, sd, sk, k) =
	let fun ad (i, ds) = if i>ni then ds/nr else ad (i+1, ds + abs(a$i-m))
	in ad (0, 0.0) end
end
