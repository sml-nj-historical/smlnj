  (* Given a seed, mkRandom returns a psuedo-random number generator.
     This generator is a function whose argument is an integer greater than
     the greatest return value desired.
     
     (Linear Congruential,
      after Sedgewick,"Algorithms", Addison-Wesley, 1983, Chapter 3 pp 37-38.)
   *)

structure Random = 
    struct
	fun mkRandom seed =
	    let val m = 100000000
		val m1 = 10000
		val b = 31415821
		val a = ref seed
		fun mult (p,q) = 
		    let val p1 = p div m1
			val p0 = p mod m1
			val q1 = q div m1
			val q0 = q mod m1
		    in
			(((p0 * q1 + p1 * q0) mod m1) * m1 + p0 * q0) mod m
		    end
	    in
		(fn r => (a := (mult(!a,b) + 1) mod m; 
			  ((!a div m1) * r) div m1))
	    end
    end     
