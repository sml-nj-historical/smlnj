structure bhMath = 
    struct
	type 'a vec = 'a list

	fun getx [x,_,_] = x
	fun gety [_,y,_] = y
	fun getz [_,_,z] = z

	fun mkvs s = [s,s,s]

	fun arithv _ [] [] = []
	  | arithv (f:(real * real)->real) (u::us) (v::vs) = 
	        (f (u,v)) :: arithv f us vs

	val subv = arithv (op -)
	val addv = arithv (op +)
	val mulv = arithv (op * )
	val divv = arithv (op /)

	fun dotvp v u = fold (op +) (mulv v u) 0.0

	fun arithvs _ [] _ = []
	  | arithvs (f:(real * real)->real) (v::vs) s = 
	        (f (v,s))::arithvs f vs s
	
	val subvs = arithvs (op -)
	val addvs = arithvs (op +)
	val mulvs = arithvs (op * )
	val divvs = arithvs (op /)

	fun even x = (2 * (x div 2)) = x
	fun odd x = not (even x)

	fun pow 0.0 _ = 0.0
	  | pow x y = exp (y * ln x)

	val rand = Random.mkRandom 0

	fun xrand (l:real,h:real) = 
	    let val expand = 100000
		val r = (real (rand expand)) / (real (expand+1))
	    in
		l + (h - l)*r
	    end

	fun randvec 0 _ = []
	  | randvec n r = (xrand r) :: randvec (n-1) r

	val randvec = randvec 3
	val realzerovec = mkvs 0.0
	val intzerovec = mkvs 0

	fun mapvec f [] = []
	  | mapvec f (v::vs) = (f v)::mapvec f vs

	fun mapvec2 f [] [] = []
	  | mapvec2 f (u::us) (v::vs) = (f (u,v))::mapvec2 f us vs

	fun mapvec3 f [] [] [] = []
	  | mapvec3 f (u::us) (v::vs) (w::ws) = (f (u,v,w))::mapvec3 f us vs ws
		      
	fun realprvec [x:real,y:real,z:real] = 
	        (print "<";
		 print x;
		 print " ";
		 print y;
		 print " ";
		 print z;
		 print ">\n")
	   
	val PI = 3.14159265358979323846
	val TWO_PI = 6.28318530717958647693
	val FOUR_PI = 12.56637061435917295385
	val HALF_PI = 1.57079632679489661923
	val FRTHRD_PI = 4.18879020478639098462
    end
