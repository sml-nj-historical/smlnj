(* bug60a.sml *)

val start_seed1 = 0.71573298;
val start_seed2 = 0.31872973;
val start_seed3 = 0.45832123;

val mul1 = 147.0;
val mul2 = 375.0;
val mul3 = 13.0;

fun random seed mul =
    let val x = seed*mul*3.0
     in x - real(floor x)
    end;

fun randlist seed1 seed2 seed3 0 = []
  | randlist seed1 seed2 seed3 n =
      let val s1 = random seed1 mul1
	  val s2 = random seed2 mul2
	  val s3 = random seed3 mul3
	  val rn = (floor ((random (s1*s2*s3) 743.0)*37.0) )
       in rn::(randlist s1 s2 s3 (n-1))
      end;

fun rlist n = randlist start_seed1 start_seed2 start_seed3 n;

rlist 300;
rlist 300;
rlist 300;
rlist 300;
