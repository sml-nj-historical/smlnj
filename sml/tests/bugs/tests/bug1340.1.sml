(* bug1340.1.sml *)

structure Segfault =
struct
  open Word32

  fun F (x, y, z) = orb(andb(x, y), andb(notb x, z))

  fun <<< (x, s) =
      let val rs = Word31.-(0w32, s)
      in 
	  orb(<<(x, s), >>(x, rs))
      end
  infix 5 <<<
  val sub = Vector.sub
  infix 8 sub

  fun hash (X, (a, b, c, d)) =
	   (a + F(b, c, d) + (X sub 0)) <<< 0w3

end;
