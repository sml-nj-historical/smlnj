(* bug1191.1.sml *)

structure S =
struct
  datatype d = D | E  (* d must have more than one constructor *)
  and h = H of d
end;

structure A =
struct
  datatype h = datatype S.h
end

fun f (S.H x) = true;
fun g (A.H x) = true | g _ = false;
fun g1 (A.H x) = true; 
fun h (A.H x) = true | h (S.H x) = false;

val a = S.H(S.D);
val b = A.H(S.D);
val f_a = f a;
val g_a = g a;
val h_a = h a;
val f_b = f b;
val g_b = g b;
val h_b = h b;
val g1_a = g1 a;
val g1_b = g1 b;
