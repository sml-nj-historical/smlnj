(* test70.sml *)
(* tests printing of values of abstraction datatypes *)

signature S = 
  sig
    type s
    val x : s
    datatype d = D of s
    datatype e = E of d
  end;

structure A =
struct
  type s = string
  val x = "abc"
  datatype d = D of s
  datatype e = E of d
end;

structure B :> S = A;

B.E(B.D B.x);
