(* bug1206.2.sml *)

functor T(type t) =
struct 
  structure U = 
    struct
      datatype u = HT of t 
      type u = u      (* removing this line, the bug goes away *)
    end
  datatype s = EE of U.u 
  val x = fn (_: s) => 0
end;

structure A = T(type t = int);

