(* bug1234.2.sml *)

functor F(type t val x : t) =
struct 
  structure U = 
    struct
      datatype u = K of t 
      type u = u list
      val y = [K x]
    end
  val z = hd U.y
end;

structure A = F(type t = int val x = 3);

