(* bug1234.sml *)

functor F(type t val x : t) =
struct 
  structure U : sig type u val y : u end = 
    struct
      local
        datatype u' = K of t 
      in 
        type u = u' list
        val y = [K x]
      end
    end
  val z = hd U.y
end;

structure A = F(type t = int val x = 3);

