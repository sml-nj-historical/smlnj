(* out of order instantiation problem *)

functor F(type t1
          type t2
          datatype s = TOKEN of t2          
          sharing type t1 = s) =
struct end;

