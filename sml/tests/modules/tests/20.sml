(* test20.sml *)
(* keywords: structure, let *)

structure A =
let type t = int * int
 in struct
      val f = (fn ((x,y):t) => x)
    end
end;
