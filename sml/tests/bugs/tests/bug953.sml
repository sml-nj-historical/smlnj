(* bug953.sml *)

functor F(type t1)
         (type t2 sharing type t1=t2
                  sharing type t1 = int) =
struct end;
