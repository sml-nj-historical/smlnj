(* hidden type constructors can occur and are ok. *)

functor F() =
struct
  datatype s = S
  val x = S
  type s = int
end;

structure S = F()
