(* bug1360.2.sml *)
(* another version from Stephen Weeks (3/27/98, x86-linux) *)

val i = fn _ => 13;
val f : (unit -> 'a) * ('a -> int) -> int = fn _ => 14;

functor G(type t) =
struct
  val _ = f(fn () => (raise Bind) : t -> t,
	    i)
end;

