(* bug964.sml *)

signature S =
sig
  type 'a T = 'a

  val f: 'a T -> unit
end;

(* either the functor or the structure (or both) will exercise the error. *)

functor F (structure L: S): S =
struct
  open L
end;

structure ST: S =
struct
  type 'a T = 'a
  fun f x = ()
end;

