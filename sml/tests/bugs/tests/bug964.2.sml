(* bug964.2.sml *)

structure S : sig type 'a t = 'a end = struct type 'a t = 'a end;
