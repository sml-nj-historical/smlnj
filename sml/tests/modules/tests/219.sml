(* 219.sml *)
(* works ok because int has fixed stamp *)

signature S =
sig
  type primop  = int
end;

functor F() =
struct
  structure K =
  struct 
    type primop = int
  end
  structure M : sig structure L : S = K end =
  struct
    structure L = K
  end
end;
