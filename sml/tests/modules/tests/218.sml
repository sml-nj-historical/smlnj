(* 218.sml *)

signature S =
sig
  type primop = int
end;

structure K =
struct 
  type primop = int
end

signature T =
sig
  structure L : S = K
end;
