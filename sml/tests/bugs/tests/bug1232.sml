(* bug1232.sml *)

signature SIG1 =
sig
  type t = int
end

signature SIG2 =
sig
  structure N : SIG1
end

functor Bad(structure X : SIG1
            structure Y : SIG2 where N = X) =
struct end;
