(* bug728.sml *)
(* 728. opening a structure with substructure of same name at top level *)

structure A :
  sig
    structure A : sig end
    structure B : sig end
  end =
struct
  structure A = struct end
  structure B = struct end
end;

open A;  (* at top level *)
