(* bug1251.sml *)

structure S : sig
                structure A : sig end
                structure B : sig structure C : sig end end where C = A
              end =
struct
  structure A = struct end
  structure B = struct structure C = A end
end;

