(* test exception declarations, constructors in patterns *)

structure S =
struct
exception e;
datatype d = D;
val x = D;

exception e' = D;   (* incorrect *)
exception e' = x;   (* incorrect *)

structure A = struct datatype d = D val f = fn _ => () exception e end;

exception e' = A.D;
val _ = fn A.f _ => ()    (* incorrect *)

val e' = A.e;     (* ok *)
exception e' = A.D;     (* incorrect *)
exception e' = A.f;     (* incorrect *)
end

