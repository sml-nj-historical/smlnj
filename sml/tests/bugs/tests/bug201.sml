(* bug.201 *)

structure A = struct datatype foo = Bar of int end;

structure B : sig datatype foo' = Bar of int end =
struct
  open A
  type foo' = foo
end;
