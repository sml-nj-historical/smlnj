(* bug1093.sml *)

structure S =
struct
  abstype t = A with type u = t end
end;
