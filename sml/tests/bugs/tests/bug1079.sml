(* bug1079.sml *)

structure foo =
struct
  datatype 'a group = GROUP of 'a list | SINGLE of 'a
end;
