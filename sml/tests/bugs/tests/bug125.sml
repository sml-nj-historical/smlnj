(* bug125.sml *)

abstype
  intset = Set of int list
with
  val empty_set = Set [];
end;

Set;
