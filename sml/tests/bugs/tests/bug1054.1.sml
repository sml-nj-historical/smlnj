(* bug1054.1.sml *)

functor BugFun() =
struct
  datatype t1 = c1 of t2 * t3
  withtype t2 = t1 list
       and t3 = t2 list	(* this triggers the bug *)
end;
