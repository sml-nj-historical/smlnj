(* bug1078.sml *)

functor Bug() =
struct
  datatype foo = Foo
  withtype bar = int
  and baz = bar list
end;

structure Bug = Bug();
