(* bug 317 *)
abstype abs = X of int 
with
  datatype rep = Y of abs;
  val foo = X 1
end;

fun eq x y = Y x = Y y;
eq foo foo;
