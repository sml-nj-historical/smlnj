(* bug447.sml *)

structure S =
struct
  type 'b data = 'b list
  type 'b value = 'b
  fun at(x:'b data):'b value = hd x
end;
