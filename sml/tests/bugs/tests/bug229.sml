(* bug229.sml *)

signature SS = sig type t1 val t2:t1 end;

structure ss:SS =
struct
  local datatype t = T of int
     in val t2 = T 3
    end
end;
