(* 237.sml *)

functor f() = struct
  functor g(val x:int) = struct
    val y = x+x
  end
  structure a=g(val x=2);
end;
