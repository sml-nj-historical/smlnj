(* bug 311 *)
abstype t = C1 | C2
with
    fun f (x:t, y:t) = x=y
end;
