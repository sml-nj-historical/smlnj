signature S2 =
sig
  structure A : sig type s end
  val x : A.s
end;

signature S3 =
sig
  type u
  val y : u
end;

functor F (P: sig structure X: S3 end) =
struct
  type s = P.X.u
end;

functor G (U: S3) (V: sig end) : S2 =  (* V, ": S2" necessary *)
struct
  structure A = 
    let structure T = struct structure X = U end
     in F(T)
    end
  val x = U.y  (* : ev[A].ev[T].ev[X].ev[u] *)
end;

