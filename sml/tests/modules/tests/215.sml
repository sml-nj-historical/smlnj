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
  type u = P.X.u
  val y = P.X.y
end;

functor G (U: S3) : S3 =  (* ": S3" necessary *)
struct
  structure A = 
    let structure T =
	  struct
	    structure X =
	      struct
		datatype u = A of int
		val y = A 3
	      end
	  end
     in F(T)
    end
  type u = A.u
  val y = A.y
end;
