(* bug270.sml *)
(* Problem: Compiler bug: TypesUtil.lookTycPath: NULLstr 
   failure to interpret path for X.d in embedded signature *)

functor F(X: sig datatype d = A end) =
  struct
    structure S : sig val x : X.d end =
	struct val x = X.A end
  end
