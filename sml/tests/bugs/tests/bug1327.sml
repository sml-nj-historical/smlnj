(* bug1327.sml *)

signature S1 =
sig
  datatype t = U               
end;

signature S2 =
sig
  structure A : S1
  datatype u = U of A.t
end;

signature S3 =
sig 
  structure B : S2
end;

functor F1 (structure X : S1
	    structure Y : S2 where A = X)
    :> S3 where B = Y
 =
struct
  structure B = Y
end;

functor F2 () =
struct
  structure D = F1(structure X = struct datatype t = U end
		   structure Y = Z)
end;
