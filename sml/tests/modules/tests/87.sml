(* check env. lookup of values in substructures when processing signatures *)
signature S =
  sig
    structure A : 
	sig
	   datatype e = E
       end
    datatype d = D
    val a : A.e
    val b : d
  end
