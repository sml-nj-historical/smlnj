(* bug1432.7.sml *)

signature S1 =
sig
  datatype d = D
end;

signature S2 =
sig
  datatype d = D
  structure A :
    sig
      datatype d = D
      type s
      sharing type s = d
    end
    where type s = d  (* definition outside of sharing *)
end;

functor F (X : S2) =
struct
  structure MyS1 : S1 = X.A (* match against S1 necessary *)
end;
