(* bug 129 -- Bind exception *)

signature S_sig=
sig 
  type 'a T
  val fs: 'a T -> 'a T
end;

functor S() =
struct
  datatype 'a T =  C
  fun fs  (x: 'a T )=  C: 'a T
end;

functor F (type t)=
struct
  structure S1: S_sig= S();
  open S1
  type  FT = t T
  fun ff (x : FT)=  fs x
end;
