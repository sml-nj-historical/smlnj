(* bug 130, compiler bug *)

structure S =
struct
  datatype 'a T =  C
  fun fs(x: 'a T) = x
end

functor F(type t) =
struct
  open S
  type FT = t T
  fun ff(x : FT) = fs x
end

structure SF = F(type t=int)
