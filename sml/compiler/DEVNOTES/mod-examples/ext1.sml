functor F (A : sig
    eqtype t
    type elem
    val sub      : t -> elem
  end) 
 =
struct
  val sub = A.sub 
end (* DynamicArrayFn *)
