signature PS =
sig
  type reader
  val x : reader
end

functor PF () : PS =
struct
  type reader = int * int
  val x = (1,2)
end

structure B = PF()

signature S =
sig
  val mkReader : B.reader
end

structure P : S = 
struct
  val mkReader = B.x
end


