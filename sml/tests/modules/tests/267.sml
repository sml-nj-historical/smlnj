signature SP =
sig
  datatype reader = Rd
end

functor PF () : SP =
struct
  datatype reader = Rd
end

structure B = PF()

signature S =
sig
  val mkReader : B.reader
end

structure P : S = 
struct
  val mkReader = B.Rd
end


