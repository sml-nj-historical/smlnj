functor Dict (type t) = 
  struct
    datatype dict = HT of t 
    type dict = dict     
  end

functor T(type t) =
  struct structure U = Dict(type t = t)
         datatype s = EE of U.dict 
         val x = EE
  end;

structure A = T(type t = int)

