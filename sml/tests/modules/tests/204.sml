functor Dict (type t) : sig type dict
                        end
= struct
    datatype dict = HT of t | NIL
    type dict = dict     (* removing this line, the bug goes away *)
  end

functor T(type t) =
  struct structure U = Dict(type t = t)
         datatype s = EE of U.dict | FF
         val x = FF
  end;

structure A = T(type t = int)

