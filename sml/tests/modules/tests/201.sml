functor Dict (X : sig type s end) = 
  struct
    datatype dict = HT of X.s
  end;

functor T() =
  struct structure U = Dict(struct type s = int end)
         val x = U.HT
  end;

structure S = T()

