signature SIG = sig type t
                    val v : t
                end

functor MapIndex (F: SIG) =
  struct
    type t = F.t
    val v = F.v
  end

functor H(type u) : SIG = 
  struct
    structure S = let structure T = struct datatype t = A of int val v = A 3 
                                    end
                   in MapIndex(T)
                  end
    type t = S.t
    val v = S.v
  end

