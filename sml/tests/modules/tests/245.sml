signature SIG = sig type t
                    val v : t
                end

functor MapIndex (X : sig structure F: SIG end) =
  struct
    type A = X.F.t
  end

functor MapSquare (G : SIG) = 
struct 
 functor H(type u) : SIG = 
  struct
    structure S = 
      let structure T = 
            struct structure F = G end 
       in MapIndex(T)
          (* let structure X : sig structure F: SIG end = T
              in struct type A = X.F.t end
             end
           *)
      end
    type t = G.t
    val v = G.v
  end
end

