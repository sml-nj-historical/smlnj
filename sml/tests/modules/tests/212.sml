signature SIG = 
  sig 
    structure S : sig type t end
    val f : S.t -> unit
  end

functor G () : SIG = 
  struct 
    structure S : sig type t end = struct type t = int end
    fun f (x : S.t) = ()
  end




