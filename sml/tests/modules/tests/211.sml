functor G () =
  struct 
    structure S : sig type t end = struct type t = int end
    structure T : sig val f : S.t -> unit end = 
      struct fun f (x : S.t) = ()
      end
  end




