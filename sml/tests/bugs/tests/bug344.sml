(* bug344.sml *)
structure C : sig val f : 'a -> 'a end =
struct

  exception E
  fun foo(k:'a option -> 'a) : 'a = k(NONE)
  fun bar(x:'a option) (y: 'a) = raise E

  fun f (e) =  (* using (e:'a) as argument works *)
      foo (fn l => let fun g (x : 'a) = bar l x
		    in g e
		   end)

end
