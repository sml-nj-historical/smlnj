(* bug1243.sml *)

local
   functor Foo(type 'a t
	       val destruct : 'a t -> ('a * 'a t) option) =
      struct
	 fun first s =
	    case destruct s of
	       SOME(x,_) => x
      end
   
   structure Foo1 = 
      Foo(type 'a t = 'a list
	  fun destruct _ = let exception E in raise E end)
      
   structure Foo2 =
      Foo(datatype 'a t = T of ('a * 'a t) option
	  fun destruct _ = let exception E in raise E end)
      
in
   val _ = 13
end;
