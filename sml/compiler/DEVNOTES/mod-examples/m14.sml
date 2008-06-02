(* This test uses curried functors and nested functors simultaneously 
 *)
functor F(X: sig type t val x : t end) 
	 (Y: sig type u val y : u end) = 
struct 
  functor G(Z: sig type v val z : v end) = struct end 
end;