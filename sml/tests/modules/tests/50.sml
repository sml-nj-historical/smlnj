(* test50.sml *)
(* keywords: functor, datatype, sibling *)

functor F() : sig type t val x : t end =
struct
   structure S =
   struct
     datatype a = A1 | A2 of a
     datatype b = B of a
   end
   type t = S.b
   val x = S.B(S.A2(S.A1))
end;
