(* bug834.sml *)
(* 834. accepts duplicate names in funsig parameters *)

funsig S1 (T:sig end) (T:sig type t end) = sig val x:T.t end

functor F1(T:sig  end) (T:sig type t end) = struct type t = T.t end
