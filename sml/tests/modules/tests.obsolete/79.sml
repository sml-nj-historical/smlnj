(* misleading error message *)

functor F(A : sig type t end) : sig type t sharing type A.t = int end =
  struct
     type t = A.t
  end

(* gives the message: different global type constructors in sharing: t = Initial.Integer.int,
   which is misleading *)
