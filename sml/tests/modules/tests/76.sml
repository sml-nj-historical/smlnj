(* this shows that we have to relativize a structure in a functor
   body even if only the *signature* is "generated" by the functor
   body *)

structure S = struct val x = nil end;

functor F(A : sig type t end) =
   struct
     structure B : sig val x : A.t list end = S
  end

structure A=F(struct type t=int end);

structure S =
struct
   val x = A.B.x : int list;
end 

    
