(* bug1166.1.sml *)

functor F(type u) = 
struct
   structure A :> sig type t end where type t = u =
    struct
       type t=u
    end
end;

