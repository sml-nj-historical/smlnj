(* bug1166.3.sml *)

functor F(type u) = 
struct
   structure A :> sig type t = u end =
    struct
       type t=u
    end
end;

