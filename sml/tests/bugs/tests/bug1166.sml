(* bug1166.sml *)

functor WRef(type u) = 
struct
   structure A :> sig type t end where type t = u =
    struct
       type t=u
    end
end
