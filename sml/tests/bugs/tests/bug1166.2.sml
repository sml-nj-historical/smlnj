(* bug1166.2.sml *)

functor F() = 
struct
  datatype u = U
  structure A :> sig type t end where type t = u =
  struct
    type t=u
  end
end;

