(* test ability to reference elements of structures that have been lexically
   hidden *)

signature S1 =
sig
  type t
end

signature S2 =
sig
  structure A : S1
  open A
  structure B : 
     sig
       structure A : sig type t end
       val x : t   (* printing is incorrect, but we'll live with it *)
       sharing type t = int
     end
end
