(* This tests instantiate's ability to detect inconsistent defining type
 * sharing constraints. 
 *)
signature S0 =
sig
  type t = int
end
 
signature S1 =
sig
  type t = bool
end

signature S3 =
sig
  structure A : S0
  structure B : S1
  sharing A = B
end

functor F(X:S3) = struct end

