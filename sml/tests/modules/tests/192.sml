(* empty base environment in instToStr.mkEntEnv *)
(* derived from MLRISC/mlrisc/ra.sml *)

signature S =
sig
  type t    
end;

signature SA =
sig
  structure A : S
end;

functor F
  (structure X : S) 
  (structure Y : SA 
    where type A.t = X.t) =
struct
end;
