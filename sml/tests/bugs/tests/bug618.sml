(* bug618.sml *)

signature S0 =
sig
  exception Error of string  (* string arg necessary *)
end

functor F (X : S0) =
struct
  open X
  fun extend_one (i,v,r) j = if i = j then v else (r j) (* necessary *)
  fun extend_env _ = raise Error "foo"
end

structure A : S0 =
struct
  exception Error of string
end

structure B = F(A)


