(* bug619.sml *)

signature SIG =
sig
  exception Error of string
end

functor F (X : SIG) =
struct
  open X

  datatype Exp = APP of Exp * (Exp list)
  datatype Val = FUNC of Val list -> (Val -> unit) -> unit

  fun extend_one (i,v,r) j = if i = j then v else (r j)
  fun extend_env([],[],r) = r
    | extend_env(i::is,v::vs,r) = extend_env(is,vs,extend_one(i,v,r))
    | extend_env _ = raise Error "mismatching environment extension"

  fun  meaning (APP(e,es)) r k =
	meaning e r (fn (FUNC f) => meaninglis es r (fn vs => f vs k))

  and meaninglis [] r k = k []
    | meaninglis (e :: es) r k =
	meaning e r (fn v => meaninglis es r (fn vs => k (v :: vs)))
end

structure A : SIG =
struct
  exception Error of string
end

structure B = F(A)  (* necessary *)

