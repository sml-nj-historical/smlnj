(* bug989.2.sml *)
(* 989. Compiler bug: Instantiate:explore_tclass.5 *)

functor ParseFun(type Arg
		 type Result
		 val parser: string -> Arg -> Result
		 sharing type Arg = unit	(*1*)
		) =
struct
  fun parse file = parser file ()		(*2*)
end;

