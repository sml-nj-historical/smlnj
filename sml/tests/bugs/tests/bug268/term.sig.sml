(*-----------------FILE:  term.sig.sml----------------------*)

signature termsig =
sig

datatype term =
    Const of string
  | Var of string
  | Func of string * term list

end;
