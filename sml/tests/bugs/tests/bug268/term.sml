(*--------------FILE:  term.sml-------------------------*)

import "term.sig";

functor termFC ():termsig =

struct

datatype term =
    Const of string
  | Var of string
  | Func of string * term list

end;
