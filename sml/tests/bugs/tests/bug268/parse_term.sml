(*------------FILE: parse_term.sml---------------------------*)

import "term.sig";

functor parse_termFC (structure TERM:termsig) =
struct

open TERM

fun term_nil x = (x:term list) = []

end;
