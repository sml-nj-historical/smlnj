(* bug213.2.sml *)
(* Clausal function declaration goes off the trolley. *)

datatype 'cs $$$ = $ of 'cs | $$;

fun Fn1 ((c,p)::mtl) ({c = $$, p = $$}) =
  if (c,p) = (3,3)
  then
   { p = $ p,c = $ c}::(Fn1 mtl {c = $$, p = $$})
  else
   Fn1 mtl {c = $$, p = $$}
  | Fn1 ((c,_)::mtl) ({ c = $$ ,p = $ p}) =
    {p = $ p, c = $ c}::(Fn1 mtl {c = $$, p = $ p})
  | Fn1 ((_,p)::mtl) ({p = $$, c = $ c}) =
    {p = $ p, c = $ c}::(Fn1 mtl {p = $$, c = $ c})
  | Fn1 _  _ = [];
