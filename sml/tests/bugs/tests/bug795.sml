(* bug795.sml *)
(* 795. Compiler bug: elabDecl:open:FctBodyStr *)

functor Fun (Str: sig end) =
  let open Str
   in struct end
  end;
