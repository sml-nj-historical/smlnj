(* pp-desc.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * A tree representation of pretty printing directives.
 *)

structure PPDesc =
  struct

    datatype indent
      = Abs of int		(* indent relative to outer indentation *)
      | Rel of int		(* indent relative to start of box *)

  (* The pp_desc type is parameterized over the token, style, and device
   * types.
   *)
    datatype ('tok, 'sty, 'dev) pp_desc
      = HBox of ('tok, 'sty, 'dev) pp_desc list
      | VBox of (indent * ('tok, 'sty, 'dev) pp_desc list)
      | HVBox of (indent * ('tok, 'sty, 'dev) pp_desc list)
      | HOVBox of (indent * ('tok, 'sty, 'dev) pp_desc list)
      | Box of (indent * ('tok, 'sty, 'dev) pp_desc list)
      | Token of 'tok
      | String of string
      | Style of ('sty * ('tok, 'sty, 'dev) pp_desc list)
      | Break of {nsp : int, offset : int}
      | NewLine
      | NBSpace of int
      | Control of ('dev -> unit)

  end;

