(* bug906.sml *)
(* 906. subscript exception compiling sigs with type abbreviations *)

signature WIDGET =
sig
  datatype valign = VCenter  (* if this deleted, bug goes away *)
  type widget
end;

signature WIDGET_SET =
sig
  structure W : WIDGET
  type set_item = W.widget
end;
