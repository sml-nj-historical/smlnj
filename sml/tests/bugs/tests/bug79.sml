(* bug79.sml
79. withtype
*)

datatype type1 = T of type2 * type3
withtype type2 = int (* this could be a large expression *)
and      type3 = type2 * string;
