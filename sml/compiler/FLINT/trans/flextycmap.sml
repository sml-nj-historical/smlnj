(* flextycmap.sml *)

(* A map from entVars to canonical FLEXTYC...maybe combine with
   EPMap in the future. *)
structure FlexTycMap = RedBlackMapFn (type ord_key = Stamps.stamp
				      val compare = Stamps.compare)


(* DBM: since the RedBlackMapFn just defines a map type constructor,
 * there is really no distinction between FlexTycMap and EPMap. They
 * both define equivalent polymorphic maps over stamps. *)