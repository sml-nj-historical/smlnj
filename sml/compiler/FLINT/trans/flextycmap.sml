
(* A map from entVars to canonical FLEXTYC...maybe combine with
   EPMap in the future. *)
structure FlexTycMap = RedBlackMapFn (type ord_key = Stamps.stamp
				      val compare = Stamps.compare)