(*
 * Internal data structure representing a CM dependency graph.
 * (coarse-grain: groups)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure GroupGraph = struct

    type privileges = StringSet.set

    datatype kind =
	NOLIB
      | LIB of privileges		(* wrapped privileges *)
      | STABLELIB of DependencyGraph.bnode IntBinaryMap.map
                                        (* stable library with bnodo map *)

    (* the "required" field includes everything:
     *   1. privileges required by subgroups
     *   2. newly required privileges
     *   3. privileges that would be wrapped once the group is stabilized
     *
     * The list of sub-libraries includes an AbsPath.t.  This is the path
     * that was originally found in the CM description file and led to
     * inclusion of the sublibrary.  This contrasts
     * with the "grouppath" member of the sublibrary itself which
     * records the path of its actual description file.
     * The two paths are not necessarily equal because of aliases. *)
    datatype group =
	GROUP of { exports: DependencyGraph.impexp SymbolMap.map,
		   kind: kind,
		   required: privileges,
		   grouppath: AbsPath.t,
		   sublibs: (AbsPath.t * group) list }
end
