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
      | STABLELIB

    (* the "required" field includes everything:
     *   1. privileges required by subgroups
     *   2. newly required privileges
     *   3. privileges that would be wrapped once the group is stabilized
     *)
    datatype group =
	GROUP of { exports: DependencyGraph.impexp SymbolMap.map,
		   kind: kind,
		   required: privileges,
		   grouppath: SrcPath.t,
		   sublibs: group list }
end
