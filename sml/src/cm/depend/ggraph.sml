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
    type privilegespec = { required : privileges, granted : privileges }

    datatype group =
	GROUP of { exports: DependencyGraph.impexp SymbolMap.map,
		   islib: bool,
		   privileges: privilegespec,
		   grouppath: AbsPath.t,
		   subgroups: group list,
		   stableinfo: BinInfo.info IntBinaryMap.map option }
end
