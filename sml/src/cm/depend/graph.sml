(*
 * Internal data structure representing a CM dependency graph.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure DependencyGraph = struct

    type symbol = Symbol.symbol
    type groupinfo = Dummy.t
    type primitive = Primitive.primitive

    type filter = SymbolSet.set option

    datatype node =
	PNODE of primitive
      | NODE of { smlinfo: SmlInfo.info,
		  localimports: node list,
		  globalimports: farnode list }

    withtype farnode = filter * node

    (* the filter is duplicated in each member of the map to
     * make it easier to build the global graph *)
    datatype gnode =
	GNODE of { groupinfo: groupinfo,
		   imports: gnode list,
		   filter: filter,
		   exports: farnode SymbolMap.map,
		   dangling: node list }

    (* environments used for dependency analysis *)
    datatype env =
	EMPTY
      | FCTENV of { looker: symbol -> value option,
		    domain: unit -> SymbolSet.set }
      | BINDING of symbol * value
      | LAYER of env * env

    withtype value = env

    fun describeNode (PNODE p) = Primitive.toString p
      | describeNode (NODE { smlinfo, ... }) = SmlInfo.describe smlinfo
end
