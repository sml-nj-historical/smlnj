structure DependencyGraph = struct

    type smlinfo = unit
    type groupinfo = unit
    type primitive = unit

    type filter = SymbolSet.set option

    datatype node =
	PNODE of primitive
      | NODE of { smlinfo: smlinfo,
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

end
