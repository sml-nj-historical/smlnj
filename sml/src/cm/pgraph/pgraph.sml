(* pgraph.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * A list-of-edges representation of the dependency graph.
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure PortableGraph = struct

    type varname = string

    type namespace = string (* SIG | STR | FCT or more *)

    datatype rhs =
	SYM of namespace * string
      | SYMS of varname list
      | IMPORT of { lib: varname, syms: varname }
      | COMPILE of { src: string, env: varname, syms: varname, native: bool }
      | FILTER of { env: varname, syms: varname }
      | MERGE of varname list

    datatype def = DEF of { lhs: varname, rhs: rhs }

    datatype graph =
	GRAPH of { imports: varname list,
		   defs: def list,
		   export: varname }
end
