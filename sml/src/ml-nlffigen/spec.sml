(*
 * spec.sml - A data structure describing the export interface of a
 *            C program.
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure Spec = struct

    datatype constness = RO | RW
    type tag = string

    datatype ctype =
	SCHAR | UCHAR | SINT | UINT | SSHORT | USHORT | SLONG | ULONG
      | FLOAT | DOUBLE | VOIDPTR
      | STRUCT of tag
      | UNION of tag
      | FPTR of cft
      | PTR of cobj
      | ARR of { t: ctype, d: int, esz: int }

    withtype cft = { args: ctype list, res: ctype option }

    and cobj = constness * ctype

    datatype fieldspec =
	OFIELD of { offset: int, spec: cobj, synthetic: bool }
      | SBF of { offset: int, constness: constness, bits: word, shift: word }
      | UBF of { offset: int, constness: constness, bits: word, shift: word }

    type field = { name: string, spec: fieldspec }

    type s =
	 { tag: tag, anon: bool, size: word, fields: field list }
    type u =
	 { tag: tag, anon: bool, size: word, largest: field, all: field list }

    type gvar = { name: string, spec: cobj }

    type gfun = { name: string, spec: cft }

    type gty = { name: string, spec: ctype }

    type spec = { structs: s list,
		  unions: u list,
		  gtys: gty list,
		  gvars: gvar list,
		  gfuns: gfun list }
end
