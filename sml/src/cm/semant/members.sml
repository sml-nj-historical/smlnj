(* just a placeholder so far *)

(*
 * Collections of members in CM descriptions.
 *   Involves:
 *     - running tools
 *     - fully analyzing sub-groups and sub-libraries
 *     - parsing ML files and getting their export lists
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature MEMBERCOLLECTION = sig

    type symbol = GenericVC.Symbol.symbol
    type smlinfo = DependencyGraph.smlinfo

    exception DuplicateImport of symbol * smlinfo * smlinfo
    exception DuplicateDefinition of symbol * smlinfo * smlinfo

    type collection

    val expandOne : AbsPath.t * string option -> collection
    val sequential : collection * collection -> collection

    val num_look : collection -> string -> int
    val ml_look : collection -> GenericVC.Symbol.symbol -> bool
    val cm_look : collection -> string -> bool
end

structure MemberCollection :> MEMBERCOLLECTION = struct

    structure DG = DependencyGraph

    type smlinfo = DG.smlinfo
    type symbol = GenericVC.Symbol.symbol

    exception DuplicateImport of symbol * smlinfo * smlinfo
    exception DuplicateDefinition of symbol * smlinfo * smlinfo

    datatype collection =
	COLLECTION of { subexports: DG.farnode SymbolMap.map,
		        smlfiles: smlinfo list,
			localdefs: smlinfo SymbolMap.map }

    fun expandOne (f: AbsPath.t, c: string option) = raise Fail "notyet"

    fun sequential (COLLECTION c1, COLLECTION c2) = let
	fun se_error (s, (_, DG.NODE n1), (_, DG.NODE n2)) =
	    raise DuplicateImport (s, #smlinfo n1, #smlinfo n2)
	fun ld_error (s, f1, f2) = raise DuplicateDefinition (s, f1, f2)
	val se_union = SymbolMap.unionWithi se_error
	val ld_union = SymbolMap.unionWithi ld_error
    in
	COLLECTION { subexports = se_union (#subexports c1, #subexports c2),
		     smlfiles = #smlfiles c1 @ #smlfiles c2,
		     localdefs = ld_union (#localdefs c1, #localdefs c2) }
    end

    fun num_look (c: collection) (s: string) = 0

    fun cm_look (c: collection) (s: string) = false

    fun ml_look (COLLECTION { subexports, localdefs, ... }) s =
	isSome (SymbolMap.find (subexports, s)) orelse
	isSome (SymbolMap.find (localdefs, s))
end
