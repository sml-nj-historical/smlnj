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
    type smlinfo = SmlInfo.info

    type collection

    val empty : collection

    val expandOne : (AbsPath.t -> DependencyGraph.farnode SymbolMap.map)
	-> { sourcepath: AbsPath.t, group: AbsPath.t, class: string option,
	     error : string -> unit }
	-> collection
    val sequential : collection * collection * (string -> unit) -> collection

    val num_look : collection -> string -> int
    val ml_look : collection -> GenericVC.Symbol.symbol -> bool
    val cm_look : collection -> string -> bool
end

structure MemberCollection :> MEMBERCOLLECTION = struct

    structure DG = DependencyGraph
    structure Symbol = GenericVC.Symbol

    type smlinfo = SmlInfo.info
    type symbol = Symbol.symbol

    datatype collection =
	COLLECTION of { subexports: DG.farnode SymbolMap.map,
		        smlfiles: smlinfo list,
			localdefs: smlinfo SymbolMap.map }

    val empty =
	COLLECTION { subexports = SymbolMap.empty,
		     smlfiles = [],
		     localdefs = SymbolMap.empty }

    fun sequential (COLLECTION c1, COLLECTION c2, error) = let
	fun describeSymbol (s, r) = let
	    val ns = Symbol.nameSpace s
	in
	    Symbol.nameSpaceToString ns :: " " :: Symbol.name s :: r
	end
	fun se_error (s, x as (_, n1), (_, n2)) =
	    (error (concat (describeSymbol
			    (s, [" imported from ", DG.describeNode n1,
				 " and also from ", DG.describeNode n2])));
	     x)
	val se_union = SymbolMap.unionWithi se_error
	fun ld_error (s, f1, f2) =
	    (error (concat (describeSymbol
			    (s, [" defined in ", SmlInfo.describe f1,
				 " and also in ", SmlInfo.describe f2])));
	     f1)
	val ld_union = SymbolMap.unionWithi ld_error
    in
	COLLECTION { subexports = se_union (#subexports c1, #subexports c2),
		     smlfiles = #smlfiles c1 @ #smlfiles c2,
		     localdefs = ld_union (#localdefs c1, #localdefs c2) }
    end

    fun expandOne gexports { sourcepath, group, class, error } = let
	fun noPrimitive () = let
	    val expansions = PrivateTools.expand error (sourcepath, class)
	    fun exp2coll (PrivateTools.GROUP p) =
		COLLECTION { subexports = gexports p,
			     smlfiles = [],
			     localdefs = SymbolMap.empty }
	      | exp2coll (PrivateTools.SMLSOURCE src) = let
		    val { sourcepath = p, history = h, share = s } = src
		    val i =  SmlInfo.new
			Policy.default
			{ sourcepath = p, group = group,
			  error = error, history = h,
			  share = s, stableinfo = NONE }
		    val exports = SmlInfo.exports i
		    fun addLD (s, m) = SymbolMap.insert (m, s, i)
		    val ld = SymbolSet.foldl addLD SymbolMap.empty exports
		in
		    COLLECTION { subexports = SymbolMap.empty,
				 smlfiles = [i],
				 localdefs = ld }
		end
	    val collections = map exp2coll expansions
	    fun combine (c1, c2) = sequential (c2, c1, error)
	in
	    foldl combine empty collections
	end
    in
	if isSome class then noPrimitive ()
	else case Primitive.fromString (AbsPath.spec sourcepath) of
	    SOME p => let
		val exports = Primitive.exports p
		fun addFN (s, m) =
		    SymbolMap.insert (m, s, (NONE, DG.PNODE p))
		val se = SymbolSet.foldl addFN SymbolMap.empty exports
	    in
		COLLECTION { subexports = se,
			     smlfiles = [],
			     localdefs = SymbolMap.empty }
	    end
	  | NONE => noPrimitive ()
    end

    fun num_look (c: collection) (s: string) = 0

    fun cm_look (c: collection) (s: string) = false

    fun ml_look (COLLECTION { subexports, localdefs, ... }) s =
	isSome (SymbolMap.find (subexports, s)) orelse
	isSome (SymbolMap.find (localdefs, s))
end
