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

    type symbol = Symbol.symbol
    type smlinfo = SmlInfo.info

    type collection

    type farlooker =
	AbsPath.t ->
	(DependencyGraph.farsbnode * DependencyGraph.env) SymbolMap.map

    val empty : collection

    val expandOne : farlooker
	-> { sourcepath: AbsPath.t, group: AbsPath.t, class: string option,
	     error : string -> (PrettyPrint.ppstream -> unit) -> unit }
	-> collection
    val sequential : collection * collection * (string -> unit) -> collection

    val build : collection
	-> { nodemap: DependencyGraph.snode SymbolMap.map,
 	     rootset: DependencyGraph.snode list }
	

    val num_look : collection -> string -> int
    val ml_look : collection -> symbol -> bool
    val cm_look : collection -> string -> bool
end

structure MemberCollection :> MEMBERCOLLECTION = struct

    structure DG = DependencyGraph
    structure EM = GenericVC.ErrorMsg
    structure CBE = GenericVC.BareEnvironment

    type smlinfo = SmlInfo.info
    type symbol = Symbol.symbol

    datatype collection =
	COLLECTION of { subexports: (DG.farsbnode * DG.env) SymbolMap.map,
		        smlfiles: smlinfo list,
			localdefs: smlinfo SymbolMap.map }

    type farlooker =
	AbsPath.t ->
	(DependencyGraph.farsbnode * DependencyGraph.env) SymbolMap.map

    val empty =
	COLLECTION { subexports = SymbolMap.empty,
		     smlfiles = [],
		     localdefs = SymbolMap.empty }

    fun convertEnv cmenv = let
	fun modulesOnly sl = let
	    fun addModule (sy, set) =
		case Symbol.nameSpace sy of
		    (Symbol.STRspace | Symbol.SIGspace |
		     Symbol.FCTspace | Symbol.FSIGspace) =>
			SymbolSet.add (set, sy)
		  | _ => set
	in
	    foldl addModule SymbolSet.empty sl
	end
	fun cvt CBE.CM_NONE = NONE
	  | cvt (CBE.CM_ENV { look, symbols }) =
	    SOME (DG.FCTENV { looker = cvt o look,
			      domain = modulesOnly o symbols })
    in
	valOf (cvt cmenv)
    end

    fun sequential (COLLECTION c1, COLLECTION c2, error) = let
	fun describeSymbol (s, r) = let
	    val ns = Symbol.nameSpace s
	in
	    Symbol.nameSpaceToString ns :: " " :: Symbol.name s :: r
	end
	fun se_error (s, x as (fn1, _), (fn2, _)) =
	    (error (concat (describeSymbol
			    (s, [" imported from ", DG.describeFarSBN fn1,
				 " and also from ", DG.describeFarSBN fn2])));
	     x)
	val se_union = SymbolMap.unionWithi se_error
	fun ld_error (s, f1, f2) =
	    (error (concat (describeSymbol
			    (s, [" defined in ", SmlInfo.spec f1,
				 " and also in ", SmlInfo.spec f2])));
	     f1)
	val ld_union = SymbolMap.unionWithi ld_error
    in
	COLLECTION { subexports = se_union (#subexports c1, #subexports c2),
		     smlfiles = #smlfiles c1 @ #smlfiles c2,
		     localdefs = ld_union (#localdefs c1, #localdefs c2) }
    end

    fun expandOne gexports { sourcepath, group, class, error } = let
	fun noPrimitive () = let
	    fun e0 s = error s EM.nullErrorBody
	    val expansions = PrivateTools.expand e0 (sourcepath, class)
	    fun exp2coll (PrivateTools.GROUP p) =
		COLLECTION { subexports = gexports p,
			     smlfiles = [],
			     localdefs = SymbolMap.empty }
	      | exp2coll (PrivateTools.SMLSOURCE src) = let
		    val { sourcepath = p, history = h, share = s } = src
		    val i =  SmlInfo.info
			Policy.default
			{ sourcepath = p, group = group,
			  error = error, history = h,
			  share = s }
		    val exports = SmlInfo.exports i
		    fun addLD (s, m) = SymbolMap.insert (m, s, i)
		    val ld = SymbolSet.foldl addLD SymbolMap.empty exports
		in
		    COLLECTION { subexports = SymbolMap.empty,
				 smlfiles = [i],
				 localdefs = ld }
		end
	    val collections = map exp2coll expansions
	    fun combine (c1, c2) = sequential (c2, c1, e0)
	in
	    foldl combine empty collections
	end
    in
	if isSome class then noPrimitive ()
	else case Primitive.fromString (AbsPath.spec sourcepath) of
	    SOME p => let
		val exports = Primitive.exports p
		fun addFN (s, m) = let
		    val cmenv = Primitive.lookup p s
		    val env = convertEnv cmenv
		    val fsbn = (NONE, DG.SB_BNODE (DG.PNODE p))
		in
		    SymbolMap.insert (m, s, (fsbn, env))
		end
		val se = SymbolSet.foldl addFN SymbolMap.empty exports
	    in
		COLLECTION { subexports = se,
			     smlfiles = [],
			     localdefs = SymbolMap.empty }
	    end
	  | NONE => noPrimitive ()
    end

    fun build (COLLECTION c) = BuildDepend.build c

    fun num_look (c: collection) (s: string) = 0

    fun cm_look (c: collection) (s: string) = false

    fun ml_look (COLLECTION { subexports, localdefs, ... }) s =
	isSome (SymbolMap.find (subexports, s)) orelse
	isSome (SymbolMap.find (localdefs, s))
end
