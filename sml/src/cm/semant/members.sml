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
    type impexp = DependencyGraph.impexp

    type collection

    val empty : collection

    val expandOne : GeneralParams.params * (AbsPath.t -> GroupGraph.group)
	-> { sourcepath: AbsPath.t, group: AbsPath.t,
	     class: string option,
	     error : GenericVC.ErrorMsg.complainer }
	-> collection
    val sequential : collection * collection * (string -> unit) -> collection

    val build : collection * SymbolSet.set option * (string -> unit)
	-> impexp SymbolMap.map

    val subgroups : collection -> GroupGraph.group list

    val num_look : collection -> string -> int
    val ml_look : collection -> symbol -> bool
    val cm_look : collection -> string -> bool
end

structure MemberCollection :> MEMBERCOLLECTION = struct

    structure DG = DependencyGraph
    structure EM = GenericVC.ErrorMsg
    structure CBE = GenericVC.BareEnvironment
    structure SS = SymbolSet
    structure GG = GroupGraph

    type smlinfo = SmlInfo.info
    type symbol = Symbol.symbol
    type impexp = DG.impexp

    datatype collection =
	COLLECTION of { imports: impexp SymbolMap.map,
		        gimports: impexp SymbolMap.map,
		        smlfiles: smlinfo list,
			localdefs: smlinfo SymbolMap.map,
			subgroups: GG.group list }

    val empty =
	COLLECTION { imports = SymbolMap.empty,
		     gimports = SymbolMap.empty,
		     smlfiles = [],
		     localdefs = SymbolMap.empty,
		     subgroups = [] }

    fun sequential (COLLECTION c1, COLLECTION c2, error) = let
	fun describeSymbol (s, r) = let
	    val ns = Symbol.nameSpace s
	in
	    Symbol.nameSpaceToString ns :: " " :: Symbol.name s :: r
	end
	fun i_error (s, x as (fn1, _), (fn2, _)) =
	    (error (concat (describeSymbol
			    (s, [" imported from ", DG.describeFarSBN fn1,
				 " and also from ", DG.describeFarSBN fn2])));
	     x)
	val i_union = SymbolMap.unionWithi i_error
	val gi_union = SymbolMap.unionWith #1
	fun ld_error (s, f1, f2) =
	    (error (concat (describeSymbol
			    (s, [" defined in ", SmlInfo.spec f1,
				 " and also in ", SmlInfo.spec f2])));
	     f1)
	val ld_union = SymbolMap.unionWithi ld_error
    in
	COLLECTION { imports = i_union (#imports c1, #imports c2),
		     gimports = gi_union (#gimports c1, #gimports c2),
		     smlfiles = #smlfiles c1 @ #smlfiles c2,
		     localdefs = ld_union (#localdefs c1, #localdefs c2),
		     subgroups = #subgroups c1 @ #subgroups c2 }
    end

    fun expandOne (params, rparse) arg = let
	val primconf = #primconf params
	val { sourcepath, group, class, error } = arg
	fun noPrimitive () = let
	    fun e0 s = error EM.COMPLAIN s EM.nullErrorBody
	    fun w0 s = error EM.WARN s EM.nullErrorBody
	    val expansions = PrivateTools.expand e0 (sourcepath, class)
	    fun exp2coll (PrivateTools.GROUP p) = let
		    val g as GG.GROUP { exports = i, islib, ... } = rparse p
		    val gi = if islib then SymbolMap.empty else i
	        in
		    COLLECTION { imports = i, gimports = gi, smlfiles = [],
				 localdefs = SymbolMap.empty,
				 subgroups = [g] }
	        end
	      | exp2coll (PrivateTools.SMLSOURCE src) = let
		    val { sourcepath = p, history = h, share = s } = src
		    val i =  SmlInfo.info
			params
			{ sourcepath = p, group = group,
			  error = error,
			  history = h,
			  share = s }
		    val exports = SmlInfo.exports i
		    val _ = if SS.isEmpty exports then w0 "no module exports"
			    else ()
		    fun addLD (s, m) = SymbolMap.insert (m, s, i)
		    val ld = SS.foldl addLD SymbolMap.empty exports
		in
		    COLLECTION { imports = SymbolMap.empty,
				 gimports = SymbolMap.empty,
				 smlfiles = [i],
				 localdefs = ld,
				 subgroups = [] }
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
		val exports = Primitive.exports primconf p
		val plook = Primitive.lookup primconf p
		fun addFN (s, m) = let
		    val env = plook s
		    val fsbn = (NONE, DG.SB_BNODE (DG.PNODE p))
		in
		    SymbolMap.insert (m, s, (fsbn, env))
		end
		val imp = SS.foldl addFN SymbolMap.empty exports
	    in
		COLLECTION { imports = imp,
			     gimports = SymbolMap.empty,
			     smlfiles = [],
			     localdefs = SymbolMap.empty,
			     subgroups = [] }
	    end
	  | NONE => noPrimitive ()
    end

    fun build (COLLECTION c, fopt, error) = BuildDepend.build (c, fopt, error)

    fun subgroups (COLLECTION { subgroups = sg, ... }) = sg

    fun num_look (c: collection) (s: string) = 0

    fun cm_look (c: collection) (s: string) = false

    fun ml_look (COLLECTION { imports, localdefs, ... }) s =
	isSome (SymbolMap.find (imports, s)) orelse
	isSome (SymbolMap.find (localdefs, s))
end
