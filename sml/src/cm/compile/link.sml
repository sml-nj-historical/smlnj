(*
 * Link traversals.
 *   - manages shared state
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure GP = GeneralParams
    structure DG = DependencyGraph
    structure GG = GroupGraph
    structure E = GenericVC.Environment
    structure DE = DynamicEnv
    structure EM = GenericVC.ErrorMsg
    structure PP = PrettyPrint

    type env = E.dynenv
in
    signature LINK = sig
	(* Evict value from cache if it exists *)
	val evict : GP.info -> SmlInfo.info -> unit

	(* Check all values and evict those that depended on other
	 * meanwhile evicted ones. *)
	val cleanup : GP.info -> unit

	val newTraversal : GG.group ->
	    { group: GP.info -> env option,
	      exports: (GP.info -> env option) SymbolMap.map }

	val sysval : GenericVC.PersStamps.persstamp option -> env option

	(* discard persistent state *)
	val reset : unit -> unit
    end

    functor LinkFn (structure MachDepVC : MACHDEP_VC
		    val getBFC : SmlInfo.info -> MachDepVC.Binfile.bfContent
		    val system_values : env ref) :> LINK = struct

	structure BF = MachDepVC.Binfile

	type bfun = GP.info -> E.dynenv -> E.dynenv

	datatype bnode =
	    B of bfun * BinInfo.info * bnode list

	val stablemap = ref (StableMap.empty: bnode StableMap.map)

	val emptyStatic = E.staticPart E.emptyEnv
	val emptyDyn = E.dynamicPart E.emptyEnv

	fun sysval NONE = NONE
	  | sysval (SOME pid) =
	    SOME (DynamicEnv.bind (pid,
				   DynamicEnv.look (!system_values) pid,
				   DynamicEnv.empty))
	    handle DynamicEnv.Unbound => NONE

	fun exn_err (msg, error, descr, exn) = let
	    fun ppb pps =
		(PP.add_newline pps;
		 PP.add_string pps (General.exnMessage exn);
		 PP.add_newline pps)
	in
	    error (concat [msg, " ", descr]) ppb;
	    raise exn
	end

	fun execute (bfc, de) = let
	    fun exec () = E.dynamicPart (BF.exec (bfc, de))
	in
	    case sysval (BF.exportPidOf bfc) of
		NONE => exec ()
	      | SOME de => de
	end

	fun memoize thunk = let
	    val r = ref (fn _ => raise Fail "Link:memoize")
	    fun firsttime gp = let
		val v = thunk gp
	    in
		r := (fn _ => v);
		v
	    end
	in
	    r := firsttime;
	    fn gp => !r gp
	end

	type smemo = E.dynenv * SmlInfo.info list

	val smlmap = ref (SmlInfoMap.empty: smemo SmlInfoMap.map)

	fun evict gp i = let
	    fun check () =
		case SmlInfo.sh_mode i of
		    Sharing.SHARE true =>
			SmlInfo.error gp i EM.WARN
			  (concat ["sharing for ",
				   SmlInfo.descr i,
				   " may be lost"])
			  EM.nullErrorBody
		  | _ =>  ()
	in
	    (smlmap := #1 (SmlInfoMap.remove (!smlmap, i))
	     before check ())
	    handle LibBase.NotFound => ()
	end

	fun cleanup gp = let
	    val visited = ref SmlInfoSet.empty
	    fun visit i =
		if SmlInfoSet.member (!visited, i) then true
		else
		    case SmlInfoMap.find (!smlmap, i) of
			NONE => false
		      | SOME (_, l) => let
			    val bl = map visit l
			    val b = List.all (fn x => x) bl
			in
			    if b then
				(visited := SmlInfoSet.add (!visited, i);
				 true)
			    else (evict gp i; false)
			end
	in
	    app (visit o #1) (SmlInfoMap.listItemsi (!smlmap))
	end

	fun prim2dyn p (gp: GP.info) =
	    E.dynamicPart (Primitive.env (#primconf (#param gp)) p)

	fun getPerv (gp: GP.info) = E.dynamicPart (#pervasive (#param gp))

	fun link_stable (i, e) = let
	    val stable = BinInfo.stablename i
	    val os = BinInfo.offset i
	    val descr = BinInfo.describe i
	    val _ = Say.vsay ["[linking with ", descr, "]\n"]
	    val error = BinInfo.error i EM.COMPLAIN
	in
	    let fun work s =
		(Seek.seek (s, os);
		 (* We can use an empty static env because no
		  * unpickling will be done. *)
		 BF.read { stream = s, name = descr, senv = emptyStatic })
		val bfc =
		    SafeIO.perform { openIt = fn () => BinIO.openIn stable,
				     closeIt = BinIO.closeIn,
				     work = work,
				     cleanup = fn () => () }
		    handle exn =>
			exn_err ("unable to load library module",
				 error, descr, exn)
		val epid = BF.exportPidOf bfc
	    in
		execute (bfc, e)
		handle exn => exn_err ("link-time exception in library code",
				       error, descr, exn)
	    end
	end

	fun link_sml (gp, i, getE, snl) = let
	    fun fresh () = let
		val bfc = getBFC i
	    in
		case getE gp of
		    NONE => NONE
		  | SOME e =>
			(SOME (execute (bfc, e))
			 handle exn =>
			     exn_err ("link-time exception in user program",
				      SmlInfo.error gp i EM.COMPLAIN,
				      SmlInfo.descr i,
				      exn))
	    end handle _ => NONE
	in
	    case SmlInfo.sh_mode i of
		Sharing.SHARE _ =>
		    (case SmlInfoMap.find (!smlmap, i) of
			 NONE =>
			     (case fresh () of
				  NONE => NONE
				| SOME de => let
				      val m = (de, snl)
				  in
				      smlmap :=
				        SmlInfoMap.insert (!smlmap, i, m);
				      SOME de
				  end)
		       | SOME (de, _) => SOME de)
	      | Sharing.DONTSHARE => (evict gp i; fresh ())
	end

	fun registerGroup g = let
	    val GG.GROUP { grouppath, kind, sublibs, ... } = g
	    val visited = ref SrcPathSet.empty
	    fun registerStableLib (GG.GROUP { exports, ... }) = let
		val localmap = ref StableMap.empty
		fun bn (DG.PNODE p) =
		    (fn gp => fn _ => prim2dyn p gp, NONE)
		  | bn (DG.BNODE n) = let
			val { bininfo = i, localimports, globalimports } = n
			fun new () = let
			    val e0 = (getPerv, [])
			    fun join ((f, NONE), (e, l)) =
				(fn gp => DE.atop (f gp emptyDyn, e gp), l)
			      | join ((f, SOME (i, l')), (e, l)) =
				(e, B (f, i, l') :: l)
			    val ge = foldl join e0 (map fbn globalimports)
			    val le = foldl join ge (map bn localimports)
			in
			    case (BinInfo.sh_mode i, le) of
				(Sharing.SHARE _, (e, [])) => let
				    fun thunk gp = link_stable (i, e gp)
				    val m_thunk = memoize thunk
				in
				    (fn gp => fn _ => m_thunk gp, NONE)
				end
			      | (Sharing.SHARE _, _) =>
				EM.impossible "Link: sh_mode inconsistent"
			      | (Sharing.DONTSHARE, (e, l)) =>
				(fn gp => fn e' =>
				   link_stable (i, (DE.atop (e', e gp))),
				 SOME (i, l))
			end
		    in
			case StableMap.find (!stablemap, i) of
			    SOME (B (f, i, [])) =>
				(case BinInfo.sh_mode i of
				     Sharing.DONTSHARE => (f, SOME (i, []))
				   | _ => (f, NONE))
			  | SOME (B (f, i, l)) => (f, SOME (i, l))
			  | NONE =>
				(case StableMap.find (!localmap, i) of
				     SOME x => x
				   | NONE => let
					 val x = new ()
				     in
					 localmap :=
					    StableMap.insert (!localmap, i, x);
					 x
				     end)
		    end

		and fbn (_, n) = bn n

		fun sbn (DG.SB_SNODE n) =
		    EM.impossible "Link:SNODE in stable lib"
		  | sbn (DG.SB_BNODE (DG.PNODE _, _)) = ()
		  | sbn (DG.SB_BNODE (n as DG.BNODE { bininfo, ... }, _)) = let
			val b as B (_, i, _) =
			    case bn n of
				(f, NONE) => B (f, bininfo, [])
			      | (f, SOME (i, l)) => B (f, i, l)
		    in
			stablemap := StableMap.insert (!stablemap, i, b)
		    end

                fun fsbn (_, n) = sbn n
		fun impexp (n, _) = fsbn n
	    in
		SymbolMap.app impexp exports
	    end
	in
	    if SrcPathSet.member (!visited, grouppath) then ()
	    else (visited := SrcPathSet.add (!visited, grouppath);
		  app registerGroup sublibs;
		  case kind of
		      GG.STABLELIB => registerStableLib g
		    | _ => ())
	end

	fun newTraversal (group as GG.GROUP { exports, ... }) = let
	    val _ = registerGroup group

	    val l_stablemap = ref StableMap.empty
	    val l_smlmap = ref SmlInfoMap.empty

	    fun bnode (B (f, i, l)) =
		case StableMap.find (!l_stablemap, i) of
		    SOME th => th
		  | NONE => let
			val fl = map bnode l
			fun th gp = let
			    fun add (t, e) = DE.atop (t gp, e)
			in
			    f gp (foldl add emptyDyn fl)
			end
			val m_th = memoize th
		    in
			l_stablemap :=
			  StableMap.insert (!l_stablemap, i, m_th);
			m_th
		    end

	    fun sbn (DG.SB_BNODE (DG.PNODE p, _)) = (SOME o prim2dyn p, [])
	      | sbn (DG.SB_BNODE (DG.BNODE { bininfo, ... }, _)) = let
		    val b = valOf (StableMap.find (!stablemap, bininfo))
		    fun th gp =
			SOME (bnode b gp)
			handle exn => NONE
		in
		    (th, [])
		end
	      | sbn (DG.SB_SNODE n) = sn n

	    and sn (DG.SNODE n) = let
		val { smlinfo = i, localimports, globalimports } = n
	    in
		case SmlInfoMap.find (!l_smlmap, i) of
		    SOME th => (th, [i])
		  | NONE => let
			fun atop (NONE, _) = NONE
			  | atop (_, NONE) = NONE
			  | atop (SOME e, SOME e') = SOME (DE.atop (e, e'))
			fun add ((f, l), (f', l')) =
			    (fn gp => atop (f gp, f' gp), l @ l')
			val gi = foldl add (SOME o getPerv, [])
			                   (map fsbn globalimports)
			val (getE, snl) = foldl add gi (map sn localimports)
			fun thunk gp = link_sml (gp, i, getE, snl)
			val m_thunk = memoize thunk
		    in
			l_smlmap := SmlInfoMap.insert (!l_smlmap, i, m_thunk);
			(m_thunk, [i])
		    end
	    end

	    and fsbn (_, n) = sbn n

	    fun impexp (n, _) = #1 (fsbn n)

	    val exports' = SymbolMap.map impexp exports

	    fun group' gp = let
		fun one (_, NONE) = NONE
		  | one (f, SOME e) =
		    (case f gp of
			 NONE => NONE
		       | SOME e' => SOME (DE.atop (e', e)))
	    in
		SymbolMap.foldl one (SOME emptyDyn) exports'
	    end
	in
	    { exports = exports', group = group' }
	end

	fun reset () = (stablemap := StableMap.empty;
			smlmap := SmlInfoMap.empty)
    end
end
