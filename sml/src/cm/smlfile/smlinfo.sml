signature SMLINFO = sig

    type info

    type policy = Policy.policy

    type fileoffset = AbsPath.t * int
    type stableinfo = { skeleton: Skeleton.decl, binary: fileoffset }

    val new : policy ->
	{ sourcepath: AbsPath.t,
	  group: AbsPath.t,
	  history: string list,
	  share: bool option,
	  stableinfo: stableinfo option }
	-> info

    val exports : info -> SymbolSet.set
    val describe : info -> string
end

structure SmlInfo :> SMLINFO = struct

    type source = GenericVC.Source.inputSource
    type parsetree = GenericVC.Ast.dec

    type policy = Policy.policy

    type info = Dummy.t
(*
	INFO {
	      sourcepath: AbsPath.t,
	      lastseen: TStamp.t ref,
	      parsetree: { tree: parsetree, source: source } option ref,
	      skelpath: AbsPath.t,
	      skeleton: Skeleton.decl option ref,
	      (* to be extended *)
	     }
*)

    type fileoffset = AbsPath.t * int
    type stableinfo = { skeleton: Skeleton.decl, binary: fileoffset }

(*
    fun new policy { sourcepath, group, history, share, stableinfo = NONE } =
	INFO {
	      sourcepath = sourcepath,
	      lastseen = ref TStamp.NOTSTAMP,
	      parsetree = ref NONE,
	      skelpath = Policy.mkSkelPath policy sourcepath,
	      skeleton = ref NONE
	     }
*)
    fun new policy arg = Dummy.v

(*
    fun outdated (INFO { sourcepath, lastseen = ref ts, ... }) = let
	val nts = AbsPath.modtime sourcepath
    in
	(TStamp.earlier (ts, nts), nts)
    end
*)

(*
    fun getParseTree (i as INFO { sourcepath, parsetree, lastseen, ... }) = let
	val (outd, nts) = outdated i
	fun parse () =
    in
	if outd then parse ()
	else case !parsetree of
	    NONE => parse ()
	  | SOME pt => pt
    end
*)

    fun exports i = (ignore Dummy.v; SymbolSet.empty)
    fun describe i = Dummy.f ()
end
