(* binfile-new.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com
 *)
(*
 * This revised version of signature BINFILE is now machine-independent.
 * Moreover, it deals with the file format only and does not know how to
 * create new binfile contents (aka "compile") or how to interpret the
 * pickles.  As a result, it does not statically depend on the compiler.
 * (Eventually we might want to support a light-weight binfile loader.)
 *)
signature BINFILE = sig

    type bfContents

    exception FormatError

    type pid = PersStamps.persstamp
    type stats = { env: int, inlinfo: int, data: int, code: int }
    type pickle = { pid: pid, pickle: Word8Vector.vector }

    val staticPidOf    : bfContents -> pid
    val exportPidOf    : bfContents -> pid option
    val lambdaPidOf    : bfContents -> pid
    val cmDataOf       : bfContents -> pid list

    val senvPickleOf   : bfContents -> pickle
    val lambdaPickleOf : bfContents -> pickle

    (* calculate the size in bytes occupied by some binfile contents *)
    val size : { contents: bfContents, nopickle: bool } -> int

    (* create the abstract binfile contents *)
    val create : { imports: ImportTree.import list,
		   exportPid: pid option,
		   cmData: pid list,
		   senv: pickle,
		   lambda: pickle,
		   csegments: CodeObj.csegments } -> bfContents

    (* read binfile contents from an IO stream *)
    val read : { arch: string, version: int list,
		 name: string, stream: BinIO.instream }
	       -> { contents: bfContents, stats: stats }

    (* write binfile contents to an IO stream *)
    val write : { arch: string, version: int list,
		  stream: BinIO.outstream,
		  contents: bfContents, nopickle: bool }
		-> stats

    (* Given a dynamic environment, link the code object contained in
     * some given binfile contents. The result is the delta environment
     * containing the bindings (if any) resulting from this link operation. *)
    val exec : bfContents * DynamicEnv.env -> DynamicEnv.env
end
