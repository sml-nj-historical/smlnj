(*
 * Reading and writing skeletons to skeleton files.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SKELIO = sig
    exception InternalError
    val read : AbsPath.t * TStamp.t -> Skeleton.decl option
    val write : AbsPath.t * Skeleton.decl -> unit
end

structure SkelIO :> SKELIO = struct

    structure SK = Skeleton
    structure SS = SymbolSet
    structure S = Symbol
    structure SP = GenericVC.SymPath

    exception InternalError
    exception FormatError

    val s2b = Byte.stringToBytes
    val b2c = Byte.byteToChar

    val version = "Skeleton 1\n"

    fun makeset l = SS.addList (SS.empty, l)

    fun inputLine s = let
	fun finish acc = String.implode (rev acc)
	fun loop acc =
	    case Option.map b2c (BinIO.input1 s) of
		NONE => finish (#"\n" :: acc)
	      | SOME #"\n" => finish (#"\n" :: acc)
	      | SOME c => loop (c :: acc)
    in
	loop []
    end

    fun write_decl (s, d) = let

	(* We are consing up the whole output as a list of strings
	 * before concatenating it to form the final result and
	 * wrinting it out using one single `output' call. *)
	fun w_name (n, r) =
	    (case S.nameSpace n of
		 S.SIGspace => "'"	(* only tyvars could start like that *)
	       | S.FCTspace => "("	(* no sym can start like that *)
	       | S.FSIGspace => ")"	(* no sym can start like that *)
	       | S.STRspace => ""	(* this should be safe now *)
	       | _ => raise InternalError)
	    :: S.name n :: "." :: r

	(* foldl means that last element appears first in output! *)
	fun w_list w (l, r) = foldl w (";" :: r) l

	fun w_path (SP.SPATH p, r) = w_list w_name (p, r)

	fun w_decl (SK.Bind (name, def), r) =
	    "b" :: w_name (name, w_modExp (def, r))
          | w_decl (SK.Local (x, y), r) = "l" :: w_decl (x, w_decl (y, r))
	  | w_decl (SK.Par l, r) = "p" :: w_list w_decl (l, r)
	  | w_decl (SK.Seq l, r) = "q" :: w_list w_decl (l, r)
 	  | w_decl (SK.Open d, r) = "o" :: w_modExp (d, r)
	  | w_decl (SK.Ref s, r) = "r" :: w_list w_name (SS.listItems s, r)

	and w_modExp (SK.Var p, r) = "v" :: w_path (p, r)
	  | w_modExp (SK.Decl d, r) = "d" :: w_list w_decl (d, r)
	  | w_modExp (SK.Let (d, m), r) =
	    "l" :: w_list w_decl (d, w_modExp (m, r))
 	  | w_modExp (SK.Ign1 (m1, m2), r) =
	    "i" :: w_modExp (m1, w_modExp (m2, r))
    in
	BinIO.output (s, s2b (concat (version :: w_decl (d, ["\n"]))))
    end

    fun read_decl s = let

	fun rd () = Option.map b2c (BinIO.input1 s)

	local
	    fun get (ns, first) = let
		fun loop (accu, NONE) = raise FormatError
		  | loop ([], SOME #".") = raise FormatError
		  | loop (accu, SOME #".") = ns (String.implode (rev accu))
		  | loop (accu, SOME s) = loop (s :: accu, rd ())
	    in
		loop ([], first)
	    end
	in
	    fun r_name (SOME #"'") = get (S.sigSymbol, rd ())
	      | r_name (SOME #"(") = get (S.fctSymbol, rd ())
	      | r_name (SOME #")") = get (S.fsigSymbol, rd ())
	      | r_name first = get (S.strSymbol, first)
	end

	(* lists are written in reverse order, so a tail-recursive
	 * reader is exactly right because it undoes the reversal *)
	fun r_list r first = let
	    (* argument order important: side effects in arguments! *)
	    fun rl (l, SOME #";") = l
	      | rl (l, first) = rl (r first :: l, rd ())
	in
	    rl ([], first)
	end

	fun r_path first = SP.SPATH (r_list r_name first)

	fun r_decl (SOME #"b") = SK.Bind (r_name (rd ()), r_modExp (rd ()))
	  | r_decl (SOME #"l") = SK.Local (r_decl (rd ()), r_decl (rd ()))
	  | r_decl (SOME #"p") = SK.Par (r_list r_decl (rd ()))
	  | r_decl (SOME #"q") = SK.Seq (r_list r_decl (rd ()))
 	  | r_decl (SOME #"o") = SK.Open (r_modExp (rd ()))
	  | r_decl (SOME #"r") = SK.Ref (makeset (r_list r_name (rd ())))
	  | r_decl _ = raise FormatError

	and r_modExp (SOME #"v") = SK.Var (r_path (rd ()))
	  | r_modExp (SOME #"d") = SK.Decl (r_list r_decl (rd ()))
	  | r_modExp (SOME #"l") =
	    SK.Let (r_list r_decl (rd ()), r_modExp (rd ()))
 	  | r_modExp (SOME #"i") = SK.Ign1 (r_modExp (rd ()), r_modExp (rd ()))
	  | r_modExp _ = raise FormatError

	val firstline = inputLine s
	val r = if firstline = version then r_decl (rd ())
		else raise FormatError
	val nl = rd ()
    in
	if nl = SOME #"\n" then r else raise FormatError
    end

    fun read (ap, ts) =
	if TStamp.earlier (AbsPath.tstamp ap, ts) then NONE
	else let
	    val s = AbsPath.openBinIn ap
	    val r = read_decl s
		handle exn => (BinIO.closeIn s; raise exn)
	in
	    BinIO.closeIn s; SOME r
	end handle _ => NONE

    fun write (ap, sk) = let
	val s = AbsPath.openBinOut Say.vsay ap
    in
	(Interrupt.guarded (fn () => write_decl (s, sk));
	 BinIO.closeOut s)
	handle exn => let
	    val p = AbsPath.name ap
	in
	    BinIO.closeOut s;
	    OS.FileSys.remove p handle _ => ();
	    Say.say (concat ["[writing ", p, " failed]\n"]);
	    raise exn
	end
    end handle Interrupt.Interrupt => raise Interrupt.Interrupt
             | InternalError => raise InternalError
             | _ => ()
end
