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

    val version = "Decl 9\n"

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
	fun w_name (n, r) = let
	    val ns = S.nameSpace n
	    val prefix =
		case ns of
		    S.STRspace => "#"
		  | S.SIGspace => "$"
		  | S.FCTspace => "%"
		  | S.FSIGspace => "&"
		  | _ => raise InternalError
	in
	    prefix :: S.name n :: "." :: r
	end

	fun w_list w (l, r) = foldr w (";" :: r) l

	fun w_path (SP.SPATH p, r) = w_list w_name (p, r)

	fun w_option w (NONE, r) = "-" :: r
	  | w_option w (SOME x, r) = "+" :: w (x, r)

	fun w_decl (SK.StrDecl l, r) =
	    let
		fun w_item ({ name, def, constraint }, r) =
		    w_name (name,
			    w_strExp (def,
				      w_option w_strExp (constraint, r)))
	    in
		"s" :: w_list w_item (l, r)
	    end
	  | w_decl (SK.FctDecl l, r) = let
		fun w_item ({ name, def }, r) =
		    w_name (name, w_fctExp (def, r))
	    in
		"f" :: w_list w_item (l, r)
	    end
          | w_decl (SK.LocalDecl (x, y), r) = "l" :: w_decl (x, w_decl (y, r))
	  | w_decl (SK.SeqDecl l, r) = "q" :: w_list w_decl (l, r)
 	  | w_decl (SK.OpenDecl l, r) = "o" :: w_list w_strExp (l, r)
	  | w_decl (SK.DeclRef s, r) = "r" :: w_list w_name (SS.listItems s, r)

	and w_strExp (SK.VarStrExp p, r) = "v" :: w_path (p, r)
	  | w_strExp (SK.BaseStrExp d, r) = "s" :: w_decl (d, r)
	  | w_strExp (SK.AppStrExp (p, l), r) =
	    "a" :: w_path (p, w_list w_strExp (l, r))
	  | w_strExp (SK.LetStrExp (d, se), r) =
	    "l" :: w_decl (d, w_strExp (se, r))
	  | w_strExp (SK.AugStrExp (se, s), r) =
	    "g" :: w_strExp (se, w_list w_name (SS.listItems s, r))
 	  | w_strExp (SK.ConStrExp (s1, s2), r) =
 	    "c" :: w_strExp (s1, w_strExp(s2, r))

	and w_fctExp (SK.VarFctExp (p, fe), r) =
	    "v" :: w_path (p, w_option w_fctExp (fe, r))
	  | w_fctExp (SK.BaseFctExp { params, body, constraint }, r) = let
		fun w_item ((mn, se), r) =
		    w_option w_name (mn, w_strExp (se, r))
	    in
		"f" ::
		w_list w_item (params,
			       w_strExp (body,
					 w_option w_strExp (constraint, r)))
	    end
	  | w_fctExp (SK.AppFctExp (p, sel, feo), r) =
	    "a" ::
	    w_path (p, w_list w_strExp (sel, w_option w_fctExp (feo, r)))
	  | w_fctExp (SK.LetFctExp (d, fe), r) =
	    "l" :: w_decl (d, w_fctExp (fe, r))

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
	    fun r_name (SOME #"#") = get (S.strSymbol, rd ())
	      | r_name (SOME #"$") = get (S.sigSymbol, rd ())
	      | r_name (SOME #"%") = get (S.fctSymbol, rd ())
	      | r_name (SOME #"&") = get (S.fsigSymbol, rd ())
	      | r_name _ = raise FormatError
	end

	fun r_list r = let
	    fun loop (accu, NONE) = raise FormatError
	      | loop (accu, SOME #";") = rev accu
	      | loop (accu, cur) = loop ((r cur) :: accu, rd ())
	in
	    fn first => loop ([], first)
	end

	fun r_path first = SP.SPATH (r_list r_name first)

	fun r_option r (SOME #"-") = NONE
	  | r_option r (SOME #"+") = SOME (r (rd ()))
	  | r_option r _ = raise FormatError

	fun r_decl (SOME #"s") =
	    let
		fun r_item first = {
				    name = r_name first,
				    def = r_strExp (rd ()),
				    constraint = r_option r_strExp (rd ())
				   }
	    in
		SK.StrDecl (r_list r_item (rd ()))
	    end
	  | r_decl (SOME #"f") =
	    let
		fun r_item first = {
				    name = r_name first,
				    def = r_fctExp (rd ())
				   }
	    in
		SK.FctDecl (r_list r_item (rd ()))
	    end
	  | r_decl (SOME #"l") = SK.LocalDecl (r_decl (rd ()), r_decl (rd ()))
	  | r_decl (SOME #"q") = SK.SeqDecl (r_list r_decl (rd ()))
 	  | r_decl (SOME #"o") = SK.OpenDecl (r_list r_strExp (rd ()))
	  | r_decl (SOME #"r") = SK.DeclRef (makeset (r_list r_name(rd ())))
	  | r_decl _ = raise FormatError

	and r_strExp (SOME #"v") = SK.VarStrExp (r_path (rd ()))
	  | r_strExp (SOME #"s") = SK.BaseStrExp (r_decl (rd ()))
	  | r_strExp (SOME #"a") =
	    SK.AppStrExp (r_path (rd ()), r_list r_strExp (rd ()))
	  | r_strExp (SOME #"l") =
	    SK.LetStrExp (r_decl (rd ()), r_strExp (rd ()))
	  | r_strExp (SOME #"g") =
	    SK.AugStrExp (r_strExp (rd ()), makeset (r_list r_name (rd ())))
 	  | r_strExp (SOME #"c") =
 	    SK.ConStrExp (r_strExp (rd ()), r_strExp (rd ()))
	  | r_strExp _ = raise FormatError

	and r_fctExp (SOME #"v") =
	    SK.VarFctExp (r_path(rd()), r_option r_fctExp(rd()))
	  | r_fctExp (SOME #"f") =
	    let
		fun r_param first = (r_option r_name first, r_strExp (rd ()))
	    in
		SK.BaseFctExp {
			       params = r_list r_param (rd ()),
			       body = r_strExp (rd ()),
			       constraint = r_option r_strExp (rd ())
			      }
	    end
	  | r_fctExp (SOME #"a") =
	    SK.AppFctExp (r_path (rd ()),
			  r_list r_strExp (rd ()),
			  r_option r_fctExp (rd ()))
	  | r_fctExp (SOME #"l") =
	    SK.LetFctExp (r_decl (rd ()), r_fctExp (rd ()))
	  | r_fctExp _ = raise FormatError

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
