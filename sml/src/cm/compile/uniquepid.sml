(* uniquepid.sml
 *
 * Generating unique pids.  (We no longer use the hash of the static
 * environment as a persistent identifier.   Instead, we create a
 * brand new stamp every time a compilation unit is being compiled
 * UNLESS we find that the SAME compilation unit has been compiled
 * before.  In the latter case we check whether the export interface
 * has changed since last time (by comparing the environment hashes),
 * and if it has not, then we reuse the old pid.
 *
 *   (C) 2002 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@research.bell-labs.com)
 *)
local
    type pid = PersStamps.persstamp
    type triplet = { staticPid: pid, fingerprint: pid, pepper: string }
in    
structure UniquePid :> sig
    type info
    val reset : unit -> unit
    val getInfo : GeneralParams.info -> SrcPath.file -> info
    val uniquepid : info -> pid -> pid * string
    val saveInfo : GeneralParams.info -> SrcPath.file -> triplet -> unit
    val sync : GeneralParams.info -> unit
end = struct
    type cache = triplet StringMap.map
    type info = triplet option

    type cachecache = cache StringMap.map

    val cc : cachecache ref = ref StringMap.empty

    fun reset () = cc := StringMap.empty

    exception BadCache

    fun readCache file = let
	fun hex2string hs = let
	    fun h2i #"0" = 0
	      | h2i #"1" = 1
	      | h2i #"2" = 2
	      | h2i #"3" = 3
	      | h2i #"4" = 4
	      | h2i #"5" = 5
	      | h2i #"6" = 6
	      | h2i #"7" = 7
	      | h2i #"8" = 8
	      | h2i #"9" = 9
	      | h2i #"A" = 10
	      | h2i #"B" = 11
	      | h2i #"C" = 12
	      | h2i #"D" = 13
	      | h2i #"E" = 14
	      | h2i #"F" = 15
	      | h2i _ = raise BadCache
	    fun hdig i = h2i (String.sub (hs, i))
	    fun loop (i, a) =
		if i + 1 >= size hs then String.implode (rev a)
		else loop (i + 2,
			   Char.chr (16 * hdig i + hdig (i + 1)) :: a)
	in
	    loop (0, [])
	end

	fun hex2pid hs =
	    case PersStamps.fromHex hs of
		SOME p => p
	      | NONE => raise BadCache

	fun work s = let
	    fun loop m = let
		val line = TextIO.inputLine s
	    in
		case String.tokens Char.isSpace line of
		    [hkey, hhash, hpid, hpepper] => let
			val key = hex2string hkey
			val pid = hex2pid hpid
			val hash = hex2pid hhash
			val pepper = hex2string hpepper
		    in
			loop (StringMap.insert (m, key,
						{ staticPid = pid,
						  fingerprint = hash,
						  pepper = pepper }))
		    end
		  | _ => m
	    end
	in
	    loop StringMap.empty
	end
    in
	SafeIO.perform { openIt = fn () => TextIO.openIn file,
			 closeIt = TextIO.closeIn,
			 work = work,
			 cleanup = fn _ => () }
	handle _ => StringMap.empty
    end

    fun getBininfo bn =
	SOME (SafeIO.perform { openIt = fn () => BinIO.openIn bn,
			       closeIt = BinIO.closeIn,
			       work = Binfile.readFingerprintInfo,
			       cleanup = fn _ => () })
	handle _ => NONE

    val pid2hex = PersStamps.toHex

    fun writeCache (file, m) = let
	fun work s = let
	    fun one (key, { staticPid, fingerprint, pepper }) = let
		fun out x = TextIO.output (s, x)
		fun outp p = out (pid2hex p)
		fun out1 c = TextIO.output1 (s, c)
		fun outh i = out1 (String.sub ("0123456789ABCDEF", i))
		fun outh2 i = (outh (i div 16); outh (i mod 16))
		fun outc c = outh2 (Char.ord c)
		fun outv v = CharVector.app outc v
	    in
		outv key; out " ";
		outp fingerprint; out " ";
		outp staticPid; out " ";
		outv pepper; out "\n"
	    end
	in
	    StringMap.appi one m
	end
    in
	SafeIO.perform { openIt = fn () => AutoDir.openTextOut file,
			 closeIt = TextIO.closeOut,
			 work = work,
			 cleanup = fn _ => (OS.FileSys.remove file
					    handle _ => ()) }
    end

    fun getInfo (gp: GeneralParams.info) src = let
	val fnp = #fnpolicy (#param gp)
	val bn = FilenamePolicy.mkBinName fnp src
	val { file = cn, key } = FilenamePolicy.mkIdCacheName fnp src
	val bininfo = getBininfo bn
	fun drep w (NONE : triplet option) = NONE
	  | drep w (x as SOME y) =
	    (Say.dsay ["... got triplet from ", w, ": ",
		       key, " ", pid2hex (#fingerprint y),
		       "-->", pid2hex (#staticPid y),
		       " (", #pepper y, ")\n"];
	     x)
    in
	case drep "bininfo" bininfo of
	    SOME _ => bininfo
	  | NONE => let
		val c = case StringMap.find (!cc, cn) of
			    NONE =>
			    let val c = readCache cn
			    in cc := StringMap.insert (!cc, cn, c); c
			    end
			  | SOME c => c
	    in
		drep "cache" (StringMap.find (c, key))
	    end
    end

    fun add_cc (cn, key, data) = let
	val cc0 = !cc
	val c = getOpt (StringMap.find (cc0, cn), StringMap.empty)
	val c' = StringMap.insert (c, key, data)
    in
	cc := StringMap.insert (cc0, cn, c')
    end

    fun saveInfo (gp: GeneralParams.info) src data =
	if #slave_mode (#param gp) then ()
	else let val fnp = #fnpolicy (#param gp)
		 val { file = cn, key } = FilenamePolicy.mkIdCacheName fnp src
	     in
		 add_cc (cn, key, data)
	     end

    fun uniquepid data hash = let
	fun new () = let
	    val pepper = Time.toString (Time.now ())
	    val pid = Rehash.addPepper { hash = hash, pepper = pepper }
	in
	    Say.dsay ["--- generating new pid\n"];
	    (pid, pepper)
	end
    in
	case data of
	    SOME { staticPid, fingerprint, pepper } =>
	    (if PersStamps.compare (hash, fingerprint) = EQUAL then
		 (Say.dsay ["+++ reusing old pid\n"];
		  (staticPid, pepper))
	     else new ())
	  | NONE => new ()
    end

    fun sync (gp: GeneralParams.info) = let
	fun add (cn, c) =
	    (Say.dsay [">>> syncing ", cn, "\n"];
	     writeCache (cn, StringMap.unionWith #1 (c, readCache cn)))
    in
	if #slave_mode (#param gp) then () else StringMap.appi add (!cc)
    end
end
end
