(* xauth.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Support for X11 authentication.  The authentication file, which is
 * specified by the XAUTHORITY variable (default $HOME/.Xauthority),
 * consists of a sequence of entries with the following format:
 *
 *      2 bytes         Family value (second byte is as in protocol HOST)
 *      2 bytes         address length (always MSB first)
 *      A bytes         host address (as in protocol HOST)
 *      2 bytes         display "number" length (always MSB first)
 *      S bytes         display "number" string
 *      2 bytes         name length (always MSB first)
 *      N bytes         authorization name string
 *      2 bytes         data length (always MSB first)
 *      D bytes         authorization data string
 *
 * This implementation is partially based on code contributed by Juergen Buntrock.
 *)

structure XAuth : X_AUTH =
  struct

    structure EXB = EXeneBase

    val get8 = Word8.toInt o Word8Vector.sub
  (* this version of get16 handles unaligned data *)
    fun get16 (s, i) = let
          val s = Word8Vector.extract (s, i, SOME 2)
          in
            LargeWord.toInt(Pack16Big.subVec(s, 0))
          end
    fun getData (s, i, n) = Word8Vector.extract (s, i, SOME n)
    fun getString (s, i, n) = Byte.unpackStringVec (s, i, SOME n)

  (* the different family codes (from X.h and xc/lib/Xau/Xauth.h) *)
    val familyInternet	= 0
    val familyDECnet	= 1
    val familyChaos	= 2
    val familyLocal	= 256
    val familyWild	= 65535

  (* return the default name of the authentication file (either
   * specified by the XAUTHORITY environment variable, or the
   * file $HOME/.Xauthority.  If neither XAUTHORITY or HOME 
   * are defined, then ".Xauthority" is returned.
   *)
    fun authFileName () = (case (OS.Process.getEnv "XAUTHORITY")
	   of (SOME fname) => fname
	    | NONE => (case (OS.Process.getEnv "HOME")
		 of (SOME path) => path ^ "/.Xauthority"
		  | NONE => ".Xauthority"
		(* end case *))
	  (* end case *))

  (* read the entire contents of a file *)
    fun readFile file = let
	  val instrm = BinIO.openIn file
	  val contents = BinIO.inputAll instrm
	  in
	    BinIO.closeIn instrm;
	    contents
	  end

  (* extract an authentication entry from a data string *)
    fun extractAuth contents = let
	  val len = Word8Vector.length contents
	  fun getLen start = get16(contents, start-2)
	  fun extract offset = if (offset < len)
		then let
		  val addrStart = 4 + offset
		  val addrLen = getLen addrStart
		  val dpyStart = addrStart + addrLen + 2
		  val dpyLen = getLen dpyStart
		  val nameStart = dpyStart + dpyLen + 2
		  val nameLen = getLen nameStart
		  val dataStart = nameStart + nameLen + 2
		  val dataLen = getLen dataStart
		  val next = dataStart + dataLen
		  in
		    SOME(EXB.AUTH{
			family = get16 (contents, offset),
			addr = getString (contents, addrStart, addrLen),
			dpy = getString (contents, dpyStart, dpyLen),
			name = getString (contents, nameStart, nameLen),
			data = getData (contents, dataStart, dataLen)
		      }, next)
		  end
		else NONE
	  in
	    extract
	  end

  (* searches the default authentication file for the first entry that
   * matches the family, network address and display number.  If no
   * such match is found, then NONE is returned.  The * value familyWild
   * matches anything, as do the empty strings when given for addr or dpy.
   *)
    fun getAuthByAddr {family, dpy, addr} = let
	  val extractAuth = extractAuth (readFile (authFileName()))
	  fun cmpStr ("", _) = true
	    | cmpStr (_, "") = true
	    | cmpStr (a, b) = (a = b)
	  fun chkAuth (EXB.AUTH{family=f, dpy=d, addr=a, ...}) = (
		((family = familyWild) orelse (f = familyWild) orelse (family = f))
		andalso cmpStr(dpy, d)
		andalso cmpStr(addr, a))
	  fun look offset = (case (extractAuth offset)
		 of NONE => NONE
		  | (SOME(auth, next)) =>
		      if (chkAuth auth) then (SOME auth) else look next
		(* end case *))
	  in
	    look 0
	  end
	    handle _ => NONE

  (* this similar to getAuthByAddr, except that a list of acceptable
   * authentication methods is specified by the list authNames.  It
   * returns the matching authentication info that matches the earliest
   * name on the list.  NONE is returned if no match is found.
   *)
    fun getBestAuthByAddr {family, addr, dpy, authNames} = let
	  val extractAuth = extractAuth (readFile (authFileName()))
	  fun cmpStr ("", _) = true
	    | cmpStr (_, "") = true
	    | cmpStr (a, b) = (a = b)
	  fun chkAuth (EXB.AUTH{family=f, dpy=d, addr=a, ...}) = (
		((family = familyWild) orelse (f = familyWild) orelse (family = f))
		andalso cmpStr(dpy, d)
		andalso cmpStr(addr, a))
	  fun look (offset, bestRank, best) = (case (extractAuth offset)
		 of NONE => best
		  | (SOME(auth as EXB.AUTH{name, ...}, next)) =>
		      if (chkAuth auth)
		        then let
			  fun chkName ([], _) = look (next, bestRank, best)
			    | chkName (n::r, rank) =
			        if (rank < bestRank)
				  then if (name = n)
				    then look (next, rank, SOME auth)
				    else chkName (r, rank+1)
				  else look (next, bestRank, best)
			  in
			    chkName (authNames, 0)
			  end
		        else look (next, bestRank, best)
		(* end case *))
	  in
	    look (0, length authNames, NONE)
	  end
	    handle _ => NONE

  (* read the specified authentication file and return a list of
   * entries that satisfy the given predicate.
   *)
    fun readAuthFile checkAuth file = let
	  val extractAuth = extractAuth (readFile file)
	  fun filter (offset, l) = (case (extractAuth offset)
		 of NONE => rev l
		  | (SOME(auth, next)) => if (checkAuth auth)
		      then filter (next, auth::l)
		      else filter (next, l)
		(* end case *))
	  in
	    filter (0, [])
	  end

  end;

