(* generic-sock.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure GenericSock : GENERIC_SOCK =
  struct
    structure PS = PreSock

    fun sockFn x = CInterface.c_function "SMLNJ-Sockets" x

    val c_socket	: (int * int * int) -> PS.sockFD
	  = sockFn "socket"
    val c_socketPair	: (int * int * int) -> (PS.sockFD * PS.sockFD)
	  = sockFn "socketPair"

    fun fd2sock fd = PS.SOCK { fd = fd, nb = ref false }

  (* create sockets using default protocol *)
    fun socket (PS.AF(af, _), PS.SOCKTY(ty, _)) =
	  fd2sock (c_socket (af, ty, 0))
    fun socketPair (PS.AF(af, _), PS.SOCKTY(ty, _)) = let
	  val (s1, s2) = c_socketPair (af, ty, 0)
	  in
	    (fd2sock s1, fd2sock s2)
	  end

  (* create sockets using the specified protocol *)
    fun socket' (PS.AF(af, _), PS.SOCKTY(ty, _), prot) =
	  fd2sock (c_socket (af, ty, prot))
    fun socketPair' (PS.AF(af, _), PS.SOCKTY(ty, _), prot) = let
	  val (s1, s2) = c_socketPair (af, ty, prot)
	  in
	    (fd2sock s1, fd2sock s2)
	  end

  end
