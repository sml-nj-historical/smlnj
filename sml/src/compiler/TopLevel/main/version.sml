(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* version.sml *)

structure Version : sig

    val version : {
            system : string,      	(* the system title *)
	    version_id : int list,	(* the version number *)
            date : string         	(* date of creation *)
	  }

    val banner : string

  end = struct

    val version = {
	    system = "Standard ML of New Jersey ",
	    version_id = [110, 30],
	    date = "November 3, 2000"
	  }

    fun f ([], l) = l
      | f ([x : int], l) = (Int.toString x)::l
      | f (x::r, l) = (Int.toString x) :: "." :: f(r, l)

    val banner = concat (
	    #system version :: "v" :: 
	    f (#version_id version, [" [FLINT v1.5], ", #date version]))

  end
