(* dummy implementation of functor LinkCM *)

functor LinkCM () = struct

    structure CM = struct
	fun parse s = let
	    val c = AbsPath.cwdContext ()
	    val p = AbsPath.native { context = AbsPath.cwdContext (),
				     spec = s }
	in
	    CMParse.parse p
	end
    end

    structure CMB = struct
	fun setRetargetPervStatEnv x = ()
	fun wipeOut () = ()
	fun make' _ = ()
    end
end

signature CMTOOLS = sig end
signature COMPILATION_MANAGER = sig end
