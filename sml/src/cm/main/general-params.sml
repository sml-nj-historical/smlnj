(*
 * "General" parameters that may differ from invocation to invocation of
 * CM.  The "info" type bundles them up so they can be passed around
 * more conveniently.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure GeneralParams = struct

    type param = { penv: SrcPath.env,
		   fnpolicy: FilenamePolicy.policy,
		   symval: string -> { get: unit -> int option,
				       set: int option -> unit },
		   keep_going: bool }

    type info = { param: param,
		  groupreg: GroupReg.groupreg,
		  errcons: PrettyPrint.ppconsumer }

    fun bind { param = { penv, fnpolicy, symval, keep_going },
		groupreg, errcons } rb =
	{ param = { penv = SrcPath.bind penv rb,
		    fnpolicy = fnpolicy,
		    symval = symval,
		    keep_going = keep_going },
	  groupreg = groupreg, errcons = errcons }
end
