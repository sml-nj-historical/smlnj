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

    type param = { primconf : Primitive.configuration,
		   pcmode : PathConfig.mode,
		   fnpolicy: FilenamePolicy.policy,
		   keep_going: bool,
		   pervasive: GenericVC.Environment.environment,
		   corenv: GenericVC.BareEnvironment.staticEnv,
		   pervcorepids: PidSet.set }

    type info = { param: param,
		  groupreg: GroupReg.groupreg,
		  errcons: PrettyPrint.ppconsumer }
end
