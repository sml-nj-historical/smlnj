structure ModPropList =
struct

  val { getFn = sigBoundeps, setFn = setSigBoundeps, ... } = let
      fun holder (e: Modules.sigrec) = #properties e
      fun init _ = NONE: (EntPath.entPath * Types.pkind) list option
  in
      PropList.newProp (holder, init)
  end
	
end