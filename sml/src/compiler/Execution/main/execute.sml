(* execute.sml
 *
 * (C) 2001 Lucent Technologies, Bell labs
 *)

(*****************************************************************************
 *                        EXECUTING THE EXECUTABLE                           *
 *****************************************************************************)

structure Execute : sig
    val mkexec : CodeObj.csegments -> CodeObj.executable
    val execute : { executable: CodeObj.executable,
		    imports: ImportTree.import list,
		    exportPid: PersStamps.persstamp option,
		    dynenv: DynamicEnv.env } -> DynamicEnv.env
end = struct
    structure Obj = Unsafe.Object
    type object = Obj.object

    val say = Control_Print.say
    fun bug s = ErrorMsg.impossible ("Execute: " ^ s)


    (** turn the byte-vector-like code segments into an executable closure *)
    fun mkexec (cs : CodeObj.csegments) = let
	val ex = CodeObj.exec (#c0 cs)
	val nex =
	    if (Word8Vector.length (#data cs) > 0) then
		(fn ivec =>
		    ex (Obj.mkTuple (Obj.toTuple ivec @
				     [CodeObj.mkLiterals (#data cs)])))
	    else (fn ivec => ex ivec)
    in
	foldl (fn (c, r) => (CodeObj.exec c) o r) nex (#cn cs)
    end 

    (** perform the execution of the excutable, output the new dynenv *)
    fun execute {executable, imports, exportPid, dynenv} = let
	val args : object = let
            fun selObj (obj, i) =
		Obj.nth(obj, i)
		handle _ => bug "unexpected linkage interface in execute"
            fun getObj ((p, n), zs) = let
		fun get (obj, ImportTree.ITNODE [], z) = obj::z
                  | get (obj, ImportTree.ITNODE xl, z) = let
			fun g ((i, n), x) = get (selObj(obj, i), n, x)
                    in foldr g z xl
                    end
                val obj =
		    case  DynamicEnv.look dynenv p of
			SOME obj => obj
		      | NONE => 
			(say ("lookup " ^ (PersStamps.toHex p) ^ "\n");
			 raise CompileExn.Compile
				  "imported objects not found or inconsistent")
            in get(obj, n, zs)
            end
	in Obj.mkTuple (foldr getObj [] imports)
        end
	val result : object = executable args
    in case exportPid of
	   NONE => DynamicEnv.empty
	 | SOME p => DynamicEnv.singleton (p, result)
    end

    val execute = Stats.doPhase (Stats.makePhase "Execute") execute
end
