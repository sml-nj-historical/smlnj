signature STDTOOL = sig
    val command : string option -> string
end

functor StdShellCmdTool
    (val tool : string
     val class : string
     val suffixes : string list
     val command : string * string
     val extensionStyle : Tools.extensionStyle
     val sml : bool) : STDTOOL =
struct
    val command = EnvConfig.new SOME command
    fun rule (f, ctxt) = let
	val targetfiles = Tools.extend extensionStyle f
	val mkTarget =
	    if sml then (fn tf => (tf, SOME "sml")) else (fn tf => (tf, NONE))
	val targets = map mkTarget targetfiles
	fun runcmd () = let
	    val cmd = concat [command NONE, " ", f]
	    val _ = Say.vsay (concat ["[", cmd, "]\n"])
	in
	    if OS.Process.system cmd = OS.Process.success then ()
	    else raise Tools.ToolError { tool = tool, msg = cmd }
	end
	fun rfun () =
	    (if Tools.outdated tool (targetfiles, f) then runcmd () else ();
	     targets)
    in
	ctxt rfun
    end

    val _ = Tools.registerClass (class, rule)
				
    fun sfx s =
	Tools.registerClassifier
	  (Tools.stdSfxClassifier { sfx = s, class = class })

    val _ = app sfx suffixes
end
