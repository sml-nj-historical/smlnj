(*
 * Configurable path anchors for new CM.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

signature PATHCONFIG = sig

    type mode

    val hardwire : (string * string) list -> mode
    val envcfg : (string * string EnvConfig.getterSetter) list -> mode
    val bootcfg : string -> mode

    val configAnchor : mode -> string -> (unit -> string) option
end

(*
 * The names of config anchors must be names of actual files.
 * Function configAnchor will map the name of the anchor to
 * the directory that contains the corresponding file.
 *)
structure PathConfig :> PATHCONFIG = struct

    type mode = string -> (unit -> string) option

    fun hardwire [] (a: string) = NONE
      | hardwire ((a', v) :: t) a =
	if a = a' then SOME (fn () => v) else hardwire t a

    fun envcfg [] (a: string) = NONE
      | envcfg ((a', gs) :: t) a =
	if a = a' then SOME (fn () => EnvConfig.getSet gs NONE)
	else envcfg t a

    fun bootcfg bootdir a = let
	fun isDir x = OS.FileSys.isDir x handle _ => false
	val d = OS.Path.concat (bootdir, a)
    in
	if isDir d then SOME (fn () => d) else NONE
    end

    fun configAnchor m s = m s
end
