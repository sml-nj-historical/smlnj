(*
 * Instantiating the tools library for CM.
 *
 *   Copyright (c) 2000 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Tools = ToolsFn (val load_plugin = CM.load_plugin)
