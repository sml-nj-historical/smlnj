(* hide.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Hide the eXene internals and define the top-level structures.
 *)

(* hide internal eXene structures *)
structure XProtTypes = struct end;
structure KeyBut = struct end;
structure XEventTypes = struct end;
structure XErrors = struct end;
structure HashXId = struct end;
structure XPrint = struct end;
structure XCvtFuns = struct end;
structure XReply = struct end;
structure XRequest = struct end;
structure XIo = struct end;
structure XShutdown = struct end;
structure XDisplay = struct end;
structure XWin = struct end;
structure Keysym = struct end;
structure Keymap = struct end;
structure PenRep = struct end;
structure WinRegistry = struct end;
structure FontBase = struct end;
structure FontServer = struct end;
structure GCServer = struct end;
structure DrawMaster = struct end;
structure Display = struct end;
structure DrawTypes = struct end;
structure HashWindow = struct end;
structure Pen = struct end;
structure Draw = struct end;
structure ColorServer = struct end;
structure Pixmap = struct end;
structure Image = struct end;
structure Tile = struct end;
structure Cursor = struct end;
structure XAtoms = struct end;
structure XProps = struct end;
structure WindowEnv = struct end;
structure TopLevelWin = struct end
structure Window = struct end;
structure KeysymTranslation = struct end;

(* open EXene *)
structure EXeneBase = EXene.EXeneBase;
structure Font = EXene.Font;
structure Drawing = EXene.Drawing;
structure ICCC = EXene.ICCC;
structure Interact = EXene.Interact;
structure EXeneWin = EXene.EXeneWin;
structure StdCursor = EXene.StdCursor;

