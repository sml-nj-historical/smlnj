(* pp-desc-fn.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * This interface provides a declarative way to specify pretty-printing.
 *)

functor PPDescFn (S : sig
    include PP_STREAM
      where type indent = PPDesc.indent
      where type pp_desc = (token, style, device) PPDesc.pp_desc
  end) :> PP_DESC =
  struct

    structure PPS = S
    structure D = PPDesc

    type pp_desc = PPS.pp_desc
    type token = PPS.token
    type style = PPS.style
    type indent = PPS.indent

    val hBox    = D.HBox
    val vBox    = D.VBox
    val hvBox   = D.HVBox
    val hovBox  = D.HOVBox
    val box     = D.Box
    val token   = D.Token
    val string  = D.String
    val style   = D.Style
    val break   = D.Break
    fun space n = D.Break{nsp = n, offset = 0}
    val cut     = D.Break{nsp = 0, offset = 0}
    val newline = D.NewLine
    val control = D.Control

  end;

