 (* pp-init.sml
 *
 * COPYRIGHT (c) 2003 The SML/NJ Fellowship
 *
 * An implementation of SML/NJ's PP interface.
 *   - This is an (almost) literal copy of the original code in
 *     smlnj-lib/PP/examples/old-pp.sml
 *)


signature PRETTYPRINT =
sig 
  include PP_STREAM
  val with_pp : device -> (stream -> unit) -> unit
  val pp_to_string : int -> (stream -> 'a -> unit) -> 'a -> string
end

structure PrettyPrint : PRETTYPRINT =
struct

  type ppconsumer = {
      consumer : string -> unit,
      linewidth : int,
      flush : unit -> unit
    }

  structure Dev =
  struct
    type device = ppconsumer
    type style = unit
    fun sameStyle _ = true
    fun pushStyle _ = ()
    fun popStyle _ = ()
    fun defaultStyle _ = ()
    fun depth _ = NONE
    fun lineWidth {consumer, linewidth, flush} = SOME linewidth
    fun textWidth _ = NONE
    fun space ({consumer, linewidth, flush}, n) =
          consumer (StringCvt.padLeft #" " n "")
    fun newline {consumer, linewidth, flush} = consumer "\n"
    fun string ({consumer, linewidth, flush}, s) = consumer s
    fun char ({consumer, linewidth, flush}, c) = consumer(str c)
    fun flush {consumer, linewidth, flush} = flush()
  end

  structure PP = PPStreamFn
      (structure Token = StringToken
       structure Device = Dev)

  open PP

  fun with_pp device (f: PP.stream -> unit) =
      let val ppstrm = PP.openStream device
       in f ppstrm;
          PP.closeStream ppstrm
      end

  fun pp_to_string wid ppFn obj =
      let val l = ref ([] : string list)
          fun attach s = l := s :: !l
          val device = {consumer = attach, linewidth = wid, flush = fn()=>()}
       in with_pp device
            (fn ppStrm => ppFn ppStrm obj);
          String.concat(List.rev(!l))
      end

end (* structure PrettyPrint *)
