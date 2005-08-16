(* root.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 * Definitions for widget root.
 *)

signature ROOT =
  sig

    structure EXB : EXENE_BASE

    type root
    type style

    val mkRoot : (string * EXB.authentication option) -> root
    val delRoot : root -> unit
    val sameRoot : root * root -> bool
    val displayOf : root -> EXB.display
    val screenOf : root -> EXB.screen
    val shades : root -> EXB.color -> WidgetBase.shades
    val tile : root -> string -> EXB.tile
    val colorOf : root -> EXB.color_spec -> EXB.color
    val openFont : root -> string -> EXB.font
    val stdCursor : root -> EXeneBase.std_cursor -> EXeneBase.cursor
    val ringBell : root -> int -> unit

    val sizeOfScr    : root -> Geometry.size
    val sizeMMOfScr  : root -> Geometry.size
    val depthOfScr   : root -> int

    val isMonochrome : root -> bool

    val styleOf : root -> style
    val styleFromStrings : root * string list -> style

  end (* ROOT *)

structure Root =
  struct

    structure EXB = EXeneBase

  (* Root object, corresponding to display/screen pair.
   *  server = ""          => "unix:0.0"
   *         = ":d"        => "unix:d.0"
   *         = "host:d"    => "host:d.0"
   *         = "host:d.s"  => "host:d.s"
   * where host is an internet address (e.g., "128.84.254.97") or "unix".
   *
   * At present, screen is always the default screen.
   *)
    type style = Styles.style

    datatype root = Root of {
      id : unit ref,
      scr : EXB.screen,
      mkshade : EXB.color -> ShadeServer.shades,
      mktile : string -> EXB.tile,
      style : style,
      idGen : unit -> int
    }

    val initImages = [
          (Quark.quark "lightGray", Images.lightGray),
          (Quark.quark "gray", Images.gray)
        ]

    fun mkRoot (server, auth) = let
          val scr = EXB.defaultScreenOf (EXB.openDisplay (server, auth))
          val idChan = CML.channel ()
          fun idLoop i = (CML.send(idChan,i);idLoop(i+1))
          val is = ImageServer.mkImageServer initImages
          val ts = TileServer.mkTileServer (scr,ImageServer.getImage is)
          val ss = ShadeServer.mkShadeServer scr
          val tilef = TileServer.getTile ts
          in
            CML.spawn (fn () => idLoop 0);
            Root {
                id = ref (), 
                scr = scr, 
                style = Styles.emptyStyle {scr=scr,tilef=tilef}, 
                mktile = tilef,
                mkshade = ShadeServer.getShades ss,
                idGen = fn () => CML.recv idChan}
          end

    fun screenOf (Root {scr,...}) = scr
    fun displayOf (Root {scr,...}) = EXB.displayOfScr scr
    fun delRoot root = EXB.closeDisplay (displayOf root)
    fun sameRoot (Root {id,...},Root{id=id',...}) = id = id'
    fun shades (Root{mkshade,...}) c = mkshade c
    fun tile (Root{mktile,...}) s = mktile s
    fun colorOf (Root{scr,...}) color_spec = EXB.colorOfScr scr color_spec
    fun openFont (Root{scr,...}) = Font.openFont (EXB.displayOfScr scr)
    fun stdCursor (Root{scr,...}) = EXB.stdCursor (EXB.displayOfScr scr)
    fun ringBell (Root{scr,...}) = EXB.ringBell (EXB.displayOfScr scr)
    fun sizeOfScr (Root{scr,...}) = EXB.sizeOfScr scr
    fun sizeMMOfScr (Root{scr,...}) = EXB.sizeMMOfScr scr
    fun depthOfScr (Root{scr,...}) = EXB.depthOfScr scr

    fun styleOf (Root {style,...}) = style

    fun isMonochrome (Root{scr,...}) = 
          EXB.displayClassOfScr scr = EXB.StaticGray andalso 
          EXB.depthOfScr scr = 1

    fun styleFromStrings (Root{scr,mktile,...}, sl) =
          Styles.styleFromStrings ({scr=scr,tilef=mktile},sl)

  end (* Root *)
