(* scrollbar.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Scrollbar widget.
 *)

structure Scrollbar : SCROLLBAR = struct

  structure CML = CML
  structure W = Widget

  open CML Geometry EXeneBase Interact Widget ScrollView

    val min = Int.min
    val max = Int.max

  datatype scroll_evt = 
    ScrUp of real
  | ScrDown of real
  | ScrStart of real
  | ScrMove of real
  | ScrEnd of real

  datatype scrollbar = 
    Scrollbar of {
      widget : Widget.widget,
      evt : scroll_evt CML.event,
      setvals : {top : real option, sz : real option } -> unit
    }

  datatype mseMsg = 
    Grab of point 
  | Move of point
  | Ungrab of point
  | UpGrab of point
  | UpUngrab of point
  | DownGrab of point
  | DownUngrab of point

  datatype rqst =
    SetVals of {top : real option, sz : real option }
  | DoRealize of {
      env : in_env,
      win : window,
      sz : size
    }

  type scroll = {
    curx : int,
    swid : int
  }

  val initSize = 1000
  val minSwid = 8

  fun newVals (me as {curx, swid}, size, arg) =
    case arg of
      {top=NONE, sz=NONE} => me
    | {top=SOME top, sz=NONE} => 
        {curx=min(size-swid,max(0,floor(top * (real size)))),swid=swid}
    | {top=NONE, sz=SOME sz} => 
        {curx=curx,swid=min(size-curx,max(minSwid,ceil(sz * (real size))))}
    | {top=SOME top, sz=SOME sz} => 
      let
        val sz' = min(size,max(minSwid,ceil(sz * (real size))))
        val top' = min(size-sz',max(0,floor(top * (real size))))
      in
        {curx=top',swid=sz'}
      end

  fun mkScroll (root, dim, color, bg, {bounds_of, realize} : scroll_view) = let
    val _ = if dim < 4
              then LibBase.failure{module="Scrollbar",func="mkScroll",msg="dim < 4"}
              else ()
    val scr = screenOf root
    val msechan = channel ()  (* mouse to scrollbar *)
    val valchan = channel ()  (* scrollbar to user *)
    val reqchan = channel ()  (* user to scrollbar *)
    val mevt = recvEvt msechan
    val reqevt = recvEvt reqchan

         (* mouse reader *)
    fun mseP m = let

      fun downLoop (movef,upf) = let
        fun loop () =
          case msgBodyOf (sync m) of 
            MOUSE_LastUp {pt,...} => upf pt
          | MOUSE_Motion {pt,...} => (movef pt;loop ())
          | _ => loop ()
      in
        loop ()
      end

      fun loop () =
        case msgBodyOf (sync m) of 
          MOUSE_FirstDown {but=btn as MButton 1,pt,...} => (
            send (msechan, UpGrab pt);
            downLoop (fn _ => (), fn p => send(msechan, UpUngrab p));
            loop ()
          )
        | MOUSE_FirstDown {but=btn as (MButton 2),pt,...} => (
            send (msechan, Grab pt);
            downLoop (
              fn p => send(msechan, Move p),
              fn p => send(msechan, Ungrab p)
            );
            loop ()
          )
        | MOUSE_FirstDown {but=btn as MButton 3,pt,...} => (
            send (msechan, DownGrab pt);
            downLoop (fn _ => (),fn p => send(msechan, DownUngrab p));
            loop ()
          )
        | _ => loop ()
    in
      loop ()
    end

    val config = realize (root,color)

    fun realizeScroll {env=inenv, win, sz=winsz} me = let
      val InEnv{m,ci,...} = Interact.ignoreKey inenv
      val config = config (Drawing.drawableOfWin win)

      fun reconfig ({curx,swid},size,sz,redraw) = let
            val data as {size=size',draw,...} = config sz
            val curx' = (curx*size') div size
            val swid' = (swid*size') div size
            in
              if redraw then draw (curx',swid') else ();
              cmdP ({curx=curx', swid=swid'}, data)
            end

      and cmdP (me, {size,coord,draw,move}) = let

        fun sendVal (v, f) = send (valchan, f ((real v)/(real (size))))

        fun moveSlide (me as {curx,swid}, x) = let
          val curx' = min(size-swid,max(0,x))
        in
          if curx' <> curx then let
          in
            move (curx, swid, curx', swid);
            {curx=curx',swid=swid}
          end
          else me
        end

        fun handleCIEvt (evt, me : scroll) =
          case msgBodyOf evt of
            CI_OwnDeath => me
          | CI_Redraw _ => (draw (#curx me, #swid me); me)
          | CI_Resize (RECT{wid,ht,...}) => 
              reconfig (me, size, SIZE{wid=wid,ht=ht},true)
          | _ => me

        fun handleReqEvt (SetVals arg, me as {curx,swid}) = 
          let
            val me' as {curx=curx',swid=swid'} = newVals (me, size, arg)
          in
            if curx <> curx' orelse swid <> swid' then
               move (curx, swid, curx', swid')
            else ();
            me'
          end
          | handleReqEvt (DoRealize _,me) = me


        fun handleMEvt (Grab p, me as {curx,swid}) = let
          val x = coord p
          val maxx = size - swid
          val (xoff, me') =
            if curx <= x andalso x < curx + swid then ((x - curx), me)
            else if 0 <= x andalso x < maxx+swid then 
              let
                val curx' = min(maxx, max(0, x - (swid div 2)))
              in
                (x - curx', moveSlide (me, curx'))
              end
            else if x < 0 then (swid div 2, moveSlide (me, 0))
            else (swid div 2, moveSlide (me, maxx))
  
          fun hMEvt (Ungrab x, me) = 
            let
              val me' = moveSlide (me, (coord x) - xoff)
            in
              sendVal (#curx me', ScrEnd);
              (false, me')
            end
            | hMEvt (Move x, me) =
            let
              val me' = moveSlide (me, (coord x) - xoff)
            in
              if (#curx me <> #curx me') then sendVal (#curx me', ScrMove)
              else ();
              (true, me')
            end
            | hMEvt (_, me) = (true, me)  (* protocol error *)
  
          fun loop me = select [
              wrap (reqevt,fn evt => loop (handleReqEvt (evt, me))),
              wrap (ci, fn evt => loop (handleCIEvt (evt, me))),
              wrap (mevt, fn evt => 
                case hMEvt (evt, me) of
                  (true, m) => loop m
                | (false, m) => m)
            ]
                
        in
          sendVal (#curx me', ScrStart);
          loop me' end
        | handleMEvt (UpGrab _,me) = let

          fun hMEvt (UpUngrab x, me) = (sendVal (coord x, ScrUp); (false, me))
            | hMEvt (_, me) = (true, me)  (* protocol error *)

          fun loop me = 
            select [
              wrap (reqevt,fn evt => loop (handleReqEvt (evt, me))),
              wrap (ci, fn evt => loop (handleCIEvt (evt, me))),
              wrap (mevt, fn evt => 
                case hMEvt (evt, me) of
                  (true, m) => loop m
                | (false, m) => m)
            ]
          in
            loop me
          end
        | handleMEvt (DownGrab p,me) = let

          fun hMEvt (DownUngrab x, me) = (sendVal (coord x, ScrDown); (false, me))
            | hMEvt (_, me) = (true, me)  (* protocol error *)

          fun loop me = 
            select [
              wrap (reqevt,fn evt => loop (handleReqEvt (evt, me))),
              wrap (ci, fn evt => loop (handleCIEvt (evt, me))),
              wrap (mevt, fn evt => 
                case hMEvt (evt, me) of
                  (true, m) => loop m
                | (false, m) => m)
            ]
          in
            loop me
          end
        | handleMEvt (_,me) = me   (* protocol error *)

        fun cmdLoop me =
          cmdLoop (select [
            wrap (reqevt, fn evt => handleReqEvt (evt, me)),
            wrap (mevt, fn evt => handleMEvt (evt, me)),
            wrap (ci, fn evt => handleCIEvt (evt, me))
          ])
      in
        cmdLoop me
      end
    in
      spawn (fn () => mseP m);
      spawn (fn() => (reconfig (me, initSize, winsz,false);()));
      ()
    end

    fun initLoop vals =
      case recv reqchan of
        SetVals arg => initLoop (newVals (vals, initSize, arg))
      | DoRealize arg => realizeScroll arg vals
  in
    spawn (fn () => initLoop {curx=0,swid=initSize});
    Scrollbar {
      widget = 
        mkWidget{
          root=root,
          args= fn () => {background = bg},
          boundsOf=bounds_of dim, 
          realize=fn arg => send(reqchan, DoRealize arg)
        }, 
      evt = recvEvt valchan,
      setvals = (fn arg => send (reqchan, SetVals arg))
    }
  end

  val attrs = [
      (Attrs.attr_width,          Attrs.AT_Int,     Attrs.AV_Int 12),
      (Attrs.attr_background,     Attrs.AT_Color,   Attrs.AV_Str "gray"),
      (Attrs.attr_color,          Attrs.AT_Color,   Attrs.AV_NoValue)
    ]

  fun scrollbar scrollView (root,view,args) = let
        val attrs = W.findAttr (W.attrs(view,attrs,args))
        val sz = Attrs.getInt(attrs Attrs.attr_width)
        val bg = Attrs.getColor(attrs Attrs.attr_background)
        val color = case Attrs.getColorOpt(attrs Attrs.attr_color) of
                      NONE => bg
                    | SOME c => c
        in mkScroll(root,sz,color,SOME bg,scrollView) end

  val hScrollbar = scrollbar horzScrollbar
  val vScrollbar = scrollbar vertScrollbar

  fun mkHScrollbar root {sz, color} = let
        val color = case color of 
                      SOME c => c
                    | NONE => colorOfScr (screenOf root) (CMS_Name "gray")
        in mkScroll (root, sz, color, NONE, horzScrollbar) end

  fun mkVScrollbar root {sz, color} = let
        val color = case color of 
                      SOME c => c
                    | NONE => colorOfScr (screenOf root) (CMS_Name "gray")
        in mkScroll (root, sz, color, NONE, vertScrollbar) end

  fun widgetOf (Scrollbar {widget,...}) = widget
  fun evtOf (Scrollbar {evt,...}) = evt
  fun setVals (Scrollbar{setvals,...}) arg = setvals arg

end (* ScrollBar *)


