(* router.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Generic event routers. 
 *)

signature ROUTER = 
  sig

    structure EXB : EXENE_BASE
    structure Interact : INTERACT

    exception NotFound

    type router

    val mkRouter : Interact.in_env * Interact.out_env * 
      (EXB.window * Interact.out_env) list -> router

    val addChild : router -> EXB.window * Interact.out_env -> unit
    val delChild : router -> EXB.window -> unit
    val getChildEnv : router -> EXB.window -> Interact.out_env
      (* Return environment associated in router with given window.
       * Raise NotFound if not found.
       *)

    val routePair : Interact.in_env * Interact.out_env * Interact.out_env -> unit

  end (* ROUTER *)

structure Router : ROUTER = struct

  structure EXB = EXeneBase
  structure Interact = Interact

  exception NotFound

  open CML EXeneBase Interact

  datatype route_req = 
    AddChild of (window * out_env)
  | DelChild of window
  | GetChild of window

  datatype router = Router of {
    reqch : route_req chan,
    replych : out_env option chan
  }

  (* The router is constructed with an in_env, out_env for a
   * composite widget and an initial distribution
   * list. The router listens for an event on the input environment, 
   * resolves the event to an output environment, and passes the event
   * along.
   *)
  fun mkRouter (InEnv{m, k, ci,...}, myOut, outList) = let
    val routeReqCh = channel() and routeReplyCh = channel()
          
    val winMap = newMap()
    val find = lookup winMap
    (* val findMsg = addrLookup winMap *)
    fun findMsg m = addrLookup winMap m
    val insert = insert winMap
    val remove = remove winMap
          
    fun mEvt (OutEnv {m,...}) = m
    fun kEvt (OutEnv {k,...}) = k
    fun ciEvt (OutEnv {ci,...}) = ci
          
    fun handleReq (AddChild item) = insert item
      | handleReq (DelChild w) = ((remove w; ()) handle _ => ())
      | handleReq (GetChild w) = send(routeReplyCh, (SOME(find w)) handle _ => NONE)
          
    fun handleEvt proj msg = (
      case stripMsg msg of 
        Here _ => select [
            proj myOut msg,
            wrap (recvEvt routeReqCh, fn req => (handleReq req; handleEvt proj msg))
          ]
      | ToChild msg' => sync (proj (findMsg msg') msg'))
          
    val evt = choose [
      wrap (recvEvt routeReqCh, handleReq),
      wrap (m, handleEvt mEvt),
      wrap (k, handleEvt kEvt),
      wrap (ci, handleEvt ciEvt)
    ]
          
    fun loop () = (sync evt; loop ())
    fun init [] = ()
      | init (item::rest) = (insert item; init rest)
  in
    init outList;
    XDebug.xspawn ("Router", loop);
    Router {reqch = routeReqCh, replych = routeReplyCh}
  end
          
  fun addChild (Router{reqch,...}) arg = send (reqch, AddChild arg)
  fun delChild (Router{reqch,...}) arg = send (reqch, DelChild arg)
  fun getChildEnv (Router{reqch, replych}) arg = (
    send (reqch, GetChild arg);
    case recv replych of
      NONE => raise NotFound
    | SOME e => e
  )
        
  (* Simple router for a composite widget with a single child.
   *)
  fun routePair (InEnv{m, k, ci,...}, parentOut, childOut) = let
          
    fun mEvt (OutEnv {m,...}) = m
    fun kEvt (OutEnv {k,...}) = k
    fun ciEvt (OutEnv {ci,...}) = ci
          
    fun handleEvt proj msg =
      case stripMsg msg of
        Here _ => sync (proj parentOut msg)
      | ToChild msg' => sync (proj childOut msg')
          
    fun loop () =
      loop (sync(choose[
        wrap (m, handleEvt mEvt),
        wrap (k, handleEvt kEvt),
        wrap (ci, handleEvt ciEvt)
      ]))
          
  in
    XDebug.xspawn ("Router2", loop);
    ()
  end
          
end (* Router *)

