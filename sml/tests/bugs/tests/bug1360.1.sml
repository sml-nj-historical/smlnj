(* bug1360.1.sml *)

signature EXTERN =
 sig
  type T
 end 

signature PRINTABLE =
 sig
  type T
 end 

signature KEY =
 sig
  include PRINTABLE
 end 

signature EXTERN_KEY =
 sig
  include EXTERN
 end 

signature EXTERNAL =
 sig
  include PRINTABLE
 end 

signature PROTOCOL =
 sig

  structure Address: KEY
  structure Pattern: KEY
  structure Connection_Key: KEY

  structure Outgoing: EXTERNAL

  type connection_extension
  type listen_extension
  type session_extension

  datatype connection = C of {send: unit -> unit,
			      abort: unit -> unit,
			      extension: connection_extension}

  datatype listen = L of {stop: unit -> unit, extension: listen_extension}

  datatype handler = H of Connection_Key.T
                  -> {connection_handler: connection -> unit,
	              data_handler: connection * unit -> unit,
	              status_handler: connection * unit -> unit}

  datatype session = S of {connect: Address.T * handler -> unit,
			   listen: Pattern.T * handler * unit -> listen,
			   extension: session_extension}

  val session: (session -> 'a) -> 'a

 end ;

functor Connection (structure Address: KEY
		    structure Pattern: KEY
		    structure Connection_Key: KEY
		    structure Outgoing: EXTERNAL
		    type connection_extension
		    type listen_extension
		    type session_extension
		    type connection_state
		    type protocol_state): PROTOCOL =
 struct

  structure Address = Address
  structure Pattern = Pattern
  structure Connection_Key = Connection_Key
  structure Outgoing = Outgoing

  type connection_extension = connection_extension
  type listen_extension = listen_extension
  type session_extension = session_extension

  datatype connection = C of {send: unit -> unit,
		     abort: unit -> unit,
	             extension: connection_extension}

  datatype listen = L of {stop: unit -> unit, extension: listen_extension}

  datatype handler = H of Connection_Key.T
                  -> {connection_handler: connection -> unit,
	              data_handler: connection * unit -> unit,
	              status_handler: connection * unit -> unit}

  datatype session = S of {connect: Address.T * handler -> unit,
		     listen: Pattern.T * handler * unit -> listen,
	             extension: session_extension}


   exception Y

   fun session _ = raise Y

 end;

functor Tcp (structure Lower: PROTOCOL): PROTOCOL=
 struct

  structure Tcp_Connection =
   struct

    structure Tcp_Key = struct type T = unit end
    structure Tcp_Address = struct type T = unit end
    structure Tcp_Pattern = struct type T = unit end
    structure Tcp_Host_Id = struct type T = unit end
    type host_id = unit
    type port = unit

    structure Outgoing = Lower.Outgoing
    type outgoing = Outgoing.T

    datatype tcp_connection_extension =
	Tcp_Connection_Extension of
	  {urgent_send: Outgoing.T -> unit}

    type session_extension = unit
    type protocol_state = unit
    type connection_state = protocol_state
    datatype transport_listen_extension =
	Listen_Extension of {local_port: port, additional: unit}
    type listen_extension = transport_listen_extension

    type connection_internals_record =
	{tcp_send: (unit -> unit) ref,
	 tcp_abort: (unit -> unit) ref,
	 urgent_send: (Outgoing.T -> unit) ref}

    datatype connection_internals = Connection_Internals of
				      connection_internals_record

    type connection_extension =
          (tcp_connection_extension * connection_internals) ref

    structure Conn =
      Connection (structure Address = Tcp_Address
		  structure Pattern = Tcp_Pattern
		  structure Connection_Key = Tcp_Key
		  structure Outgoing = Outgoing
		  type connection_extension = connection_extension
		  type listen_extension = listen_extension
		  type session_extension = session_extension
		  type connection_state = connection_state
		  type protocol_state = protocol_state)

   end					



  local
   structure Conn:
    sig
     structure Address: KEY
     structure Pattern: KEY
     structure Connection_Key: KEY
     structure Outgoing: EXTERNAL
     type listen_extension
     type session_extension
     datatype listen = L of {stop: unit -> unit, extension: listen_extension}
    end
   = Tcp_Connection.Conn
  in
    open Conn
  end



  structure Conn = Tcp_Connection.Conn

  structure Transport_Address = Tcp_Connection.Tcp_Address
  structure Transport_Pattern = Tcp_Connection.Tcp_Pattern
  structure Transport_Key     = Tcp_Connection.Tcp_Key

  structure Host_Id = Tcp_Connection.Tcp_Host_Id
  type host_id = Tcp_Connection.host_id
  type port    = Tcp_Connection.port

  local
    
    structure Export_Listen_Extension:
      sig
	datatype transport_listen_extension =
	  Listen_Extension of {local_port: port, additional: unit}
      end
    = Tcp_Connection
  in
    open Export_Listen_Extension
  end
  type additional_listen_extension = unit


  
  local
    structure Internals:
     sig datatype connection_internals =
	   Connection_Internals of Tcp_Connection.connection_internals_record
	 datatype tcp_connection_extension =
	   Tcp_Connection_Extension of 
	   {urgent_send: Outgoing.T -> unit}

     end = Tcp_Connection
  in
    open Internals
  end

  type connection_extension = tcp_connection_extension

  datatype connection = C of {send: unit -> unit,
			      abort: unit -> unit,
			      extension: connection_extension}
  datatype handler = H of Connection_Key.T
		  -> {connection_handler: connection -> unit,
		      data_handler: connection * unit -> unit,
		      status_handler: connection * unit -> unit}
  datatype session = S of {connect: Address.T * handler -> unit,
			   listen: Pattern.T * handler * unit -> listen,
			   extension: session_extension}

  exception Y

  exception Bad_Get_Tcb

  fun get_tcb _ = raise Bad_Get_Tcb

  fun add_to_window _ = raise Y

  fun start_user_timer _ = raise Y

  fun stop_user_timer _ = raise Y

  local

   fun match_ack actual registered = false

   val x = ref (SOME ())
   fun signal_send (send, ack) =
	case ! send of
	   NONE => ()
	 | _ => signal_send (send, ack)

  in 
   fun execute_actions (connection as Connection_Internals internals) =
	let fun loop () =
		  (signal_send (x, true);
		   loop ())	
       in loop ()
       end 

  end 

  fun tcp_send_packet (internals, packet, urgent) = raise Y

  fun close_or_abort _ = raise Y

  fun convert_connection _ = raise Y

  fun create_tcb (open_fun, key, conn_send) =
    let fun tcp_send _ = raise Y
	fun urgent_send _ = raise Y
	fun tcp_abort _ = raise Y
	val connection_internals =
	     Connection_Internals {tcp_send = ref tcp_send,
				   tcp_abort = ref tcp_abort,
				   urgent_send = ref urgent_send}
    in connection_internals
    end 

  fun do_connect (key, (conn as Conn.C {send, abort, extension}), open_fun) =
       let val internals = create_tcb (open_fun, key, send)
	   val Connection_Internals {urgent_send, ...} = internals
	   fun urgent_send_fun packet = (! urgent_send) packet
	   fun build_extension () =
		let val tcp_extension =
			   Tcp_Connection_Extension
			      {urgent_send = urgent_send_fun}
		in (tcp_extension, internals)
		end
       in extension := build_extension ()
       end
  fun no_op _ = ()
  val handler_exception_value = {connection_handler = no_op,
				 data_handler = no_op,
				 status_handler = no_op}

  fun tcp_handler (open_fun, tally_open, tally_close, H tcp_handler_fun) =
       let fun conn_handler_fun key =
		let val started = ref false
		    val tcp_connection_handler =
		         ref (no_op: connection -> unit)
		    fun start_connection conn =
			 if ! started then ! tcp_connection_handler
			 else
			  ((started := true;
			    do_connect (key, conn, open_fun);
			    let val {connection_handler, data_handler,
				     status_handler} =
				       tcp_handler_fun key
					 handle x => handler_exception_value
			    in tcp_connection_handler := connection_handler;
			       connection_handler
			    end)
			   handle x =>
				   (! tcp_connection_handler))
		    fun register conn =
			 if ! started then ! tcp_connection_handler
			 else
			  start_connection conn
		    fun conn_data_handler (conn as Conn.C {extension, ...},
					   packet) =
			 (register conn;
			  ())
		    fun conn_status_handler (conn as Conn.C {extension, ...},
					     status_value) =
			 (register conn;
			  ())
		    fun conn_connection_handler (connection as
						 Conn.C {send, abort,
							 extension}) =
			 (let val tcp_connection_handler = register connection 
			      val (tcp_extension,
				   Connection_Internals {tcp_send,
							 tcp_abort, ...}) =
				    ! extension
			      val tcp_connection = C {send = ! tcp_send,
						      abort = ! tcp_abort,
						      extension =
							 tcp_extension}
			  in tally_open ();
			     tcp_connection_handler tcp_connection;
			     tally_close ()
			  end)
		in {connection_handler = conn_connection_handler,
		    data_handler = conn_data_handler,
		    status_handler = conn_status_handler}
		end
       in Conn.H conn_handler_fun
       end

  fun tcp_session session_fn conn_session =  
       let val connection_count = ref 0;
	   fun tally_open () = connection_count := ! connection_count + 1
	   fun tally_close () =
		(connection_count := ! connection_count - 1;
		 if ! connection_count < 0 then () else ())
	   fun active_open (s, _, _) = s
	   fun tcp_connect (Conn.S {connect = conn_connect, ...})
			   (address, handler) =
		conn_connect (address,
			      tcp_handler (active_open, tally_open,
					   tally_close, handler))
	   fun tcp_listen (Conn.S {listen = conn_listen, ...})
			  (pattern, handler, count) = raise Y
	   fun tcp_extension (Conn.S {extension = conn_extension, ...}) =
		conn_extension
	   val tcp_session = S {connect = tcp_connect conn_session,
				listen = tcp_listen conn_session,
				extension = tcp_extension conn_session}
	   fun after_session () = ()
       in (session_fn tcp_session
	   before after_session ())
	  handle x => (after_session (); raise x)
       end


  fun session (session_fn: session -> 'a) =
       Conn.session (tcp_session session_fn)

 end 
