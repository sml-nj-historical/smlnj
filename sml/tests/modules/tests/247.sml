  functor Tcp () =
  struct
    structure Tcp_Connection =
    struct
      type port = unit
      datatype t = L of {l: port}
    end

    type port = Tcp_Connection.port

    signature LISTEN_EXTENSION =
    sig
      datatype t = L of {l: port}
    end

    structure Listen_Extension:LISTEN_EXTENSION 
      = Tcp_Connection
  end;
