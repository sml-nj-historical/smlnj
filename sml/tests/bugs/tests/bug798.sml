(* bug798.sml *)
(* 798. incorrect sharing violation error *)

signature CONCUR_ML =
  sig

    type 'a event

    type thread_id
    type 'a cond_var
    type 'a chan

  end; (* signature CONCUR_ML *)

signature INTERNAL_CML =
  sig

    datatype 'a queue_t = Q of {front : 'a list ref, rear : 'a list ref}

    include CONCUR_ML

  end; (* INTERNAL_CML *)

structure CML : INTERNAL_CML =
  struct

    datatype 'a queue_t = Q of {front : 'a list ref, rear : 'a list ref}

  (* Per-thread descriptors *)
    datatype thread_id = TID of {  (* thread ids *)
	id	   : int,
	done_comm  : bool ref,		(* set this whenever this thread does *)
					(* some concurrency operation. *)
	death_cond : unit cond_var
      }
  (* condition variables *)
    and 'a cond_var = COND of 'a cond_state ref
    and 'a cond_state
      = COND_unset of (thread_id * bool ref * 'a SMLofNJ.Cont.cont) list
      | COND_set of 'a

  (* channels *)
    datatype 'a chan = CHAN of {
	inq	: (thread_id * 'a SMLofNJ.Cont.cont) chanq,
	outq	: (thread_id * 'a * unit SMLofNJ.Cont.cont) chanq
      }
      withtype 'a chanq = (bool ref * 'a) queue_t

  (* events *)
    datatype abort_fn = NO_ABORT | ABORT of (unit -> unit)
    datatype 'a base_evt = BASE_EVT of {
	  pollfn : unit -> bool,
	  dofn : abort_fn -> 'a,
	  blockfn : (bool ref * abort_fn * (unit -> unit)) -> 'a,
	  abortfn : abort_fn
	}
    datatype 'a event
      = EVT of ('a base_evt list * bool)    (* the boolean is true if one of the *)
					    (* base events has an abort action *)
      | GUARD of (unit -> 'a event)

  end; (* CML *)

signature RUN_CML =
  sig
    structure CML : CONCUR_ML
  end; (* RUN_CML *)

functor RunCML (CML : INTERNAL_CML) : RUN_CML =
  struct
    structure CML : CONCUR_ML = CML
  end; (* functor RunCML *)

signature CONCUR_IO =
  sig
    structure CML : CONCUR_ML
  end; (* CONCUR_IO *)

functor ConcurIO (RunCML : RUN_CML) : CONCUR_IO =
  struct
    structure CML = RunCML.CML
  end; (* functor ConcurIO *)

signature TRACE_CML =
  sig
    structure CML : CONCUR_ML
    structure CIO : CONCUR_IO
  end; (* TRACE_CML *)

functor TraceCML (
    structure CML : INTERNAL_CML
          and RunCML : RUN_CML
	  and CIO : CONCUR_IO
    sharing CML = RunCML.CML = CIO.CML
  ) : TRACE_CML = struct

    open CML (* need to open INTERNAL_CML version before rebinding CML *)

    structure CIO : CONCUR_IO = CIO
    structure CML : CONCUR_ML = CML

  end; (* TraceCML *)

structure CML :> sig  (* hide the internals, but preserve type equality *)
    structure CML : sig
        include CONCUR_ML
        where type thread_id = CML.thread_id
	    and type 'a chan = 'a CML.chan
	    and type 'a event = 'a CML.event
      end
    structure RunCML : RUN_CML
    structure CIO : CONCUR_IO
    structure TraceCML : TRACE_CML
    sharing CML = RunCML.CML = CIO.CML = TraceCML.CML
  end = struct
    structure CML = CML
    structure RunCML = RunCML(CML);
    structure CIO = ConcurIO(RunCML);
    structure TraceCML = TraceCML(
        structure CML = CML
              and RunCML = RunCML
              and CIO = CIO);
  end; (* CML *)
