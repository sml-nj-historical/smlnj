(* bug214.1.sml *)
(* impossible error, lookPath, when printing top-level result *)

signature TERM =
  sig
    datatype term =  Const of string
         and varbind = Varbind of string * term
  end

functor Term ( ) : TERM =
  struct
    datatype term = Const of string
	 and varbind = Varbind of string * term
  end

signature BUG =
  sig
    structure Term : TERM
    type progentry
    val bug : progentry list
  end

functor Bug ( structure Term : TERM ) : BUG =
  struct
    structure Term = Term
    open Term
    datatype progentry =
      Progentry of string * Term.term * Term.varbind list * Term.term
    val bug =
      [Progentry("test",Const "test",
		 [Varbind("v",Const("test"))],Const("test"))]
  end

structure Term : TERM = Term ();
structure Bug : BUG = Bug ( structure Term = Term );

Bug.bug;
