(* bug19.sml *)

signature A =
    sig
	exception Foo of string
    end;

structure B =
    struct
	exception Foo : string
    end;

structure C : A =
    struct
	exception Foo : string
    end;
