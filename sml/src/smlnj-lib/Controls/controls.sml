(* controls.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

structure Controls : CONTROLS =
  struct

    open ControlReps

    fun control {name, pri, obscurity, help, ctl} = Ctl{
	    name = Atom.atom name,
	    get = fn () => !ctl,
	    set = fn v => ctl := v,
	    priority = pri,
	    obscurity = obscurity,
	    help = help
	  }

    fun genControl {name, pri, obscurity, help, default} = control {
	    name = name, pri = pri, obscurity = obscurity, help = help,
	    ctl = ref default
	  }

  (* this exception is raised to communicate that there is a syntax error
   * in a string representation of a control value.
   *)
    exception ValueSyntax of {ctlName : string, value : string}

    fun stringControl {tyName, fromString, toString} = let
	  fun mk (Ctl{name, get, set, priority, obscurity, help}) = Ctl{
		  name = name,
		  get = fn () => toString(get()),
		  set = fn sval => (case fromString sval
		     of NONE => raise ValueSyntax{
			    ctlName = Atom.toString name,
			    value = sval
			  }
		      | SOME v => set v
		    (* end case *)),
		  priority = priority,
		  obscurity = obscurity,
		  help = help
		}
	  in
	    mk
	  end

    fun name (Ctl{name, ...}) = Atom.toString name
    fun get (Ctl{get, ...}) = get()
    fun set (Ctl{set, ...}, v) = set v
    fun info (Ctl{priority, obscurity, help, ...}) = {
	    priority = priority, obscurity = obscurity, help = help
	  }

    fun compare (Ctl{priority=p1, ...}, Ctl{priority=p2, ...}) = let
	  fun collate ([], []) = EQUAL
	    | collate ([], _) = LESS
	    | collate (_, []) = GREATER
	    | collate (x::xs, y::ys) =
		if (x = y) then collate(xs, ys)
		else if (x < y) then LESS
		else GREATER
	  in
	    collate (p1, p2)
	  end

  end
