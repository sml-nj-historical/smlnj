(* controls.sig
 *   An implementation of "controls" -- representing environment-configurable
 *   global state (flags, options, ...) of a program.
 *
 * COPYRIGHT (c) 2002 Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume
 *)
signature CONTROLS = sig

    (* Exception to be raised when trying to access a non-existent control: *)
    exception NoSuchControl

    (* Exception to be raised when an svar's setter function fails to
     * successfully parse its argument.  The t field will hold the
     * type's name, the s field will hold the original argument to the
     * setter. *)
    exception FormatError of { t: string, s: string }

    (* A "registry" is a grouping mechanism that can be used to tie related
     * controls together.  All controls in the same registry share a common
     * name prefix, a common priority, a common obscurity level, a common
     * optional name suffix (which will be used to configure a "default"
     * value), and a common method of turning control names into
     * names of environment variables.  Each registry has its own
     * registry name. *)
    type registry

    (* Each registry consists of several typed groups.  A group
     * ties together a number of controls of the same type.  The group
     * shares parsing/unparsing routines for their common type. *)
    type 'a group

    (* A typed variable is a pair consisting of a getter- and a setter
     * function that manipulate the state of a control. *)
    type 'a var = { get : unit -> 'a, set : 'a -> unit }

    (* An untyped svar (string variable) manipulates the state of a control
     * via string values that get parsed and unparsed according to the
     * group that the control belongs to.  If parsing fails, the
     * setter of an svar will raise FormatError. *)
    type svar = string var

    (* The "control" type describes a control by tying together the
     * name of the registry that it belongs to (rname), its priority,
     * its obscurity level, its name, a descriptive string, and
     * an svar to manipulate the control's value. *)
    type control = { rname : string, priority : int list, obscurity : int,
		     name : string, descr : string, svar : svar }

    (* Type information that needs to be provided when creating a group.
     * The tname field is a descriptive name for the type and is used
     * for the t field of exception FormatError. *)
    type 'a tinfo = { tname : string,
		      fromString : string -> 'a option,
		      toString : 'a -> string }

    (* A built-in special registry for creating non-configurable controls.
     * Controls created in "noconfig" will not show up in the output of
     * "control" or "controls" (see below). *)
    val noconfig : registry

    (* Make a new registry.
     * The initial state of each control in a registry can be configured
     * using environment variables.  The initial state of a control is
     * determined by three things:
     *    1. A constant "fallback" value that was specified when the
     *       control was created.  This value will be in effect if neither
     *       ev1 nor ev2 were set (see below).
     *    2. An environment variable ev1 that is queried every time
     *       function "init" (see below) is invoked.  (Normally, "init"
     *       should be invoked at program startup time.  The same environment
     *       variable will also be queried at module initialization time
     *       (which normally is the time when the program is originally
     *       created, but for dynamically linked modules it could be at
     *       runtime).
     *    3. An environment variable ev2 that is queried at link time,
     *       but not when "init" is invoked.
     * ev1 is obtained from the control's name by applying the mk_ename
     * function to it.  The default for mk_ename turns all letters into
     * upper case and all minuses (-) into underscores (_).
     * ev2 is obtained from by applying the mk_ename function to the
     * concatenation of the control's name and the "default suffix".
     * If default_suffix is NONE, then ev2 will not be used at all.
     *
     * The value of environment variables will be given to the setter
     * function of the svar associated with the control.
     *
     * The priority of a registry is used for (lexicographically) sorting
     * the output of function "controls" (see below).
     *
     * The obscurity level of a registry is used for limiting the number
     * of controls reported by function "controls" (see below).  The higher
     * the value, the "more obscure" the control is. *)
    val registry :
	{ name : string,
	  priority : int list,
	  obscurity : int,
	  prefix : string,
	  default_suffix : string option,
	  mk_ename : (string -> string) option } ->
	registry

    (* Make a new group: *)
    val group : registry -> 'a tinfo -> 'a group

    (* Make a new control and return a ref cell that holds its state.
     * The name of the control will be the concatenation of its
     * registry's prefix and the stem. *)
    val new :
	'a group ->
	{ stem : string, descr : string, fallback : 'a } ->
	'a ref

    (* Take an existing ref cell and register it as a control.
     * The fallback is the value that's already in the cell. *)
    val reg :
	'a group -> { stem: string, descr: string, cell: 'a ref } -> unit

    (* Access an existing control.  This function takes the stem of the
     * control's name, the full name is obtained by prepending the
     * registry's prefix. These functions can raise NoSuchControl. *)
    val acc : 'a group -> string -> 'a ref

    (* Get the svar for a control from its group.  The string specifies
     * the stem (i.e., not the full name).  If no control by that name has
     * been created, NoSuchControl will be raised. *)
    val sacc : 'a group -> string -> svar

    (* Get the control value (including the svar) representing a control.
     * The string specifies the full name of the control (i.e., not
     * just the stem).  If no control by that name has been created,
     * NoSuchControl will be raised. *)
    val control : string -> control

    (* Get a list of all controls with equal or less obscurity than the
     * optional argument.  The output will be sorted by registry according
     * the the registry' priorities and within each registry by name of
     * the control.  If the argument is NONE, then all controls will
     * be reported. *)
    val controls : int option -> control list

    (* Initialize the values of all controls by querying environment
     * variables (ev1) for them.   This function should be called
     * at program startup.  *)
    val init : unit -> unit

    (* Some pre-canned type information, provided for convenience: *)
    val bool : bool tinfo
    val int : int tinfo
    val real : real tinfo
    val string : string tinfo
    val stringList : string list tinfo
                     (* fromString = SOME o String.tokens Char.isSpace *)

    (* var interface to ref cells: *)
    val ref2var : 'a ref -> 'a var
end
