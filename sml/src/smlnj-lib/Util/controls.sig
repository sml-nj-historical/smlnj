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

    (* A "module" is a grouping mechanism that can be used to tie related
     * controls together.  All controls in the same module share a common
     * name prefix, a common priority, a common obscurity level, a common
     * optional name suffix (which will be used to configure a "default"
     * value), and a common method of turning control names into
     * names of environment variables.  Each module has its own
     * module name. *)
    type module

    (* Each module consists of several typed registries.  A registry
     * ties together a group of controls of the same type.  The group
     * shares parsing/unparsing routines for their common type. *)
    type 'a registry

    (* A typed variable is a pair consisting of a getter- and a setter
     * function that manipulate the state of a control. *)
    type 'a var = { get : unit -> 'a, set : 'a -> unit }

    (* An untyped svar (string variable) manipulates the state of a control
     * via string values that get parsed and unparsed according to the
     * registry that the control belongs to.  If parsing fails, the
     * setter of an svar will raise FormatError. *)
    type svar = string var

    (* The "control" type describes a control by tying together the
     * name of the module that it belongs to (mname), its priority,
     * its obscurity level, its name, a descriptive string, and
     * an svar to manipulate the control's value. *)
    type control = { mname : string, priority : int list, obscurity : int,
		     name : string, descr : string, svar : svar }

    (* Type information that needs to be provided when creating a registry.
     * The tname field is a descriptive name for the type and is used
     * for the t field of exception FormatError. *)
    type 'a tinfo =
	 { tname : string, parse : string -> 'a option, show : 'a -> string }

    (* A built-in special module for creating non-configurable controls.
     * Controls created in "noconfig" will not show up in the output of
     * "control" or "controls" (see below). *)
    val noconfig : module

    (* Make a new module.
     * The initial state of each control in a module can be configured
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
     *       created, but for dynamically linked modules it will be at
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
     * The priority of a module is used for (lexicographically) sorting
     * the output of function "controls" (see below).
     *
     * The obscurity level of a module is used for limiting the number
     * of controls reported by function "controls" (see below).  The higher
     * the value, the "more obscure" the control is. *)
    val module :
	{ name : string,
	  priority : int list,
	  obscurity : int,
	  prefix : string,
	  default_suffix : string option,
	  mk_ename : (string -> string) option } ->
	module

    (* Make a new registry: *)
    val registry : module -> 'a tinfo -> 'a registry

    (* Make a new control and return a typed variable to manipulate its
     * state.  The name of the control will be the concatenation of its
     * module's prefix and the stem. *)
    val new :
	'a registry ->
	{ stem : string, descr : string, fallback : 'a } ->
	'a var

    (* Make a new control and return the ref cell holding its state: *)
    val new_ref :
	'a registry ->
	{ stem : string, descr : string, fallback : 'a } ->
	'a ref

    (* Access an existing control.  This function takes the stem of the
     * control's name, the full name is obtained by prepending the
     * module's prefix. These functions can raise NoSuchControl. *)
    val acc : 'a registry -> string -> 'a var
    val acc_ref : 'a registry -> string -> 'a ref

    (* Get the svar for a control from its registry.  The string specifies
     * the stem (i.e., not the full name).  If no control by that name has
     * been created, NoSuchControl will be raised. *)
    val sacc : 'a registry -> string -> svar

    (* Get the control value (including the svar) representing a control.
     * The string specifies the full name of the control (i.e., not
     * just the stem).  If no control by that name has been created,
     * NoSuchControl will be raised. *)
    val control : string -> control

    (* Get a list of all controls with equal or less obscurity than the
     * optional argument.  The output will be sorted by module according
     * the the modules' priorities and within each module by name of
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
                     (* parse = SOME o String.tokens Char.isSpace *)
end
