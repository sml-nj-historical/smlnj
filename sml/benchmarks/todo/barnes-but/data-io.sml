(* data-io.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * I/O routines for export version of hierarchical N-body code.
 *)

signature DATA_IO =
  sig

    structure S : SPACE

    val inputData : string -> {
	    nbody : int,
	    bodies : S.body list,
	    tnow : real,
	    headline : string
	  }

  (* output routines *)
    val initOutput : {
	    outfile : string, headline : string, nbody : int, tnow : real,
	    dtime : real, eps : real, tol : real, dtout : real, tstop : real
	  } -> unit
    val output : {
	    nbody : int, bodies : S.body list, n2bcalc : int, nbccalc : int,
	    selfint : int, tnow : real
	  } -> unit
    val stopOutput : unit -> unit

  end;

functor DataIO (S : SPACE) : DATA_IO =
  struct

    structure S = S
    structure V = S.V
    structure F = Format

    val atoi = StringCvt.strToInt StringCvt.Dec

    fun inputData fname = let
	  val strm = IO.open_in fname
	  val buf = ref ("", 0)
	  fun getLn () = (case (IO.input_line strm)
		 of "" => raise Fail "inputData: EOF"
		  | s => buf := (s, 0)
		(* end case *))
	  fun skipWS () = let
		val (s, pos) = !buf
		val n = size s
		fun skip i = if (i >= n)
			then (getLn(); skipWS())
		      else if CType.isSpace(s, i)
			then skip(i+1)
			else (s, i)
		in
		  skip pos
		end
	  fun readInt () = let
		val (s, p) = skipWS()
		val (n, p) = atoi (s, p)
		in
		  buf := (s, p); n
		end
	  fun readReal () = let
                val (s, p) = skipWS()
                val (r, p) = StringCvt.strToReal (s, p)
                in
                  buf := (s, p); r
                end
	  val nbody = readInt()
	  val _ = if (nbody < 1)
		then raise Fail "absurd nbody"
		else ()
	  val ndim = readInt()
	  val _ = if (ndim <> V.dim)
		then raise Fail "absurd ndim"
		else ()
	  val tnow = readReal()
	  fun iter f = let
		fun loop (0, l) = l
		  | loop (n, l) = loop (n-1, f() :: l)
		in
		  fn n => loop (n, [])
		end
	  fun readVec () = V.implode (rev (iter readReal ndim))
	  val massList = iter readReal nbody
	  val posList = iter readVec nbody
	  val velList = iter readVec nbody
	  fun mkBodies ([], [], [], l) = l
	    | mkBodies (m::r1, p::r2, v::r3, l) = let
		val b = S.Body{
			proc = ref 0,
			mass = m,
			pos = ref p,
			vel = ref v,
			acc = ref V.zerov,
			phi = ref 0.0
		      }
		in
		  mkBodies(r1, r2, r3, b::l)
		end
	  in
	    IO.close_in strm;
	    { nbody = nbody,
	      bodies = mkBodies (massList, posList, velList, []),
	      tnow = tnow,
	      headline = implode["Hack code: input file ", fname, "\n"]
	    }
	  end

    local
      val timer = ref (System.Timer.start_timer ())
    in
    fun initTimer () = timer := System.Timer.start_timer()
    fun cputime () = let
	  val t = System.Timer.check_timer(!timer)
	  val gct = System.Timer.check_timer_gc(!timer)
	  val System.Timer.TIME{sec, usec} = System.Timer.add_time(t, gct)
	  in
	    ((real sec) + (0.000001 * (real usec))) / 60.0  (* in min. *)
	  end
    end

    type out_state = {
	tout : real,
	dtout : real,
	dtime : real,
	strm : IO.outstream
      }
    val outState = ref (NONE : out_state option)

    fun fprintf (strm, fmt, items) = IO.output(strm, F.format fmt items)
    fun printf (fmt, items) = fprintf(IO.std_out, fmt, items)
    local
      val itemFmt = F.format "%9.4f"
      val fmt = V.format{lp="", sep="", rp="", cvt=(fn x => itemFmt [F.REAL x])}
    in
    fun printvec (init, vec) = printf("\t %9s%s\n", [F.STR init, F.STR(fmt vec)])
    end (* local *)

    fun stopOutput () = (case (! outState)
	   of NONE => ()
	    | (SOME{strm, ...}) => (IO.close_out strm; outState := NONE)
	  (* end case *))

    fun initOutput {outfile, headline, nbody, tnow, dtime, eps, tol, dtout, tstop} = (
	  initTimer();
	  printf ("\n\t\t%s\n\n", [F.STR headline]);
	  printf ("%12s%12s%12s%12s%12s%12s\n",
	    map F.STR ["nbody", "dtime", "eps", "tol", "dtout", "tstop"]);
	  printf ("%12d%12.5f%12.4f%12.2f%12.3f%12.2f\n\n", [
	      F.INT nbody, F.REAL dtime, F.REAL eps, F.REAL tol,
	      F.REAL dtout, F.REAL tstop
	    ]);
	  case outfile
	   of "" => stopOutput()
	    | _ => outState := SOME{
		  dtime = dtime,
		  tout = tnow,
		  dtout = dtout,
		  strm = IO.open_out outfile
		}
	  (* end case *))

  (* compute set of dynamical diagnostics. *)
    fun diagnostics bodies = let
	  fun loop ([], arg) = {
		  mtot = #totM arg,		(* total mass *)
		  totKE = #totKE arg,		(* total kinetic energy *)
		  totPE = #totPE arg,		(* total potential energy *)
		  cOfMPos = #cOfMPos arg,	(* center of mass: position *)
		  cOfMVel = #cOfMVel arg,	(* center of mass: velocity *)
		  amVec = #amVec arg		(* angular momentum vector *)
		}
	    | loop (S.Body{
		  mass, pos=ref pos, vel=ref vel, acc=ref acc, phi=ref phi, ...
	        } :: r, arg) = let
		val velsq = V.dotvp(vel, vel)
		val halfMass = 0.5 * mass
		val posXmass = V.mulvs(pos, mass)
		in
		  loop ( r, {
		      totM = (#totM arg) + mass,
		      totKE = (#totKE arg) + halfMass * velsq,
		      totPE = (#totPE arg) + halfMass * phi,
		      keTen = V.addm(#keTen arg, V.outvp(V.mulvs(vel, halfMass), vel)),
		      peTen = V.addm(#peTen arg, V.outvp(posXmass, acc)),
		      cOfMPos = V.addv(#cOfMPos arg, posXmass),
		      cOfMVel = V.addv(#cOfMVel arg, V.mulvs(vel, mass)),
		      amVec = V.addv(#amVec arg, V.mulvs(V.crossvp(pos, vel), mass))
		    })
		end
	  in
	    loop (bodies, {
		totM = 0.0, totKE = 0.0, totPE = 0.0,
		keTen = V.zerom, peTen = V.zerom,
		cOfMPos = V.zerov, cOfMVel = V.zerov,
		amVec = V.zerov
	      })
	  end (* diagnostics *)

    fun outputData (strm, tnow, nbody, bodies) = let
	  fun fprintf (fmt, tag) item = IO.output(strm, fmt [tag item])
	  val outInt = fprintf(F.format "  %d\n", F.INT)
	  val outReal = fprintf(F.format " %21.14E\n", F.REAL)
	  val prReal = fprintf(F.format " %21.14E", F.REAL)
	  fun outVec v = let
		fun out [] = IO.output(strm, "\n")
		  | out (x::r) = (prReal x; out r)
		in
		  out(V.explode v)
		end
	  in
	    outInt nbody;
	    outInt V.dim;
	    outReal tnow;
	    app (fn (S.Body{mass, ...}) => outReal mass) bodies;
	    app (fn (S.Body{pos, ...}) => outVec(!pos)) bodies;
	    app (fn (S.Body{vel, ...}) => outVec(!vel)) bodies;
	    printf ("\n\tparticle data written\n", [])
	  end;

    fun output {nbody, bodies, n2bcalc, nbccalc, selfint, tnow} = let
	  val nttot = n2bcalc + nbccalc
	  val nbavg = floor(real n2bcalc / real nbody)
	  val ncavg = floor(real nbccalc / real nbody)
	  val data = diagnostics bodies
	  in
	    printf("\n%9s%9s%9s%9s%9s%9s%9s%9s\n", map F.STR [
		"tnow", "T+U", "T/U", "nttot", "nbavg", "ncavg", "selfint",
		"cputime"
	      ]);
	    printf("%9.3f%9.4f%9.4f%9d%9d%9d%9d%9.2f\n\n", [
		F.REAL tnow, F.REAL(#totKE data + #totPE data),
		F.REAL(#totKE data / #totPE data), F.INT nttot,
		F.INT nbavg, F.INT ncavg, F.INT selfint, F.REAL(cputime())
	      ]);
	    printvec("cm pos", #cOfMPos data);
	    printvec("cm vel", #cOfMVel data);
	    printvec("am pos", #amVec data);
	    case !outState
	     of NONE => ()
	      | (SOME{tout, dtout, dtime, strm}) =>
		  if ((tout - 0.01 * dtime) <= tnow)
		    then (
		      outputData (strm, tnow, nbody, bodies);
		      outState := SOME{
			  tout=tout+dtout, dtout=dtout, dtime=dtime, strm=strm
			})
		    else ()
	    (* end case *)
	  end

  end; (* DataIO *)

