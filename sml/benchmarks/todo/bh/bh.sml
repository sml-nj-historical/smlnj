(* hierachical N-body code, adapted from Barnes-Hut C version *)

structure BarnesHut = 
  struct
      open bhMath bhUtil Array

      val IMAX = 536870912    (* 2^29 *)
      val IMAXrs1 = Bits.rshift(IMAX,1)
      val NDIM = 3
      val NSUB = Bits.lshift(1,NDIM)

      datatype content = Body of {vel:real vec ref,
				  acc:real vec ref,
				  phi:real ref} |
	                 Cell of node array
      and
	  node = Empty | 
	         Node of {mass:real,
			  pos: real vec ref,
			  contents:content}

      fun putCell (c as Cell x) k v = (update(x,k,v);
					 c)
      fun getCell (Cell x) k = sub(x,k)
    
      fun pickshell rad =
	  let fun pickvec () =
	      let val rv = randvec (~1.0,1.0)
	      in
		  if (dotvp rv rv) > 1.0 then
		      pickvec ()
		  else
		      rv
	      end
	      val vec = pickvec ()
	  in
	      mulvs vec (rad / (sqrt (dotvp vec vec)))
	  end

      fun buildtest n =      (* builds Plummer model *)
	  let val rn = real n
	      val bodymass = 1.0 / (rn)
	      val mfrac = 0.9999
	      val sqrt2 = sqrt 2.0
	      val rsc = 3.0 * PI / 16.0
	      val vsc = sqrt (1.0 / rsc)
	      fun vN () =   (* von Neumann technique *)
		  let val x = xrand(0.0,1.0)
		      val y = xrand(0.0,0.1)
		  in
		      if (y > x*x * (pow (1.0-x*x) 3.5)) then vN ()
		      else x
		  end
	      fun mkbody _ =
		  let val r = 1.0/sqrt(pow(xrand(0.00001,mfrac))(~2.0/3.0)-1.0)
		  in
		      Node{mass=bodymass,
			   pos=ref (pickshell (rsc * r)),
			   contents=Body{vel=ref 
					 (pickshell (vsc * (sqrt2 * (vN ()) / 
						     (pow (1.0+r*r) 0.25)))),
					 acc=ref realzerovec,
					 phi=ref 0.0}}
		  end
	      fun scale (n as Node{pos,contents=Body{vel,...},...}) ps vs = 
		  (pos := subv (!pos) ps;
		   vel := subv (!vel) vs;
		   n)
	      val bodies = map mkbody (intsto n)
	      val cmr = fold (fn (Node{pos,...},v) => addv (!pos) v) 
		             bodies realzerovec
	      val cmr' = divvs (divvs cmr rn) rn
	      val cmv = fold (fn (Node{contents=Body{vel,...},...},v) => 
			          addv (!vel) v)
		             bodies realzerovec
	      val cmv' = divvs (divvs cmv rn) rn
	  in
	      map (fn b => scale b cmr' cmv') bodies
	  end

      fun intcoord rp rmin rsize =
	  let val xsc = divvs (subv rp rmin) rsize
	      exception notintcoord
	  in
	      SOME (map (fn x => if (x >= 0.0 andalso x < 1.0) then
			             floor (real IMAX * x)
				 else
				     raise notintcoord)
		    xsc)
	      handle notintcoord => NONE
		   | x => raise x
	  end

      fun cellindex iv l =
	  let fun aux [] k = 0
		| aux (v::vs) k = (if (Bits.andb(l,v)) <> 0 then
				       Bits.rshift(NSUB,k+1)
				   else
				       0) + (aux vs (k+1))
	  in
	      aux iv 0
	  end

      fun expandbox (nd as Node{pos,...}) (state as (rmin,rsize,root)) = 
	  let val ic = intcoord (!pos) rmin rsize
	  in
	      case ic of
		  NONE => 
		      let val rmid = addvs rmin (0.5 * rsize)
			  val rmin' = mapvec3 (fn (x,y,z) => if x < y then 
						                  z - rsize
							      else 
								  z)
			                      (!pos) rmid rmin
			  val rsize' = 2.0 * rsize
			  fun mksub v r = 
			      let val SOME x = (intcoord v rmin' rsize')
				  val k = cellindex x IMAXrs1
				  val a = Cell (array(NSUB,Empty))
			      in
				  putCell a k r
			      end
		      in
			  expandbox nd (case root of
					    Empty => 
					      (rmin',rsize',root)
					  | Node{...} => 
					      (rmin',
					       rsize',
					       Node{mass=0.0,
						    pos=ref realzerovec,
						    contents=mksub rmid root}))
		      end
		| SOME _ => state
	  end

      fun walk tree (acc0,eps,phi0,pos0,pskip,rsize,tol) = 
	  let fun subdivp (Node{contents=Body _,...}) _ _ = false
		| subdivp (Node{pos=pos,...}) dsq (pos0,tol) = 
		  let val tolsq = tol * tol
		      val dr = subv (!pos) pos0
		      val drsq = dotvp dr dr
		  in
		      (tolsq * drsq) < dsq
		  end
	      fun gravsub (Node{pos=pos,mass=mass,...}) (pos0,eps,acc0,phi0) = 
		  let val dr = subv (!pos) pos0
		      val drsq = (dotvp dr dr) + eps * eps
		      val drabs = sqrt drsq
		      val phii = mass / drabs
		      val phi0' = phi0 - phii
		      val mor3 = phii / drsq
		      val acc0' = addv acc0 (mulvs dr mor3)
		  in
		      (acc0',phi0')
		  end
	      fun walksub p dsq (state as (acc0,eps,phi0,pos0,pskip,tol)) = 
		  if subdivp p dsq (pos0,tol) then
		      let val Node{contents=Cell subcells,...} = p
		      in
			  foldarray (fn (n as Node _,s) => 
				         walksub n (dsq / 4.0) s
			              | (_,s) => s)
			            subcells
				    state
		      end
		  else if p = pskip then state
		  else 
		      let val (acc0',phi0') = gravsub p (pos0,eps,acc0,phi0)
		      in
			  (acc0',eps,phi0',pos0,pskip,tol)
		      end
	      val (acc0',_,phi0',_,_,_) = walksub 
		                              tree (rsize * rsize) 
					      (acc0,eps,phi0,pos0,pskip,tol)
	  in
	      (acc0',phi0')
	  end

      fun grav (p as Node{pos=pos,contents=Body{phi=phi,acc=acc,...},...}) 
	  (root,eps,rsize,tol) =
	  let val (acc0',phi0') = walk root 
	                               (realzerovec,eps,0.0,!pos,p,rsize,tol)
	  in
	      phi := phi0';
	      acc := acc0'
	  end
	
      fun loadtree (n as Node{pos=posn,...}) (rmin,rsize,root) =
	  let val SOME xp = (intcoord (!posn) rmin rsize)
	      val l = IMAXrs1
	      fun aux Empty _ = n
		| aux (n as Node{contents=Body{...},pos=posb,...}) l = 
		  let val SOME xq = (intcoord (!posb) rmin rsize)
		      val k = cellindex xq l
		      val a = Cell (array(NSUB,Empty))
		  in
		      aux (Node{mass=0.0,
				pos=ref realzerovec,
				contents=putCell a k n})
		          l
		  end
		| aux (n as Node{contents=c as Cell x,...}) l = 
		  let val i = cellindex xp l
		  in
		      putCell c i (aux (getCell c i)
				   (Bits.rshift(l,1)));
		      n
		  end
	  in
	      (rmin,rsize,aux root l)
	  end

      fun cellmass (Node{contents=Cell subcells,...}) =
	  let val () = maparray cellmass subcells
	      val m = foldarray (fn (Node{mass=mass,...},x) => mass+x
	                          | (_,x) => x)
		                subcells
				0.0
	  in
	      Node{mass=m,pos=ref realzerovec,contents=Cell subcells}
	  end
	| cellmass x = x
    
      fun cellmoment (nd as Node{contents=Cell cells,pos=pos,mass=mass,...}) =
	  let val () = maparray cellmoment cells
	      val moment = foldarray (fn (Node{pos=p,mass=m,...},v) => 
				          addv v (mulvs (!p) m)
		                       | (_,v) => v)
		                     cells
				     (!pos)
	  in
	      pos := divvs moment mass;
	      nd
	  end
	| cellmoment x = x
    

      fun maketree nodelist x = 
	  let fun aux (n as Node{mass,...},state) = 
	      if mass = 0.0 then
		  state
	      else
		  loadtree n (expandbox n state)
	  in
	      fold aux nodelist x
	  end

      fun stepsystem plist (dtime,eps,nstep,rmin,rsize,tnow,tol) = 
	  let val (rmin,rsize,tree) = maketree plist (rmin,rsize,Empty)
	      val tree' = cellmoment (cellmass tree)
	      val dthf = 0.5 * dtime
	      fun recalc (n as Node{contents=Body{acc=acc,vel=vel,...},...}) = 
		  let val acc1 = !acc
		      val _ = grav n (tree',eps,rsize,tol) 
		  in
		      if nstep > 0 then
			  let val dacc = subv (!acc) acc1
			      val dvel = mulvs dacc dthf
			  in
			      vel := addv (!vel) dvel
			  end
		      else
			  ()
		  end
	      fun adv (Node{pos=pos,contents=Body{acc=acc,vel=vel,...},...}) = 
		  let val dvel = mulvs (!acc) dthf
		      val vel1 = addv (!vel) dvel
		      val dpos = mulvs vel1 dtime
		  in
		      pos := addv (!pos) dpos;
		      vel := addv vel1 dvel
		  end
	  in
	      app recalc plist;
	      app adv plist;
	      (nstep+1,tnow+dtime)
	  end
    
      fun dump (Node{mass=m,pos=p,contents=Body{vel=v,...},...}) = 
	  (print "Mass=";
	   print m;
	   print "\n";
	   print "pos=";
	   realprvec (!p);
	   print "vel=";
	   realprvec (!v))

      fun testit n =       (* test a model for n bodies *)
	  let val bodies = buildtest n
	      val dtime = 0.025
	      val eps = 0.05
	      val nstep = 0
	      val tol = 1.0
	      val tstop = (* 0.5 *) 2.0
	      val tnow = 0.0
	      val rmin = mkvs ~2.0
	      val rsize = ~2.0 * getx rmin
	      fun doit (state as (dtime,eps,nstep,rmin,rsize,tnow,tol)) = 
		  if tnow < tstop + 0.1 * dtime then
		      let fun status step time = 
			  (print "stepping system at time ";
			   print time;
			   print " to next step ";
			   print step;
			   print "\n")
			  val _ = status nstep tnow
(*			  val _ = app dump bodies  *)
			  val (nstep',tnow') = stepsystem bodies state
		      in
			  doit (dtime,eps,nstep',rmin,rsize,tnow',tol)
		      end
		  else
		      ()
	  in
	      doit (dtime,eps,nstep,rmin,rsize,tnow,tol)
	  end
  end

