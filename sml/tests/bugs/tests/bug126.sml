(* bug 126  scope of explicit type variables *)
  type (''a, 'b)map = (''a *  'b) list

  fun plus(l:(''a,'b)map ,[]: (''a, 'b)map ): (''a, 'b)map = l
    | plus(l,hd::tl) = plus(insert(l,hd), tl)
  and insert([], p) = [p]
    | insert((x,y)::rest, (x',y')) = 
        if x=x' then (x',y')::rest
        else (x,y) :: insert(rest,(x',y'));

