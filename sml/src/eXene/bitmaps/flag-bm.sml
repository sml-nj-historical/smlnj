(* flag-bm.sml
 * this file created by bm2mlx
 * from:  mit/flagdown mit/flagup
 * on: Wed Mar  6 15:22:57 EST 1991
 *)
structure FlagBM =
  struct
    val flagdown = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=48, ht=48},
            data = [[
                "\000\000\000\000\000\000",
                "\000\000\000\000\000\000",
                "\000\000\000\000\000\000",
                "\000\000\000\120\000\000",
                "\000\000\001\254\000\000",
                "\000\000\007\135\000\000",
                "\000\000\014\001\128\000",
                "\000\000\024\000\192\000",
                "\000\000\048\000\192\000",
                "\000\000\096\000\096\032",
                "\000\000\192\000\096\096",
                "\000\001\128\000\096\224",
                "\000\003\248\000\225\224",
                "\000\007\254\001\227\224",
                "\000\014\007\003\167\224",
                "\000\028\001\135\046\224",
                "\000\024\001\142\060\224",
                "\000\048\000\220\120\192",
                "\000\048\000\248\240\000",
                "\000\097\248\113\224\000",
                "\000\096\096\099\160\000",
                "\000\096\000\099\160\000",
                "\000\096\000\099\032\000",
                "\000\096\000\096\032\000",
                "\254\096\000\096\039\255",
                "\000\096\000\096\032\000",
                "\000\096\000\096\032\000",
                "\000\096\000\096\096\000",
                "\000\096\000\096\192\000",
                "\000\096\000\097\128\000",
                "\000\096\000\099\000\000",
                "\000\096\000\102\000\000",
                "\000\096\000\108\000\000",
                "\000\096\000\124\000\000",
                "\000\127\255\244\000\000",
                "\000\063\255\228\000\000",
                "\000\000\017\004\000\000",
                "\000\000\017\004\000\000",
                "\000\000\017\004\000\000",
                "\000\000\017\004\000\000",
                "\000\000\017\004\000\000",
                "\000\000\017\004\000\000",
                "\000\000\017\004\000\000",
                "\000\000\017\004\000\000",
                "\239\253\113\063\251\031",
                "\185\215\217\110\075\094",
                "\098\012\071\240\135\226",
                "\170\033\018\136\033\152"
              ]]
          }
    val flagup = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=48, ht=48},
            data = [[
                "\000\000\000\000\000\000",
                "\000\000\000\007\254\000",
                "\000\000\000\007\254\000",
                "\000\000\000\247\086\000",
                "\000\000\003\222\174\000",
                "\000\000\007\007\086\000",
                "\000\000\012\006\174\000",
                "\000\000\024\007\254\000",
                "\000\000\048\007\254\000",
                "\000\000\096\007\032\000",
                "\000\000\192\007\032\000",
                "\000\001\128\007\096\000",
                "\000\003\248\007\224\000",
                "\000\007\254\007\224\000",
                "\000\014\007\007\160\000",
                "\000\028\001\135\032\000",
                "\000\024\001\143\032\000",
                "\000\048\000\223\032\000",
                "\000\048\000\255\032\000",
                "\000\097\248\119\032\000",
                "\000\096\096\103\032\000",
                "\000\096\000\103\032\000",
                "\000\096\000\103\032\000",
                "\000\096\000\102\032\000",
                "\254\106\074\096\039\255",
                "\000\110\170\096\032\000",
                "\000\106\234\096\032\000",
                "\000\106\170\096\096\000",
                "\000\106\171\096\192\000",
                "\000\096\000\097\128\000",
                "\042\096\000\099\042\170",
                "\085\096\000\102\085\084",
                "\042\096\000\108\170\170",
                "\085\096\000\125\085\084",
                "\042\127\255\246\170\170",
                "\085\063\255\229\085\084",
                "\042\128\017\006\170\170",
                "\085\085\081\005\085\084",
                "\042\170\177\006\170\170",
                "\085\085\081\005\085\084",
                "\042\170\177\006\170\170",
                "\085\085\081\005\085\084",
                "\042\170\177\010\170\170",
                "\085\085\081\021\085\084",
                "\042\170\169\042\170\170",
                "\085\085\085\085\085\084",
                "\042\170\170\170\170\168",
                "\000\000\000\000\000\000"
              ]]
          }
  end (* FlagBM *)
