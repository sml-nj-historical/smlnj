structure foo = struct val myfoo = 0 end;
structure bar = struct val mybar =0 end;
local open foo in open bar end;

