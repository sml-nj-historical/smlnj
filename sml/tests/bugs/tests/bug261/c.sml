(*------------------------ c.sml ------------------------*)

import "a";
import "b";

functor CF() = struct structure A = BF() end;
