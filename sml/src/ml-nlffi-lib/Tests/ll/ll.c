# include <stdio.h>
# include "ll.h"

struct s s = { 0, 0 };

void ps (void)
{
  printf ("%llx %lld\n", s.u, s.s);
}
