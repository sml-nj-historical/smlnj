#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void Say (const char *fmt, ...)
	__attribute__ ((format(printf, 1, 2)));

void Say (const char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
      vfprintf (stdout, fmt, ap);
      fflush (stdout);
      va_end(ap);

}
