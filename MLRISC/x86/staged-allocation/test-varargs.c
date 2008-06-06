#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define NEW(ty)   (ty*)malloc(sizeof(ty))

typedef struct {
  union val_u {
    int i;
    double d;
    char* s;
  } val;
  long long kind;
  long long loc;
  long long ty;
}  zipped_arg_t;

extern void varargs (void* fun, zipped_arg_t* args, zipped_arg_t* argsEnd);

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

#define N_ARGS 3
#define STK 2
#define FSTK 3
#define OFF(i) (i*4)

int main () 
{
  zipped_arg_t args[N_ARGS];
  
  args[0].val.s = "arg1=%d, arg2=%f\n";
  args[0].kind = (long long)STK;
  args[0].loc = (long long)OFF(0);
  args[0].ty = (long long)32;

  args[1].val.i = 69;
  args[1].kind = (long long)STK;
  args[1].loc = (long long)OFF(1);
  args[1].ty = (long long)32;

  args[2].val.d = 3.44;
  args[2].kind = (long long)FSTK;
  args[2].loc = (long long)OFF(2);
  args[2].ty = (long long)64;

  varargs(Say, args, &args[N_ARGS]);

  return 0;
}
