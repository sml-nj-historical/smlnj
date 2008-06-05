#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define NEW(ty)   (ty*)malloc(sizeof(ty))

extern void varargs (void* fun, void* args, int);

typedef struct {
  long long val;
  long long kind;
  long long loc;
  long long ty;
}  zipped_arg_t;

typedef struct varargs_s {
  zipped_arg_t* hd;
  int pad;
  struct varargs_s* tl;
} varargs_t;

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

#define N_ARGS 1
#define STK 2
#define FSTK 3
#define OFF(i) (i*4)

int alignb (int n) {
  n += 2;
  n *= sizeof(long long*);
  n += 16-(n%16);
  return n-(2*sizeof(void*));
}

int main () 
{
  varargs_t* args[N_ARGS];
  
  for(int i = 0 ; i < N_ARGS; i++)
    args[i] = NEW(varargs_t);

  args[N_ARGS-1]->tl = 0;
  for (int i = N_ARGS-2; i >= 0; i--)
    args[i]->tl = args[i+1];

  args[0]->hd = NEW(zipped_arg_t);
  args[0]->hd->val = (long long)"arg1=%d\n";
  args[0]->hd->kind = (long long)STK;
  args[0]->hd->loc = (long long)OFF(0);
  args[0]->hd->ty = (long long)32;
  /*
  args[1]->hd = NEW(zipped_arg_t);
  args[1]->hd->val = (long long)69;
  args[1]->hd->kind = (long long)STK;
  args[1]->hd->loc = (long long)OFF(1);
  args[1]->hd->ty = (long long)32;
  */

  varargs(Say, args[0], alignb(N_ARGS));

  return 0;
}
