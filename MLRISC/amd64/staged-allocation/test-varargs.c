#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define NEW(ty)   (ty*)malloc(sizeof(ty))

extern void varargs (void* fun, void* args, int);

typedef struct {
  void* val;
  void* kind;
  void* loc;
  void* ty;
}  zipped_arg_t;

typedef struct varargs_s {
  zipped_arg_t* hd;
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

#define N_ARGS 3
#define GPR 0
#define FPR 1
#define STK 2
#define FSTK 3

int main () 
{
  varargs_t* args[N_ARGS];
  
  for(int i = 0 ; i < N_ARGS; i++)
    args[i] = NEW(varargs_t);

  args[N_ARGS-1]->tl = 0;
  for (int i = N_ARGS-2; i >= 0; i--)
    args[i]->tl = args[i+1];

  args[0]->hd = NEW(zipped_arg_t);
  args[0]->hd->val = (void*)"%f %X\n";
  args[0]->hd->kind = (void*)GPR;
  args[0]->hd->loc = (void*)7;   
  args[0]->hd->ty = (void*)64;   

  double f = 3.14;
  void** x = (void*)&f;

  args[1]->hd = NEW(zipped_arg_t);
  args[1]->hd->val = *x;
  args[1]->hd->kind = (void*)FPR;
  args[1]->hd->loc = (void*)0;
  args[1]->hd->ty = (void*)64;   

  args[2]->hd = NEW(zipped_arg_t);
  args[2]->hd->val = (void*)0xdeadbeef;
  args[2]->hd->kind = (void*)GPR;
  args[2]->hd->loc = (void*)6;
  args[2]->hd->ty = (void*)32;   

  varargs(Say, args[0], N_ARGS*sizeof(void*));

  return 0;
}
