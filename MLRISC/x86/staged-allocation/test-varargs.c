#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define NEW(ty)   (ty*)malloc(sizeof(ty))

extern void varargs (void* fun, void* args, int);

typedef struct {
  void* val;
  void* kind;
  void* loc;
}  triplet_t;

typedef struct varargs_s {
  triplet_t* hd;
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

#define N_ARGS 2
#define STK 2
#define FSTK 3

int alignb (int n) {
  n += 2;
  n *= sizeof(void*);
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

  args[0]->hd = NEW(triplet_t);
  args[0]->hd->val = (void*)"arg1=%f\n";
  args[0]->hd->kind = (void*)STK;
  args[0]->hd->loc = (void*)(0*4);   
  /*
  args[1]->hd = NEW(triplet_t);
  args[1]->hd->val = (void*)0xdeadbeef;
  args[1]->hd->kind = (void*)STK;  
  args[1]->hd->loc = (void*)(1*4);
  */

  float f = 3.14f;
  void** x = (void*)&f;
  printf("%f\n",*(float*)x);

  args[1]->hd = NEW(triplet_t);
  args[1]->hd->val = *x;
  args[1]->hd->kind = (void*)FSTK;
  args[1]->hd->loc = (void*)(1*4);

  varargs(Say, args[0], alignb(N_ARGS));

  return 0;
}
