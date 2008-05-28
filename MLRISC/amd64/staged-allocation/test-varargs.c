#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define NEW(ty)   (ty*)malloc(sizeof(ty))

extern void varargs (void* fun, void* args);

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

int main () 
{
  varargs_t* args = NEW(varargs_t);
  varargs_t* args2 = NEW(varargs_t);
  varargs_t* args3 = NEW(varargs_t);

  args->hd = NEW(triplet_t);
  args->hd->val = (void*)"arg1=%d arg2=%f\n";
  args->hd->kind = (void*)0;  /* gpr */
  args->hd->loc = (void*)7;   /* rdi */
  args->tl = args2;
  
  args2->hd = NEW(triplet_t);
  args2->hd->val = (void*)1024;
  args2->hd->kind = (void*)0;  /* gpr */
  args2->hd->loc = (void*)6;   /* rdi */
  args2->tl = args3;

  double f = 3.14;
  void** x = (void*)&f;

  args3->hd = NEW(triplet_t);
  args3->hd->val = *x;
  args3->hd->kind = (void*)1;  /* fpr */
  args3->hd->loc = (void*)0;   /* xmm0 */
  args3->tl = 0;  

  varargs(Say, args);

  return 0;
}
