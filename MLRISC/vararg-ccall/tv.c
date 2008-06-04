#include <stdio.h>
#include <dlfcn.h>

int main ()
{
  void* handle = dlopen("./vararg", RTLD_LOCAL | RTLD_LAZY);
  
  if (!handle) {
    printf ("fail\n");
    return 1;
  }

  int (*vararg)(void*, void*, int);
  vararg = dlsym(handle, "vararg");
  if (!vararg)
    return 1;

  vararg(0, 0, 0);

  return 0;
}
