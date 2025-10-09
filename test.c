#include <stdio.h>
extern int w();
extern void *f();

int main() {
  printf("f = %p\n", f(20));
  printf("w = %d\n", w(20));
  return 0;
}
