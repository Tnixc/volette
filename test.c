#include <stdio.h>
extern int test();

int main() {
  printf("w = %d\n", test(2, 2));
  return 0;
}
