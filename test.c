#include <stdio.h>
extern int test(int);

int main() {
  printf("Res = %d\n", test(2));
  return 0;
}
