#include <stdio.h>
extern long test(int);
int main() {
  printf("Res = %ld\n", test(12345));
  return 0;
}
