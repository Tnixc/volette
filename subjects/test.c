#include <stdio.h>
extern int t(int);
// extern int test_negation(int);
// extern int test_not(int);
// extern long ptr(int);
// extern int v();
// extern int test_nested_negation(int);

int main() {
  int a = 10;
  printf("test(%d) = %d \n", a, t(a));
  return 0;
}
