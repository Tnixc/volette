#include <stdio.h>
extern int add(int);
int main() {
  printf("Identity(5) = %d\n", add(5));
  return 0;
}
