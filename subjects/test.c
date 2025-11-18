#include <stdio.h>
extern int test(int);
// extern int test_negation(int);
// extern int test_not(int);
// extern long ptr(int);
// extern int v();
// extern int test_nested_negation(int);

int main() {
  // printf("test(2) = %d (expected: 4)\n", test(2));
  // printf("test_negation(5) = %d (expected: -5)\n", test_negation(5));
  // printf("test_not(1) = %d (expected: 1)\n", test_not(1));
  // printf("test_not(0) = %d (expected: 0)\n", test_not(0));
  // printf("ptr(0) = %ld \n", ptr(0));
  // printf("test_nested_negation(7) = %d (expected: 7)\n", test_nested_negation(7));
  printf("test(62) = %d \n", test(62));
  return 0;
}
