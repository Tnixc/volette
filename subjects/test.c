#include <stdio.h>
extern int w(void);
extern int factorial(int n);
extern int fib(int n);

int main() {
    printf("w() = %d (expected 16)\n", w());
    printf("factorial(5) = %d (expected 120)\n", factorial(5));
    printf("factorial(10) = %d (expected 3628800)\n", factorial(10));
    printf("fib(0) = %d (expected 0)\n", fib(0));
    printf("fib(1) = %d (expected 1)\n", fib(1));
    printf("fib(10) = %d (expected 55)\n", fib(10));
    printf("fib(20) = %d (expected 6765)\n", fib(20));
    return 0;
}
