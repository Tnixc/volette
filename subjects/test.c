#include <stdio.h>

extern int factorial(int n);
extern int fib(int n);

int main() {
    printf("factorial(5) = %d\n", factorial(5));
    printf("fib(10) = %d\n", fib(10));
    return 0;
}
