#include <stdio.h>
extern int test(int);

int main() {
    printf("test() = %d\n", test(11));
    return 0;
}
