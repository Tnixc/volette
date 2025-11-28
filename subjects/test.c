#include <stdio.h>

extern int test_unit(void);

int main() {
    printf("test_unit() = %d (expected 42)\n", test_unit());
    return 0;
}
