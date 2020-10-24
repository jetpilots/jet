#include <stdio.h>
#include <stdlib.h>

#define jet_countof(x) (sizeof(x) / sizeof(x[0]))
#define endof(x) (x + sizeof(x) / sizeof(x[0]))

long int arr[] = { 3, 4, 5, 5, 6, 7, 8, 9 };

int main() {
    // long int* arr = malloc(8 * (1UL << 56));
    long int* arrend = endof(arr); //(1UL << 56);
#pragma clang loop unroll(enable) interleave(enable)
    for (long int *tmp = arr, arr = *tmp; tmp < arrend; ++tmp, arr = *tmp)
        printf("%ld ", arr);
    puts("");
    return 0;
}