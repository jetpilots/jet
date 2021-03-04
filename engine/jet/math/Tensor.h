#include <stdio.h>
#define countof(x) (sizeof(x) / sizeof(x[0]))

int sub2ind(int sub[], int sz[], int n) {
    int acc = 1, ind = 0;
    for (int i = n - 1; i > 0; i--) {
        ind += (sub[i] - 1) * acc;
        acc *= sz[i];
    }
    ind += (sub[0] - 1) * acc;
    return ind;
}

void ind2sub(int ind, int sz[], int* sub, int n) {
    // b is the index on the monodimensional array
    // i,j,k,m are the four resulting indices on the multidimensional array
    // si,sj,sk,sm are the size of any dimension

    // m = b % sm
    // k = (b / sm) % sk
    // j = (b / (sm*sk)) % sj
    // i = b / (sm*sk*sj)
    // --ind;
    int acc = 1;
    for (int i = n - 1; i > 0; i--) {
        sub[i] = ((ind / acc) % sz[i]) + 1;
        acc *= sz[i];
    }
    sub[0] = (ind / acc) + 1;
}

int sub2indc(int sub[], int sz[], int n) {
#define GETIND(i) ind += (sub[i] - 1) * acc, acc *= sz[i];

    int acc = 1, ind = 0;

    switch (n) {
    case 7:
        GETIND(6)
    case 6:
        GETIND(5)
    case 5:
        GETIND(4)
    case 4:
        GETIND(3)
    case 3:
        GETIND(2)
    case 2:
        GETIND(1)
    case 1:
        ind += (sub[0] - 1) * acc;
        return ind;
    case 0:
        return 0;
    default:
        return sub2ind(sub, sz, n);
    }
}

void ind2subc(int ind, int sz[], int* sub, int n) {
    // --ind;
#define GETSUB(i) sub[i] = ((ind / acc) % sz[i]) + 1, acc *= sz[i];

    int acc = 1;
    switch (n) {
    case 7:
        GETSUB(6)
    case 6:
        GETSUB(5)
    case 5:
        GETSUB(4)
    case 4:
        GETSUB(3)
    case 3:
        GETSUB(2)
    case 2:
        GETSUB(1)
    case 1:
        sub[0] = (ind / acc) + 1;
    case 0:
        return;
    default:
        if (ind > 0) ind2sub(ind, sz, sub, n);
    }
#undef GETSUB
}

int main() {
    int sz[] = { 6, 2, 4, 8 };
    int sub[] = { 2, 3, 6, 1 };

    ind2subc(382, sz, sub, countof(sz));
    for (int i = 0; i < countof(sz); i++) printf("%d ", sub[i]);
    printf("\n%d\n", sub2indc(sub, sz, countof(sz)));
    return 0;
}

// tensor type with possible specialisations for 2D/3D/4D etc.