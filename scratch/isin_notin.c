#include <stdio.h>

#define ISIN_1(x, ...) (x) == (__VA_ARGS__)
#define ISIN_2(x, a1, ...) (x) == (a1) || ISIN_1(x, __VA_ARGS__)
#define ISIN_3(x, a1, ...) (x) == (a1) || ISIN_2(x, __VA_ARGS__)
#define ISIN_4(x, a1, ...) (x) == (a1) || ISIN_3(x, __VA_ARGS__)
#define ISIN_5(x, a1, ...) (x) == (a1) || ISIN_4(x, __VA_ARGS__)
#define ISIN_6(x, a1, ...) (x) == (a1) || ISIN_5(x, __VA_ARGS__)
#define ISIN_7(x, a1, ...) (x) == (a1) || ISIN_6(x, __VA_ARGS__)
#define ISIN_8(x, a1, ...) (x) == (a1) || ISIN_7(x, __VA_ARGS__)
#define ISIN_9(x, a1, ...) (x) == (a1) || ISIN_8(x, __VA_ARGS__)
#define ISIN_10(x, a1, ...) (x) == (a1) || ISIN_9(x, __VA_ARGS__)
#define ISIN_11(x, a1, ...) (x) == (a1) || ISIN_10(x, __VA_ARGS__)
#define ISIN_12(x, a1, ...) (x) == (a1) || ISIN_11(x, __VA_ARGS__)
#define ISIN_13(x, a1, ...) (x) == (a1) || ISIN_12(x, __VA_ARGS__)
#define ISIN_14(x, a1, ...) (x) == (a1) || ISIN_13(x, __VA_ARGS__)
#define ISIN_15(x, a1, ...) (x) == (a1) || ISIN_14(x, __VA_ARGS__)
#define ISIN_16(x, a1, ...) (x) == (a1) || ISIN_15(x, __VA_ARGS__)
#define ISIN_17(x, a1, ...) (x) == (a1) || ISIN_16(x, __VA_ARGS__)
#define ISIN_18(x, a1, ...) (x) == (a1) || ISIN_17(x, __VA_ARGS__)
#define ISIN_19(x, a1, ...) (x) == (a1) || ISIN_18(x, __VA_ARGS__)
#define ISIN_20(x, a1, ...) (x) == (a1) || ISIN_19(x, __VA_ARGS__)
#define ISIN_21(x, a1, ...) (x) == (a1) || ISIN_20(x, __VA_ARGS__)
#define ISIN_22(x, a1, ...) (x) == (a1) || ISIN_21(x, __VA_ARGS__)
#define ISIN_23(x, a1, ...) (x) == (a1) || ISIN_22(x, __VA_ARGS__)
#define ISIN_24(x, a1, ...) (x) == (a1) || ISIN_23(x, __VA_ARGS__)
#define ISIN_25(x, a1, ...) (x) == (a1) || ISIN_24(x, __VA_ARGS__)
#define ISIN_26(x, a1, ...) (x) == (a1) || ISIN_25(x, __VA_ARGS__)
#define ISIN_27(x, a1, ...) (x) == (a1) || ISIN_26(x, __VA_ARGS__)
#define ISIN_28(x, a1, ...) (x) == (a1) || ISIN_27(x, __VA_ARGS__)
#define ISIN_29(x, a1, ...) (x) == (a1) || ISIN_28(x, __VA_ARGS__)
#define ISIN_30(x, a1, ...) (x) == (a1) || ISIN_29(x, __VA_ARGS__)
#define ISIN_31(x, a1, ...) (x) == (a1) || ISIN_30(x, __VA_ARGS__)
#define ISIN_32(x, a1, ...) (x) == (a1) || ISIN_31(x, __VA_ARGS__)
#define ISIN_33(x, a1, ...) (x) == (a1) || ISIN_32(x, __VA_ARGS__)
#define ISIN_34(x, a1, ...) (x) == (a1) || ISIN_33(x, __VA_ARGS__)
#define ISIN_35(x, a1, ...) (x) == (a1) || ISIN_34(x, __VA_ARGS__)
#define ISIN_36(x, a1, ...) (x) == (a1) || ISIN_35(x, __VA_ARGS__)
#define ISIN_37(x, a1, ...) (x) == (a1) || ISIN_36(x, __VA_ARGS__)
#define ISIN_38(x, a1, ...) (x) == (a1) || ISIN_37(x, __VA_ARGS__)
#define ISIN_39(x, a1, ...) (x) == (a1) || ISIN_38(x, __VA_ARGS__)
#define ISIN_40(x, a1, ...) (x) == (a1) || ISIN_39(x, __VA_ARGS__)
#define ISIN_41(x, a1, ...) (x) == (a1) || ISIN_40(x, __VA_ARGS__)
#define ISIN_42(x, a1, ...) (x) == (a1) || ISIN_41(x, __VA_ARGS__)
#define ISIN_43(x, a1, ...) (x) == (a1) || ISIN_42(x, __VA_ARGS__)
#define ISIN_44(x, a1, ...) (x) == (a1) || ISIN_43(x, __VA_ARGS__)
#define ISIN_45(x, a1, ...) (x) == (a1) || ISIN_44(x, __VA_ARGS__)
#define ISIN_46(x, a1, ...) (x) == (a1) || ISIN_45(x, __VA_ARGS__)
#define ISIN_47(x, a1, ...) (x) == (a1) || ISIN_46(x, __VA_ARGS__)
#define ISIN_48(x, a1, ...) (x) == (a1) || ISIN_47(x, __VA_ARGS__)
#define ISIN_49(x, a1, ...) (x) == (a1) || ISIN_48(x, __VA_ARGS__)
#define ISIN_50(x, a1, ...) (x) == (a1) || ISIN_49(x, __VA_ARGS__)
#define ISIN_51(x, a1, ...) (x) == (a1) || ISIN_50(x, __VA_ARGS__)
#define ISIN_52(x, a1, ...) (x) == (a1) || ISIN_51(x, __VA_ARGS__)
#define ISIN_53(x, a1, ...) (x) == (a1) || ISIN_52(x, __VA_ARGS__)
#define ISIN_54(x, a1, ...) (x) == (a1) || ISIN_53(x, __VA_ARGS__)
#define ISIN_55(x, a1, ...) (x) == (a1) || ISIN_54(x, __VA_ARGS__)
#define ISIN_56(x, a1, ...) (x) == (a1) || ISIN_55(x, __VA_ARGS__)
#define ISIN_57(x, a1, ...) (x) == (a1) || ISIN_56(x, __VA_ARGS__)
#define ISIN_58(x, a1, ...) (x) == (a1) || ISIN_57(x, __VA_ARGS__)
#define ISIN_59(x, a1, ...) (x) == (a1) || ISIN_58(x, __VA_ARGS__)
#define ISIN_60(x, a1, ...) (x) == (a1) || ISIN_59(x, __VA_ARGS__)
#define ISIN_61(x, a1, ...) (x) == (a1) || ISIN_60(x, __VA_ARGS__)
#define ISIN_62(x, a1, ...) (x) == (a1) || ISIN_61(x, __VA_ARGS__)
#define ISIN_63(x, a1, ...) (x) == (a1) || ISIN_62(x, __VA_ARGS__)
#define ISIN_64(x, a1, ...) (x) == (a1) || ISIN_63(x, __VA_ARGS__)

// #define ISIN_4(x, a1, a2, a3, a4) ((x == a1) || (x == a2))
// #define ISIN_5(x, a1, a2, a3, a4, a5) ((x == a1) || (x == a2))

#define ISIN(n, x, ...) (ISIN_##n(x, __VA_ARGS__))
// #define ISNOTIN(n, x, ...) !ISIN(n, x, __VA_ARGS__)

// if the number of elements in the array is too large, you should call this
// function because the macro will add too much inline code. What is too large?
// Let's say 64. for now. maybe that's too high already.
// Note that these functions aren't needed for native Jet arrays, lists or any
// collection. The collection has a contains(...) method which does the job
// in a way that's best for and relevant to the collection's implementation.
// Also remember that Jet generates case #a, #b, #c not with the isin call but
// rather as case _a: case _b: case _c: etc. which is far better.
int isin(int n, int x, int arr[]) {
    for (int i = 0; i < n; i++)
        if (arr[i] == x) return 1;
    return 0;
}

// int isnotin(int n, int x, int arr[], int n) { return !isin(x, arr, n); }
// for (int i=0;i<n;i++) if (arr[i]==x ) return 0;
// return 1;

int main() {
    int arr[] = { 2, 3, 4, 5, 6 };
    int q = isin(3, arr, 5);
    printf("%d\n", q);
    int qa = ISIN(5, 3, 2, 3, 4, 5, 6);
    printf("%d\n", qa);
}