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
#define ISIN(n, x, ...) (ISIN_##n(x, __VA_ARGS__))

int strsame(char* a, char* b) { return a == b || !strcmp(a, b); }

#define STR_ISIN_1(x, ...) strsame(x, __VA_ARGS__)
#define STR_ISIN_2(x, a1, ...) strsame(x, a1) || STR_ISIN_1(x, __VA_ARGS__)
#define STR_ISIN_3(x, a1, ...) strsame(x, a1) || STR_ISIN_2(x, __VA_ARGS__)
#define STR_ISIN_4(x, a1, ...) strsame(x, a1) || STR_ISIN_3(x, __VA_ARGS__)
#define STR_ISIN_5(x, a1, ...) strsame(x, a1) || STR_ISIN_4(x, __VA_ARGS__)
#define STR_ISIN_6(x, a1, ...) strsame(x, a1) || STR_ISIN_5(x, __VA_ARGS__)
#define STR_ISIN_7(x, a1, ...) strsame(x, a1) || STR_ISIN_6(x, __VA_ARGS__)
#define STR_ISIN_8(x, a1, ...) strsame(x, a1) || STR_ISIN_7(x, __VA_ARGS__)
#define STR_ISIN_9(x, a1, ...) strsame(x, a1) || STR_ISIN_8(x, __VA_ARGS__)
#define STR_ISIN_10(x, a1, ...) strsame(x, a1) || STR_ISIN_9(x, __VA_ARGS__)
#define STR_ISIN_11(x, a1, ...) strsame(x, a1) || STR_ISIN_10(x, __VA_ARGS__)
#define STR_ISIN_12(x, a1, ...) strsame(x, a1) || STR_ISIN_11(x, __VA_ARGS__)
#define STR_ISIN_13(x, a1, ...) strsame(x, a1) || STR_ISIN_12(x, __VA_ARGS__)
#define STR_ISIN_14(x, a1, ...) strsame(x, a1) || STR_ISIN_13(x, __VA_ARGS__)
#define STR_ISIN_15(x, a1, ...) strsame(x, a1) || STR_ISIN_14(x, __VA_ARGS__)
#define STR_ISIN_16(x, a1, ...) strsame(x, a1) || STR_ISIN_15(x, __VA_ARGS__)

// if the number of elements in the array is too large, you should call this
// function because the macro will add too much inline code. What is too large?
// Let's say 64. for now. maybe that's too high already.
// Note that these functions aren't needed for native Jet arrays, lists or any
// collection. The collection has a contains(...) method which does the job
// in a way that's best for and relevant to the collection's implementation.
// Also remember that Jet generates case #a, #b, #c not with the isin call but
// rather as case _a: case _b: case _c: etc. which is far better.
int int_isin(int n, int x, int arr[]) {
    for_to_where(i, n, arr[i] == x) return 1;
    return 0;
}

int cstr_isin(int n, char* x, char* arr[]) {
    for_to_where(i, n, strsame(x, arr[i])) return 1;
    return 0;
}