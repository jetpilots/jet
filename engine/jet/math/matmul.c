// returns scalar (dot product)
double matmul11(double* arr1, double* arr2, int n) {
    double sum = 0, *aend = arr1 + n;
    for (double *a1 = arr1, *a2 = arr2; a1 < aend; a1++, a2++) sum += *a1 * *a2;
    return sum;
}

// ^^ NEED 4 combinations of dot product; bwtween array & slice. what about
// selection & filter

// returns scalar. this func is helpful because you can use it when scalarising
// a vector expr in Jet, you can call this in a generated loop instead of
// collecting the array. For times when you want the collected array call
// matmul12 and get a malloc'd array.
double matmul12i(double* arr, double* tens, int n, int i) {
    // here you need the dot of the arr and the ith tensor row.
    return matmul11(arr, tens + i * n, n);
}
/*
func dot(arr1[], arr2[]) sum
    for a1 in arr1[], a2 in arr2[] do sum += a1*a2
end
matmul12i(arr[], tens[:,:], i) := dot(arr, tens[i,:])
*/

// returns vector
double* matmul12(double* arr, double* tens, int n) {
    // FIXME; ret size is NOT n but tens other dim!
    double* ret = malloc(n * sizeof(double));
    for (int i = 0; i < n; i++) ret[i] = matmul12i(arr, tens, n, i);
    return ret;
}

// DOT PRODUCT IS COMMUTATIVE, so reuse the one above
// double matmul21i(double* arr, double* tens, int n, int i) {
//     // here you need the dot of the arr and the ith tensor row.
//     return dot(arr, tens + i * n, n);
// }

double* matmul21(double* tens, double* arr, int n) {
    // FIXME; ret size is NOT n but tens other dim!
    double* ret = malloc(n * sizeof(double));
    for (int i = 0; i < n; i++) ret[i] = matmul12i(arr, tens, n, i);
    // DOT PRODUCT IS COMMUTATIVE, so reused the one above
    return ret;
}
// ^ INTERNALLY, matmul21 and matmul12 are the same at C level. Maybe you
// don't need both. At Jet level you cannot mix up row and column vectors.
// So have a .iscol runtime flag in arrays (only needed for num array)
// so that you can check operations. Or maybe do it at compile time?
// the way to change is transpose!(arr)
// apropos inplace funcs, how about allowing transpose() and transpose!()
// but then the linter wont auto add !.  NO NO NO it breaks the whole idea
// since ppl will have to define transpose as transpose!(copy()) which jet
// should do, not least because jet can elide the copy if conditions allow.
// Also ignoring return values is an  error if the function is pure.

// returs scalar. instead of this way you should define Array_dot and Slice_dot.
// double matmul22ij(double* tens1, double* tens2, int n, int i, int j) {
//     double sum = 0, *aend = tens1 + n;
//     for (double *a1 = tens1+i*n, *a2 = tens2; a1 < aend; a1++, a2++) sum +=
//     *a1 * *a2; return sum;
// }

double* matmul22(double* tens1, double* tens2, int n) {
    double* ret = malloc();
    double* retp = ret;
    for (int j = 0; j < m2; j++)
        for (int i = 0; i < m1; i++)
            *retp++ = matmul22ij(tens1, tens2, n, i, j);
    return ret;
}