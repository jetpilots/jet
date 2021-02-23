
typedef struct {
    UInt32* idxs;
    Real64* vals;
    UInt32 countnz, counttot;
    // you need counttot only to find inbounds, do lookup etc. OR DO YOU?
    // you also need to match counttot when doing a vecmul or dotproduct etc.
} SpVector;

// NO RESIZING on the compressed formats. For that you should use a KDDict
// instead.

void SpVector_init(SpVector* svec, UInt32 countnz, UInt32 counttot) {
    // Single allocation for both idxs and vals. These vectors are not
    // meant to be resized.
    svec->idxs = malloc(countnz * (sizeof(UInt32) + sizeof(Real64)));
    svec->countnz = countnz;
    svec->counttot = counttot;
    svec->vals = (Real64*)(svec->idxs + svec->countnz);
}

void SpVector_fromVector(SpVector* svec, Vector* vec) {
    UInt32 cnz = Vector_countnz(vec);
    SpVector_init(svec, cnz, vec->used);
    UInt32 j = 0;
    for_to(i, vec->used) if (vec->ref[i] != 0.0) {
        svec->idxs[j] = i;
        svec->vals[j] = vec->ref[i];
        j++;
    }
}
/// Takes an array of indexes and an array of values and sets them in the sparse
/// vector. The arrays should be sorted in increasing idx. The arrays should
/// have the same size as each other and as the sparse vector svec. Ensuring
/// this is completely YOUR responsibility.
void SpVector_set(SpVector* svec, UInt32 idxs[], Real64 vals[]) {
    for_to(pos, svec->countnz) {
        svec->idxs[pos] = idxs[pos];
        svec->vals[pos] = vals[pos];
    }
}

void SpVector_print_prec(SpVector* svec, int prec) {
    printf("{\n");
    for_to(pos, svec->countnz - 1)
        printf("    [%u] = %.*g,\n", svec->idxs[pos], prec, svec->vals[pos]);
    if (svec->countnz)
        printf("    [%u] = %.*g\n", svec->idxs[svec->countnz - 1], prec,
            svec->vals[svec->countnz - 1]);
    printf("}\n");
}
void SpVector_print(SpVector* svec) { SpVector_print_prec(svec, 15); }
void SpVector_print0_prec(SpVector* svec, int prec) {
    UInt32 last = 0;
    printf("[");

    for_to(pos, svec->countnz - 1) {
        for_to(i, svec->idxs[pos] - last) printf("0, ");
        printf("%.*g, ", prec, svec->vals[pos]);
        last = svec->idxs[pos] + 1;
    }
    if (svec->countnz) {
        for_to(i, svec->idxs[svec->countnz - 1] - last) printf("0, ");
        printf("%.*g", prec, svec->vals[svec->countnz - 1]);
    }
    printf("]\n");
}
void SpVector_print0(SpVector* svec) { SpVector_print0_prec(svec, 15); }
void SpVector_free(SpVector* svec) {
    free(svec->idxs);
    *svec = (SpVector) {};
}

Real64 SpVector_dotproduct(SpVector* svec1, SpVector* svec2) {
    Real64 sum = 0;
    // pos1 and pos2 are loop counters over i and j arrays
    UInt32 pos1 = 0, end1 = svec1->countnz;
    UInt32 pos2 = 0, end2 = svec2->countnz;
    while (pos1 < end1 && pos2 < end2) {
        // printf("%d %d\n", pos1, pos2);
        while (pos1 < end1 && svec1->idxs[pos1] < svec2->idxs[pos2]) pos1++;
        while (pos2 < end2 && svec2->idxs[pos2] < svec1->idxs[pos1]) pos2++;
        if (svec1->idxs[pos1] == svec2->idxs[pos2]) {
            // printf("hit %d %d %u %u %g %g\n", pos1, pos2, svec1->idxs[pos1],
            // svec2->idxs[pos2], svec1->vals[pos1], svec2->vals[pos2]);
            sum += svec1->vals[pos1++] * svec2->vals[pos2];
        }
    }
    return sum;
}

Real64 SpVector_rmsv(SpVector* svec1, SpVector* svec2) {
    return sqrt(SpVector_dotproduct(svec1, svec2));
}

/// In-place multiply
void SpVector_scale1(SpVector* svec, Real64 num) {
    for_to(pos, svec->countnz) svec->vals[pos] *= num;
}

void SpVector_scale(SpVector* svec, SpVector* other) {
    UInt32 pos1 = 0, end1 = svec->countnz;
    UInt32 pos2 = 0, end2 = other->countnz;
    while (pos1 < end1 && pos2 < end2) {
        while (pos1 < end1 && svec->idxs[pos1] < other->idxs[pos2]) pos1++;
        while (pos2 < end2 && other->idxs[pos2] < svec->idxs[pos1]) pos2++;
        if (svec->idxs[pos1] == other->idxs[pos2])
            svec->vals[pos1++] *= other->vals[pos2];
    }
}

void SpVector_scalev(SpVector* svec, Vector* other) {
    for_to(pos, svec->countnz) svec->vals[pos] *= other->ref[svec->idxs[pos]];
}
/// Finds (with binary search) and returns the position of the requested index
/// if present, or else the length of the input vector.
UInt32 Vector_hasIndex(Vector* vec, UInt32 idx) { return 0; };
/// Norm with respect to a scalar. When this is the mean, you get stddev
Real64 SpVector_distsqr1(SpVector* svec, Real64 ref) {
    Real64 sum = sq(ref) * (svec->counttot - svec->countnz);
    for_to(pos, svec->countnz) sum += sq(svec->vals[pos] - ref);
    return sum;
}

/// Norm with respect to a SpVector
Real64 SpVector_distsqr(SpVector* svec1, SpVector* svec2) {
    Real64 sum = 0;
    // pos1 and pos2 are loop counters over i and j arrays
    UInt32 pos1 = 0, end1 = svec1->countnz;
    UInt32 pos2 = 0, end2 = svec2->countnz;
    while (pos1 < end1 && pos2 < end2) {
        while (pos1 < end1 && svec1->idxs[pos1] < svec2->idxs[pos2])
            sum += sq(svec1->vals[pos1++]);
        while (pos2 < end2 && svec2->idxs[pos2] < svec1->idxs[pos1])
            sum += sq(svec2->vals[pos2++]);
        if (svec1->idxs[pos1] == svec2->idxs[pos2])
            sum += sq(svec1->vals[pos1++] - svec2->vals[pos2]);
    }
    return sum;
}
// {
//     BasedOnStyle : WebKit,
//     ColumnLimit : 80,
//     AllowShortIfStatementsOnASingleLine : true,
//     AllowShortLoopsOnASingleLine : true,
//     AccessModifierOffset : 0
// } {
//     BasedOnStyle : WebKit,
//     ColumnLimit : 80,
//     BreakBeforeBinaryOperators : true,
//     AllowShortIfStatementsOnASingleLine : true,
//     AllowShortLoopsOnASingleLine : true,
//     AllowShortFunctionsOnASingleLine : true,
//     AccessModifierOffset : 0
// }
/// Norm with respect to a Vector. TODO: make better use of loop counters /
/// convert to pointers where you can
Real64 SpVector_distsqrv(SpVector* svec, Vector* other) {
    Real64 sum = 0;
    UInt32 last = 0;
    for_to(pos, svec->countnz - 1) {
        for_to(i, svec->idxs[pos] - last) sum += sq(other->ref[last + i]);
        sum += sq(svec->vals[pos] - other->ref[svec->idxs[pos]]);
        last = svec->idxs[pos] + 1;
    }
    if (svec->countnz) {
        UInt32 pos = svec->countnz - 1;
        for_to(i, svec->idxs[pos] - last) sum += sq(other->ref[last + i]);
        sum += sq(svec->vals[pos] - other->ref[svec->idxs[pos]]);
    }
    return sum;
}

/// Equivalent to calling SpVector_dotproduct(svec, svec) but more efficient.
Real64 SpVector_magsqr(SpVector* svec) {
    Real64 sum = 0;
    for_to(pos, svec->countnz) sum += sq(svec->vals[pos]);
    return sum;
}

Real64 SpVector_mag(SpVector* svec) { return sqrt(SpVector_magsqr(svec)); }