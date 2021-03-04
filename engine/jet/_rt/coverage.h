#define JET_COVERAGE_UP(l)                                                     \
    { _cov_[l - 1]++; }

static void coverage_report() {
    int count = 0, l = NUMLINES;
    while (--l > 0) count += !!_cov_[l];
    printf("coverage: %d/%d lines = %.2f%%\n", count, NUMLINES,
        count * 100.0 / NUMLINES);
}
