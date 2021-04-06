#include "../base.h"
#include "numstr.c"

typedef struct {
    PtrArray dat;
    // numstr* ref;
    long rows, cols;
} DataFrame;

numstr* DataFrame__getp(DataFrame df, int row, int col) {
    return df.dat.ref + row * df.rows + col;
}
double DataFrame_get(DataFrame df, int row, int col) {
    numstr* tgt = DataFrame__getp(df, row, col);
    return numstr_get(tgt);
}

void DataFrame__convall(DataFrame df) {
    for (numstr* ai = df.dat.ref; ai < df.dat.ref + df.rows * df.cols; ai++)
        numstr_get(ai);
}

void DataFrame_dumpbin(DataFrame df, const char* outfile) {
    DataFrame__convall(df);
    // dump...
}

void DataFrame_dumpcsv(DataFrame df, const char* outfile) {
    // you DON'T need to to do _convall here!
    // strings will be written out as strings
    // a nice test case: read a csv, modify a few values & write csv back.
    // benchmark with other language CSV reader/writers. they usually
    // don't have numstrs
}

void DataFrame__pushstrn(DataFrame df, size_t sz, char* arr[]) {
    // numstr* item = arr;
    for (int i = 0; i < sz; i++) PtrArray_push(&df.dat, arr[i]);
}

// BTW dataframe should support both strings and nums!!!
void DataFrame_pushstrrow(DataFrame df, PtrArray row) {
    assert(row.used == df.cols);
    DataFrame__pushn(df, df.cols, row.ref);
}

DataFrame readcsv(const char* filename, char sep) {
    DataFrame ret = {};
    String str = slurp(filename);
    // ^ if you do strdup for each line it may be better for GC
    // since individual lines can be released after done

    // you really should cache CSV files into a binary file in the user's
    // home folder (make a jetcache folder). store bin files based on full
    // path name of the CSV, and then just load them whenever asked to load
    // a csv (unless the CSV is really newer).

    char* pos = str.ref;
    static PtrArray row;
    int refcols = 1, cols;
    while (*pos && *pos != '\n')
        if (*pos == sep) refcols++;

    while (*pos) { // within file
        cols = refcols;
        while (*pos && *pos != '\n') { // each row
            PtrArray_push(&row, pos);

            while (*pos && *pos != '\n' && *pos != sep) pos++; // each column

            // assert(*pos == sep || *pos == '\n');
            // int islast = *pos == '\n';
            if (*pos == sep) *pos++ = 0; // trample separator (comma/semicolon)
            cols--;
        }

        if (*pos == '\n') *pos++ = 0; // trample newline
        if (cols)
            ; // error raise
        DataFrame_pushstrrow(ret, row);
        PtrArray_reset(&row);
    }
}