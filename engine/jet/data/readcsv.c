#include "../base.h"
#include "numstr.c"

typedef struct {
    ptrarray_t dat;
    // numstr_t* ref;
    long rows, cols;
} dataframe_t;

numstr_t* dataframe__getp(dataframe_t df, int row, int col) {
    return df.dat.ref + row * df.rows + col;
}
double dataframe_get(dataframe_t df, int row, int col) {
    numstr_t* tgt = dataframe__getp(df, row, col);
    return numstr_get(tgt);
}

void dataframe__convall(dataframe_t df) {
    for (numstr_t* ai = df.dat.ref; ai < df.dat.ref + df.rows * df.cols; ai++)
        numstr_get(ai);
}

void dataframe_dumpbin(dataframe_t df, const char* outfile) {
    dataframe__convall(df);
    // dump...
}

void dataframe_dumpcsv(dataframe_t df, const char* outfile) {
    // you DON'T need to to do _convall here!
    // strings will be written out as strings
    // a nice test case: read a csv, modify a few values & write csv back.
    // benchmark with other language CSV reader/writers. they usually
    // don't have numstrs
}

void dataframe__pushstrn(dataframe_t df, size_t sz, char* arr[]) {
    // numstr_t* item = arr;
    for (int i = 0; i < sz; i++) PtrArray_push(&df.dat, arr[i]);
}

// BTW dataframe should support both strings and nums!!!
void dataframe_pushstrrow(dataframe_t df, ptrarray_t row) {
    assert(row.used == df.cols);
    dataframe__pushn(df, df.cols, row.ref);
}

dataframe_t readcsv(const char* filename, char sep) {
    dataframe_t ret = {};
    String str = slurp(filename);
    // ^ if you do strdup for each line it may be better for GC
    // since individual lines can be released after done

    // you really should cache CSV files into a binary file in the user's
    // home folder (make a jetcache folder). store bin files based on full
    // path name of the CSV, and then just load them whenever asked to load
    // a csv (unless the CSV is really newer).

    char* pos = str.ref;
    static ptrarray_t row;
    int refcols = 1, cols;
    while (*pos && *pos != '\n')
        if (*pos == sep) refcols++;

    while (*pos) { // within file
        cols = refcols;
        while (*pos && *pos != '\n') { // each row
            PtrArray_push(&row, pos);

            while (*pos && *pos != '\n' && *pos != sep) pos++; // each column

            if (*pos == sep) *pos++ = 0; // trample separator (comma/semicolon)
            cols--;
        }

        if (*pos == '\n') *pos++ = 0; // trample newline
        if (cols)
            ; // error raise
        dataframe_pushstrrow(ret, row);
        PtrArray_reset(&row);
    }
}