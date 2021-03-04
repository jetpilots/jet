
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iconv.h>

char* iconvert(char* str, int len, char* from) {
    int olen = len * 1.25;
    char* buf = malloc(olen);
    iconv_t ic = iconv_open(from, "utf-8");
    iconv(ic, &str, len, &buf, olen);
    iconv_close(ic);
    return buf;
}
