
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

char unhexc(char* inp) {
    static const char char2dec[256] = { //
        ['0'] = 0x00,
        ['1'] = 0x01,
        ['2'] = 0x02,
        ['3'] = 0x03,
        ['4'] = 0x04,
        ['5'] = 0x05,
        ['6'] = 0x06,
        ['7'] = 0x07,
        ['8'] = 0x08,
        ['9'] = 0x09,
        ['a'] = 0x0a,
        ['b'] = 0x0b,
        ['c'] = 0x0c,
        ['d'] = 0x0d,
        ['e'] = 0x0e,
        ['f'] = 0x0f,
        ['A'] = 0x0A,
        ['B'] = 0x0B,
        ['C'] = 0x0C,
        ['D'] = 0x0D,
        ['E'] = 0x0E,
        ['F'] = 0x0F
    };
    return (char2dec[inp[0]] << 4) + char2dec[inp[1]];
}

// in-place, because decode can do it
char* qdecode(char* src, char* tgt, int len) {
    char* end = src + len;
    while (src < end /* && tgt < end*/) {
        if (*src == '=') {
            *tgt++ = unhexc(++src);
            src += 2;
        } else {
            *tgt++ = *src++;
        }
    }
    *tgt = 0;
    return tgt;
}

typedef union {
    char enc[4];
    struct {
        char d1 : 6, d2 : 6, d3 : 6, d4 : 6;
    } dec;
} fourbytes;
static const unsigned char pr2six[256] = {
    /* ASCII table */
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 62, 64, 64, 64, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60,
    61, 64, 64, 64, 64, 64, 64, 64, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
    13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64, 64,
    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,
    45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64
};
// in-place, because decode can do it
char* bdecode(char* src, char* tgt, int len) {
    char* end = src + len;

    while (src < end) {

        char c3[] = { //
            (unsigned char)(pr2six[src[0]] << 2 | pr2six[src[1]] >> 4),
            (unsigned char)(pr2six[src[1]] << 4 | pr2six[src[2]] >> 2),
            (unsigned char)(pr2six[src[2]] << 6 | pr2six[src[3]])
        };

        for (int i = 0; i < 3; i++) *tgt++ = c3[i];

        //     fourbytes *b4src = src, *b4tgt = tgt;
        //     // this can be done inplace in this order since there is no
        //     // read after write dependency
        //     b4tgt->dec.d1 = bcharmap[b4->enc[0]];
        //     b4tgt->dec.d2 = bcharmap[b4->enc[1]];
        //     b4tgt->dec.d3 = bcharmap[b4->enc[2]];
        //     b4tgt->dec.d4 = bcharmap[b4->enc[3]];

        // tgt += 3;
        src += 4;
    }
    *tgt = 0;
    return tgt;
}

// in-place, because decode can do it. encoded-word decoding
void ewdecode(char* str, int len) {
    char *src = str, *tgt = str, *end = str + len;
    while (src < end) {
        if (*src++ != '=') return;
        if (*src++ != '?') return;
        char* enc = src;
        char* sep = strchr(src, '?');
        if (sep) *sep++ = 0; // now enc has encoding
        char encoding[32];
        snprintf(encoding, 32, "%s", enc);

        char mode = *sep++; // B or Q
        src = ++sep; // skip over next ?
        sep = strchr(src, '?');
        if (!sep)
            sep = end;
        else {
            // sep[0] = 0; // ?
            // sep[1] = 0; // =
        }
        int sublen = sep - src;
        switch (mode) {
        case 'B':
            tgt = bdecode(src, tgt, sublen);
            break;
        case 'Q':
            tgt = qdecode(src, tgt, sublen);
            break;
        default:
            *src = 0;
            return;
        }
        src += sublen + 2;
        // if (*src == '\r') {
        //     src++;
        //     if (*src == '\n') {
        //         src++;
        //         if (*src == ' ') src++;
        //     }
        // }
        // while (*src == '\n' || *src == '\r') src++;
        while (src < end && *src != '=') *tgt++ = *src++;
    }
    *tgt = 0;
}

// static const char* tests[]
//     = { "=F0=9F=94=A5=F0=9F=94=A5=F0=9F=94=A5MEGAWEEK=F0=9F=94=A5="
//         "F0=9F=94=A5=F0=9F=94=A5=C2=A0Monday=2DSunday=20=2D=20Takeaway="
//         "20Pizza=20for=205=C2=A0CHF=21" };

static const char* testsjhgh[]
    = { "=?utf-8?B?VGhhbmsgeW91IGZvciBldmFsdWF0aW5nIEludGVswq4gUGFy?=\n "
        "=?utf-8?B?YWxsZWwgU3R1ZGlvIFhFIENsdXN0ZXIgRWRpdGlvbiBmb3IgTGludXgq?=",
          "=?ISO-8859-1?B?"
          "R2VyYWRlIGluIElocmVtIFBhcnNoaXAtUG9zdGVpbmdhbmcgZWluZ2V0cm9mZmVuOiBl"
          "aW4gS29tcGxpbWVudCB2b24gaWhyLCBBbnfkbHRpbiwgMzEg?=",
          "=?utf-8?Q?=F0=9F=94=A5=F0=9F=94=A5=F0=9F=94=A5MEGAWEEK=F0=9F=94=A5="
          "F0=9F=94=A5=F0=9F=94=A5=C2=A0Monday=2DSunday=20=2D=20Takeaway="
          "20Pizza=20for=205=C2=A0CHF=21?=",
          "=?utf-8?Q?Domino=27s=20Pizza?= <no-reply@dominos.ch>",
          "=?ISO-8859-1?B?"
          "U2llLCAyOSwgVmVyd2FsdHVuZ3Nhbmdlc3RlbGx0ZS9TdHVkZW50aW4gTUFTLCBs5GNo"
          "ZWx0IFNpZSBhbiA=?=" };

int maain() {
    // char* t = tests[0];
    char t[] = "=F0=9F=94=A5=F0=9F=94=A5=F0=9F=94=A5MEGAWEEK=F0=9F=94=A5="
               "F0=9F=94=A5=F0=9F=94=A5=C2=A0Monday=2DSunday=20=2D=20Takeaway="
               "20Pizza=20for=205=C2=A0CHF=21";
    char b[]
        = "U2llLCAyOSwgVmVyd2FsdHVuZ3Nhbmdlc3RlbGx0ZS9TdHVkZW50aW4gTUFTLCBs5GNo"
          "ZWx0IFNpZSBhbiA";
    puts(t);
    qdecode(t, t, strlen(t));
    puts(t);
    puts("");
    puts(b);
    bdecode(b, b, strlen(b));
    puts(b);
    puts("");

    for (int i = 0; i < 5; i++) {
        char* ts = strdup(testsjhgh[i]);
        puts(ts);
        ewdecode(ts, strlen(testsjhgh[i]));
        puts(ts);
        puts("");
    }
}