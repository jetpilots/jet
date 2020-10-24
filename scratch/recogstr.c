#include <stdio.h>
#include <string.h>
typedef char bool;

static const char* words[] = { "function", "test", "returns", "type", "var",
    "let", "return", "as", "and", "check", "extends", "public", "private",
    "print", "describe", "json", "repr" };

static const size_t nWords = 1; // sizeof(words) / sizeof(words[0]);

static const char* const spaces64 = //
    "                                                                ";
static const int indentStep = 4;
char accum[64] = {};

bool hasPrefix(char* word, int len) {
    for (int iw = 0; iw < nWords; iw++)
        if (!strncmp(words[iw], word, len)) return 1;
    return 0;
}

bool hasWord(char* word) {
    for (int iw = 0; iw < nWords; iw++)
        if (!strcmp(words[iw], word)) return 1;
    return 0;
}

const char* firstMatchingWordWithPrefix(char* word, int len) {
    for (int iw = 0; iw < nWords; iw++)
        if (!strncmp(words[iw], word, len)) return words[iw];
    return "--";
}

void recur__old(int indent) {
    bool charUsed[256] = {};
    printf("%.*sswitch(*++yycursor) {\n", indent, spaces64);
    for (int iw = 0; iw < nWords; iw++) {
        if (!charUsed[*words[iw]])
            printf("%.*scase '%c':\n", indent, spaces64, *words[iw]);
        charUsed[*words[iw]] = 1;
    }
    printf("%.*s}\n", indent, spaces64);
}

static const char charSet[] = "abcdefghijklmnopqrstuvwxyz1234567890";
static const int nCharSet
    = sizeof(charSet) - 1; // need the \0 as part of the charset

void recur(int level) {
    // if (level > 10) return;
    if (level && !accum[level - 1]) return;
    const int indent = (level + 1) * indentStep;
    printf("%.*sswitch(*++pos) {\n", indent, spaces64);
    // printf("// accum: %s\n", accum);
    if (hasWord(accum))
        printf("%.*scase '\\0': THE_CODE(%s);\n", indent, spaces64, accum);

    for (int j = 0; j < nCharSet; j++) {
        for (int iw = 0; iw < nWords; iw++) {
            accum[level] = charSet[j]; //*words[iw];
            // if (*words[iw] != *accum) continue;
            // printf("%.*s%s %s %d\n", indent, spaces64, words[iw], accum,
            // level + 1);
            if (hasPrefix(accum, level + 1)) {
                printf("%.*scase '%c':\n", indent, spaces64, accum[level]);
                recur(level + 1);
                accum[level + 1] = 0;
                printf("%.*sbreak;\n", indent + indentStep, spaces64);
                break;
            }
        }
    }
    printf("%.*s}\n", indent, spaces64);
}

int main() {
    // char* pos[nWords];
    // memcpy(pos, words, nWords * sizeof(char*));
    // printf("%s\n", firstMatchingWordWithPrefix("function", 8));
    // printf("%s\n", firstMatchingWordWithPrefix("le", 2));
    // printf("%s\n", firstMatchingWordWithPrefix("vm", 2));
    // printf("%s\n", firstMatchingWordWithPrefix("fnuc", 4));
    // printf("%s\n", firstMatchingWordWithPrefix("func", 4));
    // puts("#include <stdio.h>");
    // puts("#define THE_CODE(x) puts(#x); return 0;\n"
    //      "#define THE_ERRHANDLER(x) puts(\"not found\"); return 1;\n");
    puts("int THE_FUNCNAME(const char* const str) {");
    puts("    const char* pos = str-1;");
    recur(0);
    puts("    THE_ERRHANDLER(str);");
    puts("}");
    // for (int iw = 0; iw < nWords; iw++) puts(words[iw]);
    // char w[nWords][9] = {}; // = words;
    // for (int i = 0; i < nWords; i++)
    //     for (int j = 0; j < 8; j++) w[i][j] = words[j][i];
    // for (int iw = 0; iw < nWords; iw++) puts(w[iw]);
}
