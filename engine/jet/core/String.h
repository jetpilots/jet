
typedef struct {
    char* ref;
    int len, cap;
} String;

jet_static void String_print(const String* const str) {
    // printf("---[%d]\n%.*s\n---\n", str->len, str->len, str->ref);
    printf("%.*s", str->len, str->ref);
}

#define roundUp32(x)                                                           \
    (--(x), (x) |= (x) >> 1, (x) |= (x) >> 2, (x) |= (x) >> 4,                 \
        (x) |= (x) >> 8, (x) |= (x) >> 16, ++(x))

jet_static void String_resize(String* str, int size) {
    printf("resize %p [%d] -> ", str->ref, str->cap);
    str->cap = size;
    str->ref = realloc(str->ref, str->cap);
    printf("%p [%d]\n", str->ref, str->cap);
}

jet_static void String_growTo(String* str, int size) {
    if (size > str->cap) String_resize(str, roundUp32(size));
}

jet_static void String_growBy(String* str, int size) {
    String_growTo(str, str->len + size);
}

jet_static void String_appendChars(String* str, char* data, int size) {
    String_growBy(str, size);
    memcpy(str->ref + str->len, data, size);
    str->len += size;
}

jet_static void String_append(String* str, String* str2) {
    String_appendChars(str, str2->ref, str2->len);
}

jet_static int String_len(String* str) {
    if (!str->len && str->ref && *str->ref) str->len = strlen(str->ref);
    return str->len;
}

jet_static char* String_end(String* str) { return str->ref + String_len(str); }

jet_static void String_justPush(String* str, char ch) {
    str->ref[str->len++] = ch;
}

jet_static void String_push(String* str, char ch) {
    String_growBy(str, 1);
    String_justPush(str, ch);
}

typedef enum { TextEncoding_ascii, TextEncoding_utf8 } TextEncoding;
jet_static String* String_iconv(
    String* str, TextEncoding from, TextEncoding to) {
    return NULL;
}

jet_static String slurp(const char* const filename) {
    String ret = {};
    FILE* file = fopen(filename, "r");
    if (!file) return ret;
    fseek(file, 0, SEEK_END);
    const size_t size = ftell(file) + 1;
    char* data = malloc(size);
    if (!data) return ret;
    fseek(file, 0, SEEK_SET);
    fread(data, size, 1, file);
    ret.ref = data;
    ret.cap = size;
    ret.len = size - 1;
    ret.ref[ret.len] = 0;
    fclose(file);
    return ret;
}