
typedef struct {
    char* ref;
    int len, cap;
} JString;

monostatic void JString_print(const JString* const str) {
    // printf("---[%d]\n%.*s\n---\n", str->len, str->len, str->ref);
    printf("%.*s", str->len, str->ref);
}

#define roundUp32(x)                                                           \
    (--(x), (x) |= (x) >> 1, (x) |= (x) >> 2, (x) |= (x) >> 4,                 \
        (x) |= (x) >> 8, (x) |= (x) >> 16, ++(x))

monostatic void JString_resize(JString* str, int size) {
    printf("resize %p [%d] -> ", str->ref, str->cap);
    str->cap = size;
    str->ref = realloc(str->ref, str->cap);
    printf("%p [%d]\n", str->ref, str->cap);
}

monostatic void JString_growTo(JString* str, int size) {
    if (size > str->cap) JString_resize(str, roundUp32(size));
}

monostatic void JString_growBy(JString* str, int size) {
    JString_growTo(str, str->len + size);
}

monostatic void JString_appendCJString(JString* str, char* data, int size) {
    JString_growBy(str, size);
    memcpy(str->ref + str->len, data, size);
    str->len += size;
}

monostatic void JString_append(JString* str, JString* str2) {
    JString_appendCJString(str, str2->ref, str2->len);
}

monostatic int JString_len(JString* str) { return str->len; }

monostatic void JString_justPush(JString* str, char ch) {
    str->ref[str->len++] = ch;
}

monostatic void JString_push(JString* str, char ch) {
    JString_growBy(str, 1);
    JString_justPush(str, ch);
}

typedef enum { TextEncoding_ascii, TextEncoding_utf8 } TextEncoding;
monostatic JString* JString_iconv(
    JString* str, TextEncoding from, TextEncoding to) { }

monostatic JString slurp(const char* const filename) {
    JString ret = {};
    FILE* file = fopen(filename, "r");
    if (!file) return ret;
    fseek(file, 0, SEEK_END);
    const size_t size = ftell(file) + 1;
    char* data = malloc(size);
    if (!data) return ret;
    fseek(file, 0, SEEK_SET);
    fread(data, size, 1, file);
    data[size - 1] = 0;
    ret.ref = data;
    ret.len = ret.cap = size;
    fclose(file);
    return ret;
}