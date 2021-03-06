
typedef struct String {
  char* ref;
  int len, cap;
} String;

monostatic void String_drop_(String* str) { }
monostatic void String_free_(String* str) { }

monostatic void String_print(const String* const str) {
  // printf("---[%d]\n%.*s\n---\n", str->len, str->len, str->ref);
  printf("%.*s", str->len, str->ref);
}
monostatic void String_json_(const String* const str, int level) {
  // printf("---[%d]\n%.*s\n---\n", str->len, str->len, str->ref);
  printf("\"%.*s\"", str->len, str->ref);
}

#define roundUp32(x)                                                       \
  (--(x), (x) |= (x) >> 1, (x) |= (x) >> 2, (x) |= (x) >> 4,               \
    (x) |= (x) >> 8, (x) |= (x) >> 16, ++(x))

monostatic void String_resize(String* str, int size) {
  eprintf("resize %p [%d] -> ", str->ref, str->cap);
  str->cap = size;
  str->ref = realloc(str->ref, str->cap);
  eprintf("%p [%d]\n", str->ref, str->cap);
}

monostatic String String_fromCString(char* str) {
  return (String) { .ref = str, .len = cstr_len(str) };
}

monostatic void String_growTo(String* str, int size) {
  if (size > str->cap) String_resize(str, roundUp32(size));
}

monostatic void String_growBy(String* str, int size) {
  String_growTo(str, str->len + size);
}

monostatic void String_appendChars(String* str, char* data, int size) {
  String_growBy(str, size);
  jet_mem_copy(str->ref + str->len, data, size);
  str->len += size;
}

monostatic void String_append(String* str, String* str2) {
  String_appendChars(str, str2->ref, str2->len);
}

monostatic int String_len(String* str) {
  if (!str->len && str->ref && *str->ref) str->len = cstr_len(str->ref);
  return str->len;
}

monostatic char* String_end(String* str) {
  return str->ref + String_len(str);
}

monostatic void String_justPush(String* str, char ch) {
  str->ref[str->len++] = ch;
}

monostatic void String_push(String* str, char ch) {
  String_growBy(str, 1);
  String_justPush(str, ch);
}

typedef enum { Charset_ascii, Charset_utf8 } Charset;
monostatic String* String_iconv(String* str, Charset from, Charset to) {
  return NULL;
}

extern thread_local const char* _err_;
monostatic String slurp(const char* const filename) {
  // TODO: change the error messages to setting _err_ which is the standard
  // jet way of doing things
  String ret = {};
  FILE* file = fopen(filename, "r");
  if (!file) {
    eprintf("slurp: file %s could not be opened\n", filename);
    return ret;
  }

  fseek(file, 0, SEEK_END);
  const size_t size = ftell(file) + 1;
  char* data = malloc(size);
  if (!data) {
    eprintf("slurp: file %s too big to fit in memory\n", filename);
    return ret;
  }
  fseek(file, 0, SEEK_SET);
  if (fread(data, size, 1, file) != 1) {
    eprintf("slurp: file %s could not be read fully\n", filename);
    return ret;
  }
  ret.ref = data;
  ret.cap = size;
  ret.len = size - 1;
  ret.ref[ret.len] = 0;
  fclose(file);
  return ret;
}