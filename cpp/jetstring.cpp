#include "jet/base.h"

typedef struct JetString {
    int size, refc;
    PtrArray chain;
    char buf[];
} JetString;

char* JetString_init(size_t size) {
    JetString* s = malloc(sizeof(JetString) + size + 1);
    s->size = size;
    s->refc = 1;
    s->chain.used = s->chain.cap = 0;
    s->chain.ref = NULL;
    s->buf[size] = 0;
    char* ret = s + sizeof(JetString);
    return ret;
}

char* JetString_snap(char* jstr) {
    JetString* j = jstr - sizeof(JetString);
    if (!j->chain.used) return jstr;
    int sum = 1;
    for_to(i, j->chain.used) {
        JetString* ji = j->chain.ref[i];
        sum += ji->size;
    }
    JetString* jnew = JetString_init(sum);
    for_to(i, j->chain.used) {
        JetString* ji = j->chain.ref[i];
        memmove(jnew->buf, ji->buf, ji->size);
    }
    char* ret = jnew - sizeof(JetString);
    return ret;
}

char* JetString_clone(char* jstr) {
    JetString* j = jstr - sizeof(JetString);
    if (j->chain.used) return JetString_snap(jstr);
    JetString* jnew = JetString_init(j->size);
    memmove(jnew->buf, j->buf, j->size);
}

char* JetString_cow(char* jstr) {
    JetString* j = jstr - sizeof(JetString);
    if (j->refc > 1) return JetString_clone(jstr);
    return jstr;
}
#define JetString_COW(j) ((j) = JetString_cow(j))

void JetString_append(char* jstr, char* jnew) {

    JetString* j = jstr - sizeof(JetString);
    JetString* jn = jnew - sizeof(JetString);
    PtrArray_push(&j->chain, jn);
}

void JetString_drop(char* jstr) {
    JetString* j = jstr - sizeof(JetString);
    j->refc--;
    for_to(i, j->chain.used) {
        JetString* ji = j->chain.ref[i];
        JetString_drop(ji->buf);
    }
}

void JetString_retain(char* jstr) {
    JetString* j = jstr - sizeof(JetString);
    j->refc++;
    for_to(i, j->chain.used) {
        JetString* ji = j->chain.ref[i];
        ji->refc++;
    }
}

#define T void*
struct Arr {
    T item;
    T next[];
};

struct Chain {
    struct Arr* item;
    int cap, used;
};

int main() {
    struct Chain c;
    foreach (double, v, &c)
        ;
}