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

// you cannot have a snap in-place, because you always return the char* and deal
// with that instead of the JetString*.
char* JetString_snap(char* jstr) {
    JetString* j = jstr - sizeof(JetString);
    if (!j->chain.used) return jstr; // JetString_snap(jstr);
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

// Do this before every write
// unless you can prove ref==1 for a given var at compile time
char* JetString_cow(char* jstr) {
    JetString* j = jstr - sizeof(JetString);
    if (j->refc > 1) return JetString_clone(jstr);
    return jstr;
}
#define JetString_COW(j) ((j) = JetString_cow(j))

void JetString_append(char* jstr, char* jnew) {
    // the chain inside a JetString is a JetString*, not a char*. When you
    // traverse this chain, you can only [... what?]
    JetString* j = jstr - sizeof(JetString);
    JetString* jn = jnew - sizeof(JetString);
    PtrArray_push(&j->chain, jn);
}

// void JetString_prepend(char* jstr, char* jnew) {
//     // the chain inside a JetString is a JetString*, not a char*. When you
//     // traverse this chain, you can only [... what?]
//     JetString* j = jstr - sizeof(JetString);
//     JetString* jn = jnew - sizeof(JetString);
//     PtrArray_shift(&j->chain, jn);
// }

void JetString_drop(char* jstr) {
    JetString* j = jstr - sizeof(JetString);
    j->refc--;
    for_to(i, j->chain.used) {
        JetString* ji = j->chain.ref[i];
        JetString_drop(ji->buf);
        // NO NO NO this will go on forever if there is a cycle
    }
}

void JetString_retain(char* jstr) {
    JetString* j = jstr - sizeof(JetString);
    j->refc++;
    for_to(i, j->chain.used) {
        JetString* ji = j->chain.ref[i];
        ji->refc++;
        // NO NO NO this will go on forever if there is a cycle
    }
}

#define T void*
struct Arr { // this is equivalent to PtrList
    T item;
    T next[]; // you have to cast it to (Arr)next to use it!!
    // T is used because it is the right size
    // BUT next will never be null here... now what
    // if T is a ptr type, then having T = null can mark the last element. But
    // you can no longer have nulls in the list as valid elements.
    // For non-ptr types you need a sentinel value that may be in-band depending
    // on your use case, too bad.
    // or you can keep a count and iterate on that.
};

struct Chain {
    struct Arr* item; // first item, ptr to array, whatever its the same
    int cap, used;
};

int main() {
    struct Chain c;
    foreach (double, v, &c)
        ;
}