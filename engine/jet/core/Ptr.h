typedef struct {
    char *start, *end, *pos;
} CPtr;

bool Ptr_seekby(CPtr* p, size_t n) {
    return (p->end - p->pos > n) && !!(p->pos += n);
}
bool Ptr_seekto(CPtr* p, size_t n) {
    return (p->end - p->start > n) && !!(p->pos = p->start + n);
}
bool Ptr_advance(CPtr* p) { return (p->pos < p->end) && !!++p; }
void Ptr_where(CPtr* p) { return p->pos - p->start; }
void Ptr_left(CPtr* p) { return p->end - p->pos; }
void Ptr_upoke(CPtr* p, char val) { *p->pos = val; }
char Ptr_upeek(CPtr* p) { return *p->pos; }
void Ptr_poke(CPtr* p, char val) {
    if (p->start <= p->pos && p->pos < p->end) Ptr__poke(p, val);
}
char Ptr_peek(CPtr* p) {
    return (p->start <= p->pos && p->pos < p->end) ? Ptr__peek(p) : 0;
}

/*
typedef struct {
    T *pos;
    Array(T)* arr;
} Ptr(T);

#define Ptr_seekby(T, p, n) if ((p)->end - (p)->pos > sizeof(T)*(n)) \
p->pos += (n);
#define Ptr_seekto(T, p, n) if ((p)->end - (p)->start > ((n)*sizeof(T))) \
(p)->pos = (p)->start + (n);
#define Ptr_advance(p) if ((p)->pos <( p)->end) ++(p);
#define Ptr_where(T, p) (((p)->pos - (p)->start))
#define Ptr_left(T, p) (((p)->end - (p)->pos - 1))

*/