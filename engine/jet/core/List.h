#define List(T) PtrList
typedef struct PtrList {
    void* item;
    struct PtrList* next;
} PtrList;

monostatic PtrList* PtrList_with(void* item) {
    // TODO: how to get separate alloc counts of List_ASTType
    // List_ASTFunc etc.?
    PtrList* li = NEW(PtrList);
    li->item = item;
    return li;
}

monostatic PtrList* PtrList_withNext(void* item, void* next) {
    // TODO: how to get separate alloc counts of List_ASTType
    // List_ASTFunc etc.?
    PtrList* li = NEW(PtrList);
    // printf("%d\n", PtrList_allocTotal);
    li->item = item;
    li->next = next;
    return li;
}

monostatic int PtrList_count(PtrList* listPtr) {
    int i = 0;
    while (listPtr) {
        listPtr = listPtr->next;
        i++;
    }
    return i;
}

// returns the a ref to the last listitem so you can use that for
// repeated appends in O(1) and not O(N)
monostatic PtrList** PtrList_append(PtrList** selfp, void* item) {
    if (*selfp == NULL) { // first append call
        *selfp = PtrList_with(item);
        return selfp;
    } else {
        PtrList* self = *selfp;
        while (self->next) self = self->next;
        self->next = PtrList_with(item);
        return &(self->next);
    }
}

monostatic void PtrList_shift(PtrList** selfp, void* item) {
    *selfp = PtrList_withNext(item, *selfp);
}

#define foreach(T, var, listSrc) foreachn(T, var, _listp_, listSrc)
#define foreachn(T, var, listp, listSrc)                                       \
    for (PtrList* listp = listSrc; listp; listp = NULL)                        \
        for (T var = (T)listp->item; listp and (var = (T)listp->item);         \
             listp = listp->next)