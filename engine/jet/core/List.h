#define List(T) PtrList
typedef struct PtrList PtrList;
struct PtrList {
    void* item;
    PtrList* next;
};

#define NEWW(T, ...) memcpy(NEW(T), &(T) { __VA_ARGS__ }, sizeof(T))

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
    int i;
    for (i = 0; listPtr; i++) listPtr = listPtr->next;
    return i;
}

// returns the a ref to the last listitem so you can use that for
// repeated appends in O(1) and not O(N)
monostatic PtrList** PtrList_append(PtrList** selfp, void* item) {
    // if (*selfp == NULL) { // first append call
    //     *selfp = PtrList_with(item);
    //     return selfp;`
    // } else {
    //     PtrList* self = *selfp;
    //     while (self->next) self = self->next;
    //     self->next = PtrList_with(item);
    //     return &(self->next);
    // }

    // while((*selfp)->next) *selfp=(*selfp)->next;

    while (*selfp) selfp = &(*selfp)->next;
    *selfp = PtrList_with(item);
    return &(*selfp)->next;
    // if you return selfp, you can do a fast pop or just access the last item
    // if you return &(*selfp)->next you can do the next append even faster
    // so which?
    // you cant pop the same selfp twice. it only makes sense for push pop push
    // pop. not push push pop pop.
    // better go for faast append
}

monostatic void PtrList_shift(PtrList** selfp, void* item) {
    *selfp = PtrList_withNext(item, *selfp);
}

monostatic void* PtrList_unshift(PtrList** selfp) {
    void* item = (*selfp)->item;
    // drop(*selfp);
    *selfp = (*selfp)->next;
    return item;
}

monostatic void* PtrList_pop(PtrList** selfp) {
    while (*selfp && (*selfp)->next) selfp = &(*selfp)->next;
    void* ret = (*selfp)->item;
    // drop(*selfp);
    *selfp = NULL;
    return ret;
}

// FIXME: it seems like foreachn will stop at the first NULL item in the list.
//        This will not allow you to have NULL objects in a list.
#define foreach(T, var, listSrc) foreachn(T, var, _listp_, listSrc)
#define foreachn(T, var, listp, listSrc)                                       \
    for (PtrList* listp = listSrc; listp; listp = NULL)                        \
        for (T var = (T)listp->item; listp && (var = (T)listp->item);          \
             listp = listp->next)