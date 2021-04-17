#define List(T) ptrlist_t
typedef struct ptrlist_t ptrlist_t;
struct ptrlist_t {
    void* item;
    ptrlist_t* next;
};

#define NEWW(T, ...) memcpy(NEW(T), &(T) { __VA_ARGS__ }, sizeof(T))

jet_static ptrlist_t* ptrlist_withNext(void* item, void* next) {
    // TODO: how to get separate alloc counts of List_ASTType
    // List_ASTFunc etc.?
    ptrlist_t* li = NEW(ptrlist_t);
    li->item = item;
    li->next = next;
    return li;
}
jet_static ptrlist_t* ptrlist_with(void* item) {
    return ptrlist_withNext(item, NULL);
}

jet_static int ptrlist_count(ptrlist_t* listPtr) {
    int i;
    for (i = 0; listPtr; i++) listPtr = listPtr->next;
    return i;
}

// returns the a ref to the last listitem so you can use that for
// repeated appends in O(1) and not O(N)
jet_static ptrlist_t** ptrlist_append(ptrlist_t** selfp, void* item) {
    while (*selfp) selfp = &(*selfp)->next;
    *selfp = ptrlist_with(item);
    return &(*selfp)->next;
    // if you return selfp, you can do a fast pop or just access the last item
    // if you return &(*selfp)->next you can do the next append even faster
    // so which?
    // you cant pop the same selfp twice. it only makes sense for push pop push
    // pop. not push push pop pop.
    // better go for faast append
}

jet_static void ptrlist_shift(ptrlist_t** selfp, void* item) {
    *selfp = ptrlist_withNext(item, *selfp);
}

jet_static void* ptrlist_unshift(ptrlist_t** selfp) {
    void* item = (*selfp)->item;
    // drop(*selfp);
    *selfp = (*selfp)->next;
    return item;
}

jet_static void* ptrlist_pop(ptrlist_t** selfp) {
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
    for (ptrlist_t* listp = listSrc; listp; listp = NULL)                      \
        for (T var = (T)listp->item; listp && (var = (T)listp->item);          \
             listp = listp->next)