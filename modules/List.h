
MKSTAT(List)

#define List(T) List
typedef struct List {
    void* value;
    struct List* next;
} List;

typedef struct ListMgr {
    size_t count;
    List *first, //
        *last, //
        *latest; // last inserted item
} ListMgr; // now write wrappers  ListMgr_push() and so on

STATIC void ListMgr_push(ListMgr* mgr, void* value)
{
    List* item = List_with(value);
    if (!mgr->last) { // first push call
        mgr->last = mgr->first = item;
    } else {
        mgr->last->next = item;
    }
    mgr->count++;
}

STATIC void ListMgr_insertBeforeLatest(ListMgr* mgr, void* value)
{
    // mgr->latest = *
    List_insertBeforeItem(&mgr->latest, value);
}

STATIC void ListMgr_insertAfterLatest(ListMgr* mgr, void* value)
{
    //  List* afterPos = mgr->latest->next;
    // // FIXME: what if afterPos is NULL?
    // afterPos = * List_insertBeforeItem(&afterPos, value);
    // mgr->latest = afterPos;
    // TODO: why not oneline?
    List_insertAfterItem(mgr->latest, value);
    // mgr->latest = *List_push(&mgr->latest, value);
}
// Jet
// insertAfterLatest(mgr:ListMgr, value:Ptr) :=
//     insertAfterItem(mgr.latest, value)

// STATIC
static List* List_with(void* value)
{
    // TODO: how to get separate alloc counts of List_ASTType
    // List_ASTFunc etc.?
    List* li = NEW(List);
    li->value = value;
    return li;
}

// STATIC
static List* List_with_next(void* value, void* next)
{
    // TODO: how to get separate alloc counts of List_ASTType
    // List_ASTFunc etc.?
    List* li = NEW(List);
    // printf("%d\n",  List_allocTotal);
    li->value = value;
    li->next = next;
    return li;
}

STATIC int List_count(List* list)
{
    int i;
    for (i = 0; list; list = list->next, ++i) (void)i;
    return i;
}

// returns the a ref to the last listitem so you can use that for
// repeated pushs in O(1) and not O(N)
STATIC List** List_push(List** selfp, void* value)
{
    if (*selfp == NULL) { // first push call
        *selfp = List_with(value);
        return selfp;
    } else {
        List* self = *selfp;
        while (self->next) self = self->next;
        self->next = List_with(value);
        return &(self->next);
    }
}

void List_insertAfterItem(List** selfp, void* value)
{
    if (!*selfp) { // first push call
        *selfp = List_with(value);
        // return selfp;
    } else {
        List_insertBeforeItem(&((*selfp)->next), value);
        // List* self = *selfp;
        // // while (self->next) self = self->next;
        // self->next = List_with(value);
        // return &(self->next);
    }
}

STATIC void List_insertBeforeItem(List** selfp, void* value)
{
    *selfp = List_with_next(value, *selfp);
}

#define FOREACH(T, var, listSrc) FOREACHN(T, var, _listp_, listSrc)
#define FOREACHN(T, var, listp, listSrc)                                       \
    for (List* listp = listSrc; listp; listp = NULL)                           \
        for (T var = (T)listp->value; listp and (var = (T)listp->value);       \
             listp = listp->next)
// #endif
