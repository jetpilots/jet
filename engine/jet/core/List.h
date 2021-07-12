#define List(T) PtrList
typedef struct PtrList PtrList;
struct PtrList {
  void* item;
  PtrList* next;
};
MKSTAT(PtrList);

monostatic PtrList* PtrList_new_(void) {
  IFDEBUG(_allocTotal_PtrList++);
  return Pool_alloc(gPool, sizeof(struct PtrList));
}

monostatic PtrList* li_with(void* item) {
  // TODO: how to get separate alloc counts of List_ASTType
  // List_ASTFunc etc.?
  PtrList* li = NEW(PtrList);
  li->item = item;
  return li;
}

monostatic PtrList* li_withNext(void* item, void* next) {
  // TODO: how to get separate alloc counts of List_ASTType
  // List_ASTFunc etc.?
  PtrList* li = NEW(PtrList);
  // printf("%d\n", li_allocTotal);
  li->item = item;
  li->next = next;
  return li;
}

monostatic UInt32 li_count(PtrList* listPtr) {
  UInt32 i;
  for (i = 0; listPtr; i++) listPtr = listPtr->next;
  return i;
}

// returns the a ref to the last listitem so you can use that for
// repeated appends in O(1) and not O(N)
monostatic PtrList** li_push(PtrList** selfp, void* item) {
  // if (*selfp == NULL) { // first append call
  //     *selfp = li_with(item);
  //     return selfp;`
  // } else {
  //     PtrList* self = *selfp;
  //     while (self->next) self = self->next;
  //     self->next = li_with(item);
  //     return &(self->next);
  // }

  // while((*selfp)->next) *selfp=(*selfp)->next;

  while (*selfp) selfp = &(*selfp)->next;
  *selfp = li_with(item);
  return &(*selfp)->next;
  // if you return selfp, you can do a fast pop or just access the last item
  // if you return &(*selfp)->next you can do the next append even faster
  // so which?
  // you cant pop the same selfp twice. it only makes sense for push pop
  // push pop. not push push pop pop. better go for faast append
}

monostatic void li_shift(PtrList** selfp, void* item) {
  *selfp = li_withNext(item, *selfp);
}

monostatic void* li_unshift(PtrList** selfp) {
  void* item = (*selfp)->item;
  // drop(*selfp);
  *selfp = (*selfp)->next;
  return item;
}

monostatic void* li_pop(PtrList** selfp) {
  while (*selfp && (*selfp)->next) selfp = &(*selfp)->next;
  void* ret = (*selfp)->item;
  // drop(*selfp);
  *selfp = NULL;
  return ret;
}

// FIXME: it seems like foreachn will stop at the first NULL item in the
// list.
//        This will not allow you to have NULL objects in a list.
#define foreach(T, var, listSrc) foreachn(T, var, _listp_, listSrc)
#define foreachn(T, var, listp, listSrc)                                   \
  for (PtrList* listp = listSrc; listp; listp = NULL)                      \
    for (T var = (T)listp->item; listp && (var = (T)listp->item);          \
         listp = listp->next)
