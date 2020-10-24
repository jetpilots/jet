inline size_t IsPowerOf2(size_t num) { return ((num - 1) & num) == 0; }

// Round to the next multiple of powerof2
inline size_t Ceil(size_t num, size_t powerof2) {
    assert(IsPowerOf2(powerof2));
    return (num + powerof2 - 1) & -(intptr_t)powerof2;
}

bool NeedResize1(size_t length, size_t added_len, size_t chunk_size) {
    size_t capacity = Ceil(length, chunk_size);
    return length + added_len > capacity;
}

ITEM* AddItems(LIST* list, size_t added_len) {
    const size_t chunk_size = 1024;
    if (NeedResize1(list->length, added_len, chunk_size)) {
        ITEM* new_items = (ITEM*)realloc(list->items,
            Ceil(list->length + added_len, chunk_size) * sizeof(ITEM));
        if (!new_items) return NULL;
        list->items = new_items;
    }
    ITEM* items = list->items + list->length;
    list->length += added_len;
    return items;
}
