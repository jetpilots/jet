#include <stdio.h>
#include <stdlib.h>

typedef struct DList {
  int refc;
  char* name;
  struct DList *prev, *next;
} DList;

void drop(DList* dl) {
  if (!dl->refc--) {
    // dl->refc--; // printf("drop %s\n", dl->name);
    if (dl->prev) drop(dl->prev);
    if (dl->next) drop(dl->next);
    free(dl);
    printf("dropped %s\n", dl->name);
  } else // if (dl->refc)
    printf("no drop %s: has %d refs\n", dl->name, dl->refc);
}

void drjop(DList* dl) {
  if (dl->refc--) return;
  if (dl->prev) drop(dl->prev);
  if (dl->next) drop(dl->next);
  free(dl);
}

#define var(name, type) type* name = malloc(sizeof(type))

int main() {
  var(a, DList);
  *a = (DList) { .name = "a", .refc = 0 };
  var(b, DList);
  *b = (DList) { .name = "b", .refc = 0 };
  a->next = b, b->prev = a;
  drop(a);
}