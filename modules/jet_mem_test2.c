#define JET_MEM_NOMEMPOOL
#include "jet_mem.h"

double* func()
{
    double* t = malloc(1000 * sizeof(double));
    free(t);
    return NULL;
}