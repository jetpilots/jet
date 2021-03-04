#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "../modules/base.h"
// this is also a sparse ND-tensor impleentation if you set the key type as Int
// and value type as double etc.
#define for_to(i, n) for (int(i) = 0; (i) < (n); (i)++)

#define POW2(n) (1 << n)
#define MIN(a, b) (a) < (b) ? (a) : (b)

#define calloc(a, b)                                                           \
    calloc(a, b);                                                              \
    __totCallocB += (a) * (b);                                                 \
    __totCalloc++;
#define malloc(b)                                                              \
    malloc(b);                                                                 \
    __totMallocB += (b);                                                       \
    __totMalloc++;

#define DIMS 3
static size_t __totCallocB, __totMallocB;
static int __totCalloc, __totMalloc;
const int POINTS_PER_LEAF = 24; // MIN(POW2(DIMS), 8);

typedef struct KDDictNode {
    int isleaf; // GET THIS OUT HOWEVER
    uint64_t threshold[DIMS];
    struct KDDictNode* child[POW2(DIMS)];
} KDDictNode;

typedef struct KDDictLeaf {
    int isleaf, npoints; // GET THIS OUT HOWEVER
    uint64_t hash[POINTS_PER_LEAF][DIMS];
    void* keys[POINTS_PER_LEAF][DIMS];
    void* value[POINTS_PER_LEAF];

} KDDictLeaf;

typedef struct KDDict {
    KDDictNode root[2];
    int count;
    uint64_t (*hash)(void*);
    uint64_t (*equal)(void*, void*);
} KDDict;

MKSTAT(KDDictNode);
MKSTAT(KDDictLeaf);
MKSTAT(KDDict);

#define UPDATE_DIR(dir, dirInDim)                                              \
    dir = 0;                                                                   \
    for_to(i, DIMS) {                                                          \
        dirInDim[i] = (hash[i] >= node->threshold[i]) << i;                    \
        dir |= dirInDim[i];                                                    \
    }

static void addEntry(KDDict* dict, void* keys[DIMS], void* value) {
    KDDictNode* node = dict->root; // the first, so skipping &[0]
    int dirInDim[DIMS], dir;
    uint64_t hash[DIMS];
    for_to(i, DIMS) hash[i] = dict->hash(keys[i]);

    UPDATE_DIR(dir, dirInDim)

    KDDictNode* parent = NULL;
    while (node->child[dir] && !node->child[dir]->isleaf) {
        parent = node;
        node = node->child[dir];
        UPDATE_DIR(dir, dirInDim)
    }

    // now the child is either NULL or a leaf (i.e. has points)
    if (!node->child[dir]) {
        node->child[dir] = NEW(KDDictLeaf);
        node->child[dir]->isleaf = 1;
    }

    // now the child must be a leaf
    KDDictLeaf* child = node->child[dir];

    if (child->npoints == POINTS_PER_LEAF) {
        KDDictLeaf tmp = *child;
        // memcpy(points, child->points, sizeof(KDTreePoint*) *
        // POINTS_PER_LEAF);

        KDDictNode* newChild = NEW(KDDictNode);
        node->child[dir] = newChild;

        *child = (KDDictLeaf) {};
        newChild->child[dir] = child;
        newChild->child[dir]->isleaf = 1;

        for_to(i, DIMS) {
            uint64_t th_mid = node->threshold[i];
            uint64_t th_lo, th_hi;
            if (node->threshold[i] >= parent->threshold[i]) {
                th_lo = parent->threshold[i];
                th_hi = 2 * node->threshold[i] - parent->threshold[i];
            } else {
                th_lo = 2 * node->threshold[i] - parent->threshold[i];
                th_hi = parent->threshold[i];
            }

            if (!dirInDim[i])
                newChild->threshold[i] = (th_mid + th_lo) / 2;
            else
                newChild->threshold[i] = (th_hi + th_mid) / 2;
        }

        // now that the thresholds are set, add the cached points
        for_to(ip, POINTS_PER_LEAF) addEntry(dict, tmp.keys[ip], tmp.value[ip]);

        addEntry(dict, keys, value); // need 1 level to compute the new
        //        centroids in case all 8 points from previous clustered into
        //        the same grandchild
    }

    // for (int i = 0; i < POINTS_PER_LEAF; i++) {
    //     if (!child->points[i]) {
    //         child->points[i] = point;
    //         child->npoints++;
    //         break;
    //     }
    // }

    for_to(i, child->npoints) {
        for_to(j, DIMS) //
            if (child->hash[i][j] != hash[j]) goto skip;
        for_to(j, DIMS) //
            if (!dict->equal(child->keys[i][j], keys[j])) goto skip;

        child->value[i] = value;
        return;
    skip:;
    }
    // didn't find a matching record already, so add a new one
    child->value[child->npoints] = value;
    for_to(j, DIMS) {
        child->hash[child->npoints][j] = hash[j];
        child->keys[child->npoints][j] = keys[j];
    }
    child->npoints++;
    dict->count++;
    // return NULL;
}

// static void removePoint(KDDictNode* node, KDTreePoint* point) { }

static void* lookup(KDDict* dict, void* keys[DIMS]) {
    KDDictNode* node = dict->root;
    uint64_t hash[DIMS];
    for_to(i, DIMS) hash[i] = dict->hash(keys[i]);

    // here as opposed to add, you descend all the way down to the leaf.
    while (node && !node->isleaf) {
        int dir, dirInDim[DIMS];
        UPDATE_DIR(dir, dirInDim)
        node = node->child[dir];
    }
    // actually you shouldn't expect to find a NULL leaf here.
    if (!node) return NULL;

    KDDictLeaf* holder = node;
    for_to(i, holder->npoints) {
        for_to(j, DIMS) if (holder->hash[i][j] != hash[j]) goto skip;
        for_to(j, DIMS) //
            if (!dict->equal(holder->keys[i][j], keys[j])) goto skip;

        return holder->value[i];
    skip:;
    }
    return NULL;
}

#define countof(x) (sizeof(x) / sizeof(x[0]))
static const char* const spc = "                                            ";
static const int levStep = 2;

static void printDotRec(FILE* f, KDDictNode* node) {
    for_to(i, POW2(DIMS)) {
        int dirInDim[DIMS];
        for_to(j, DIMS) dirInDim[j] = i & (1 << j);

        if (node->child[i]) {
            if (node->child[i]->isleaf) {
                KDDictLeaf* holder = node->child[i];

                fprintf(f,
                    "\"Node\\n<%llu, %llu, %llu>\" -> "
                    "\"Points[%d]\\n____________\\n",
                    node->threshold[0], node->threshold[1], node->threshold[2],
                    holder->npoints);
                for_to(j, holder->npoints) {
                    fprintf(f, "keys: ");
                    for_to(k, DIMS) fprintf(f, "'%s' ", holder->keys[j][k]);
                    fprintf(f, "\nhashes: ");
                    for_to(k, DIMS) fprintf(f, "0x%llx ", holder->hash[j][k]);
                    fprintf(f, "\nvalue: %p\n____________\n", holder->value[j]);
                }
                fprintf(f, "\" [label=\"[%d]\\n", i);
                for_to(j, DIMS) fprintf(f, "%c", dirInDim[j] ? '>' : '<');
                fprintf(f, "\"]\n");
            } else {
                fprintf(f,
                    "\"Node\\n<%llu, %llu, %llu>\" -> \"Node\\n<%llu, %llu, "
                    "%llu>\" "
                    "[label=\"[%d]\\n",
                    node->threshold[0], node->threshold[1], node->threshold[2],
                    node->child[i]->threshold[0], node->child[i]->threshold[1],
                    node->child[i]->threshold[2], i);
                for_to(j, DIMS) fprintf(f, "%c", dirInDim[j] ? '>' : '<');
                fprintf(f, "\"]\n");

                printDotRec(f, node->child[i]);
            }
        }
    }
}

static void printDot(KDDict* dict) {
    KDDictNode* node = dict->root;
    FILE* f = fopen("kdd.dot", "w");
    fputs(
        "digraph {\nnode [fontname=\"Miriam Libre\"]; edge [fontname=\"Miriam "
        "Libre\"];\n",
        f);
    printDotRec(f, node);
    fputs("}\n", f);
    fclose(f);
}

#include "../modules/clock.h"
// When you know the hash bounds, use this function to init. This is the case
// when you are creating a sparse matrix with known shape and the hash function
// for ints is a no-op. By the way in that case you don't need to compare keys
// at all -- lookup should succeed as soon as hashes match.
static KDDict* initDictWithLimits(uint64_t hashfn(void*),
    uint64_t equalfn(void*, void*), uint64_t lowLim[DIMS],
    uint64_t highLim[DIMS]) {
    KDDict* dict = NEW(KDDict);
    for_to(j, DIMS) {
        dict->root[0].threshold[j] = lowLim[j];
        dict->root[1].threshold[j] = (lowLim[j] + highLim[j]) / 2;
    }
    dict->root[0].child[POW2(DIMS) - 1] = &dict->root[1];
    // here's the key: boundBox[0].x[i] MUST BE < root.threshold[i] for all i
    dict->hash = hashfn;
    dict->equal = equalfn;

    return dict;
}
static KDDict* initDict(
    uint64_t hashfn(void*), uint64_t equalfn(void*, void*)) {
    uint64_t lowl[DIMS] = {};
    uint64_t highl[DIMS] = { UINT64_MAX, UINT64_MAX, UINT64_MAX };
    return initDictWithLimits(hashfn, equalfn, lowl, highl);
}
/*
fashhash64 (license: MIT)
Copyright (C) 2012 Zilong Tan (eric.zltan@gmail.com)
https://github.com/ztanml/fast-hash
*/

#define mix(h)                                                                 \
    ({                                                                         \
        (h) ^= (h) >> 23;                                                      \
        (h) *= 0x2127599bf4325c37ULL;                                          \
        (h) ^= (h) >> 47;                                                      \
    })

static uint64_t fasthash64(const void* buf, size_t len, uint64_t seed) {
    const uint64_t m = 0x880355f21e6d1965ULL;
    const uint64_t* pos = (const uint64_t*)buf;
    const uint64_t* end = pos + (len / 8);
    const unsigned char* pos2;
    uint64_t h = seed ^ (len * m);
    uint64_t v;

    while (pos != end) {
        v = *pos++;
        h ^= mix(v);
        h *= m;
    }

    pos2 = (const unsigned char*)pos;
    v = 0;

    switch (len & 7) {
    case 7:
        v ^= (uint64_t)pos2[6] << 48;
    case 6:
        v ^= (uint64_t)pos2[5] << 40;
    case 5:
        v ^= (uint64_t)pos2[4] << 32;
    case 4:
        v ^= (uint64_t)pos2[3] << 24;
    case 3:
        v ^= (uint64_t)pos2[2] << 16;
    case 2:
        v ^= (uint64_t)pos2[1] << 8;
    case 1:
        v ^= (uint64_t)pos2[0];
        h ^= mix(v);
        h *= m;
    }

    return mix(h);
}

static uint32_t fasthash32(const void* buf, size_t len, uint32_t seed) {
    uint64_t h = fasthash64(buf, len, seed);
    return h - (h >> 32);
}
#undef mix

uint64_t strhash(char* str) {
    size_t l = strlen(str);
    return fasthash64(str, l, 31337);
}

uint64_t streq(char* str1, char* str2) {
    return str1 == str2 || !strcmp(str1, str2);
    // in C, you can't have strings of different lengths at the same address,
    // since they are null-terminated. So it should suffice to compare their
    // addresses first.
}

int main(int argc, char* argv[]) {
    srand(time(0));

    // const int NPTS = argc > 1 ? atoi(argv[1]) : 10000000;
    // clock_Time t0 = clock_getTime();
    KDDict* dict = initDict(strhash, streq);
    // printDot(root);
    // return 0;
    // printf("init: %g ms\n", clock_clockSpanMicro(t0) / 1e3);

    // KDTreePoint* pt = malloc(sizeof(KDTreePoint) * NPTS);
    // t0 = clock_getTime();
    // // for_to(i, NPTS) pt[i] = (KDTreePoint) { { rndf(), rndf(), rndf() } };
    // // printf("%d rndgen+mallocs etc: %g ms\n", NPTS,
    //     clock_clockSpanMicro(t0) / 1e3);
    // t0 = clock_getTime();

    // TODO: preallocate log2(NPTS/POINTS_PER_LEAF) nodes, assuming
    // uniformly distributed points that should be OK
    // for_to(i, NPTS) addPoint(root, pt + i);
    // printf("add %d pts: %g ms\n", NPTS, clock_clockSpanMicro(t0) /
    // 1e3);
    double dx[][1] = { 0.1234567, 9.345345, 2.123123, 6.3452395, 7.39486 };
    // printNode(root, 0);
    // if (NPTS < 1000)
    const char* keys[][3] = { //
        { "asdjhakj", "", "" }, //
        { "fgsdbd", "", "" }, //
        { "esrtcert", "", "" }, //
        { "tytuhbfyy", "", "" }, //
        { "xgdvrg", "", "" }
    };
    const char* keysno[3] = { "bhabru", "", "" };

    for_to(i, 5) addEntry(dict, keys[i], dx[i]);
    printDot(dict);
    // KDTreePoint ptest[] = { { -3.14159 } };
    // t0 = clock_getTime();
    const char** sought = keysno; //[3];
    double* nea = lookup(dict, sought);
    // printf("lookup: %g us\n", clock_clockSpanNano(t0) / 1e3);

    if (nea)
        printf("found: %f at %p for '%s'\n", *nea, nea, keys[3][0]);
    else {
        printf("no match for: '%s'\n", sought[0]);
        // printPoint(ptest, 0);
    }
    printf("total calloc %zu B (%d), malloc %zu B (%d), pool %u B\n",
        __totCallocB, __totCalloc, __totMallocB, __totMalloc, gPool->usedTotal);
}