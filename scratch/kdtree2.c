#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "../modules/jet_base.h"

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

typedef struct KDTreeNode {
    int isleaf; // GET THIS OUT HOWEVER
    double threshold[DIMS]; // [D] // leaves dont hav thresholds!
    // union {
    struct KDTreeNode* child[POW2(DIMS)]; // [2^D]
    // struct KDTreePoint* points[POINTS_PER_LEAF];
    // };
} KDTreeNode;

typedef struct KDTreePointsHolder {
    int isleaf, npoints; // GET THIS OUT HOWEVER
    struct KDTreePoint* points[POINTS_PER_LEAF];
} KDTreePointsHolder;

typedef struct KDTreePoint {
    double x[DIMS]; // [D]
} KDTreePoint;

MKSTAT(KDTreeNode);
MKSTAT(KDTreePoint);
MKSTAT(KDTreePointsHolder);

static double squared(double x) { return x * x; }
static double distanceFunction(KDTreePoint* p, KDTreePoint* p2) {
    double sum = 0;
    for_to(i, DIMS) sum += squared(p->x[i] - p2->x[i]);
    return sum;
}

// #define HAS_DIR(dir, which) dir << which
static void addPoint(KDTreeNode* node, KDTreePoint* point) {
    int directionInDim[DIMS], direction = 0;
    for_to(i, DIMS) directionInDim[i] = //
        (point->x[i] >= node->threshold[i]) << i;
    // directionInDim[i] tells you whether the ith-coordinate of the point is
    // below or above (or if you prefer, to the left or right of) the
    // ith-coordinate of the node's threshold. You need to check all dimensions
    // and find out how the comparison goes (individually). In a simple binary
    // tree this is like deciding whether the new value should go to the left or
    // to the right child, but to avoid branching here it goes to child[0] or
    // child[1], and the 0 or 1 comes from the comparison result. node->child is
    // actually effectively node->child[][][] ... i.e. as many []s as the
    // dimensionality. However writing it explicitly as a multi-dim array makes
    // things difficult for a truly generic version, so I'm going to write it as
    // a normal array and calculate the overall (flat) index. this N-dimensional
    // access of the form arr[l1][l2][l3] is effectively l1*4 + l2*2 + l3 ...
    // and so on for higher dimensions, where l's are either 0 or 1. so it can
    // be reduced to bitwise ops: the *s can be done with << and adding them
    // together is a |.
    for_to(i, DIMS) direction
        |= directionInDim[i]; // JUST PUT THIS IN THE ABOVE LOOP
    // direction is now the overall index of the "correct" child to follow.

    // now 3 things can happen: (1) the child doesn't exist, (2) it does
    // and is a KDTreeNode, (3) it does and is a leaf (it has KDTreePoint[]).
    // Descend until just before you hit a leaf or NULL.
    KDTreeNode* parent = NULL;
    while (node->child[direction] && !node->child[direction]->isleaf) {
        parent = node;
        node = node->child[direction];
        // direction = (point->x >= node->threshold);
        direction = 0;
        for_to(i, DIMS) directionInDim[i] = (point->x[i] >= node->threshold[i])
            << i;
        for_to(i, DIMS) direction |= directionInDim[i];
    }

    // now the child is either NULL or a leaf (i.e. has points)
    if (!node->child[direction]) {
        node->child[direction] = jet_new(KDTreePointsHolder);
        // calloc(1, sizeof(KDTreePointsHolder));
        node->child[direction]->isleaf = 1;
    }

    // now the child must be a leaf
    KDTreePointsHolder* child = node->child[direction];

    if (child->npoints == POINTS_PER_LEAF) {
        // the array of points in this leaf is full. the leaf should now be
        // converted into a node and the POINTS_PER_LEAF points of this ex-leaf
        // should be re-added into this node so so they can go one level deeper
        // in the tree at the right spots.
        // KDTreePointsHolder* tmpPoints = child;
        KDTreePoint* points[POINTS_PER_LEAF];
        memcpy(points, child->points, sizeof(KDTreePoint*) * POINTS_PER_LEAF);

        //        child->isleaf = 0; // no longer a leaf.
        //        child->npoints = 0; // don't need it here, its not a leaf
        //        bzero(child->points, sizeof(KDTreePoint*) * POINTS_PER_LEAF);
        // *child = (KDTreeNode) {};
        KDTreeNode* newChild
            = jet_new(KDTreeNode); // calloc(1, sizeof(KDTreeNode));
        node->child[direction] = newChild;
        // NOT A LEAF! A NEW NODE
        // wipe the points, the space will be used
        // for child node ptrs now

        // and you know that there are POINTS_PER_LEAF points anyway.

        // you have to add 2 grandchildren here (actually 2^NDIMS). Don't leave
        // them at NULL. Set them as leaves.
        // TODO: figure out how to only create child->child[direction] and not
        // all 2^DIMS children here
        // for (
        // int i = direction;// i == direction /*POW2(DIMS)*/; i++) {
        // for (int i = 0; i < POW2(DIMS); i++) {
        // THIS BUSINESS IS TO AVOID FREEING THE OLD POINTSHOLDER SO IT CAN
        // BE REUSED
        *child = (KDTreePointsHolder) {};
        newChild->child[direction] = child;
        // calloc(1, sizeof(KDTreeNode));
        newChild->child[direction]->isleaf = 1; // already set
        // }

        for_to(i, DIMS) { // you need the midpoints of the new grandkids.
            // they depend on the midpoint of the child, and the midpoint of the
            // parent (i.e. two levels up the ancestry). For these you first
            // need 3 (!) "limits", the lo/mid/hi limits.
            double th_mid = node->threshold[i];
            double th_lo, th_hi;
            if (node->threshold[i] >= parent->threshold[i]) {
                // child is increasing, i.e. going to the right
                // Lower bound is the parent's threshold.
                th_lo = parent->threshold[i];
                th_hi = 2 * node->threshold[i] - parent->threshold[i];
            } else {
                // child is decreasing, i.e. going to the left.
                // Higher bound is the parent's threshold.
                th_lo = 2 * node->threshold[i] - parent->threshold[i];
                th_hi = parent->threshold[i];
            }

            if (!directionInDim[i])
                newChild->threshold[i] = (th_mid + th_lo) / 2;
            else
                newChild->threshold[i] = (th_hi + th_mid) / 2;
        }

        // Now you see if the grandkid is increasing or decreasing compared to
        // the child (the grandkid's parent). Again you have to do this for all
        // grandchildren, regardless of whether they have any points (yet).
        //        for (int gdirection = 0; gdirection < 2; gdirection++)
        //            // for (int i=0;i<2^NDIMS;i++) for (int j=0;j<NDIMS;j++)
        //            gdird[j] =
        //            // i & 1<<j; gdirection |= ...
        //            if (gdirection)
        //                child->child[gdirection]->threshold = (th_hi - th_mid)
        //                / 2;
        //            else
        //                child->child[gdirection]->threshold = (th_mid - th_lo)
        //                / 2;

        // now that the thresholds are set, add the cached points
        for (int ip = 0; ip < POINTS_PER_LEAF; ip++)
            addPoint(newChild, points[ip]);

        addPoint(node, point); // need 1 level to compute the new
        //        centroids in case all 8 points from previous clustered into
        //        the same grandchild
        return;
        // parent = node;
        // node = child;
        // child = child->child[direction];
        // direction = 0;
        // for_to(i, DIMS) directionInDim[i] = (point->x[i] >=
        // node->threshold[i]) << i; for_to(i, DIMS) direction |=
        // directionInDim[i];

        //        if (child->npoints==8)
        //        return;
    } // TODO: what to do if npoints was somehow set to > POINTS_PER_LEAF?
    // else
    // NOW finally add the point.
    // you have a static array of POINTS_PER_LEAF items, but deleting leaves
    // holes. So add will have to loop and find an open hole to plug the new
    // point in. its probably better than rearranging the array on delete,
    // because when POINTS_PER_LEAF is small the array is in cache anyway,
    // but writing to it will access DRAM.
    // child->points[child->npoints++] = point;

    for (int i = 0; i < POINTS_PER_LEAF; i++) {
        if (!child->points[i]) {
            child->points[i] = point;
            child->npoints++;
            break;
        }
    }
}

static void removePoint(KDTreeNode* node, KDTreePoint* point) { }

// always init  with this func with a known number of levels and a bounding box
// this way you have a node hierarchy ready and your add calls will not thrash
// adding subnodes and moving points down over and over.
// KDTreeNode* init(int levels, KDTreePoint* boundBox[2]) { }

// BTW for jet do you want to allow function xyz(arr[2]) etc. and then check
// at callsites if passed array has exactly 2 (or 2 or more) provably?

static KDTreePoint* getNearestPoint(KDTreeNode* node, KDTreePoint* point) {
    // TODO: PROBLEM!!! HERE you descend into the finest octant/quadrnt and
    // check only those points. The actual nearest point may be in an adjacent
    // quadrant! So you should go up 1 level than the finest, or 2?

    // here as opposed to add, you descend all the way down to the leaf.
    while (node && !node->isleaf) {
        int direction = 0, directionInDim[DIMS];
        for_to(i, DIMS) directionInDim[i] = (point->x[i] >= node->threshold[i])
            << i;
        for_to(i, DIMS) direction |= directionInDim[i];
        node = node->child[direction];
    }
    // actually you shouldn't expect to find a NULL leaf here.
    int minIndex = 0;
    double mindist = 1e300;
    if (!node) return NULL;
    KDTreePointsHolder* holder = node;
    for (int i = 0; i < holder->npoints; i++) {
        double dist = distanceFunction(holder->points[i], point);
        if (dist < mindist) {
            minIndex = i;
            mindist = dist;
        }
    }
    // a point is always returned. you asked for the near*est* point, and there
    // is always one, regardless of how near or far it is exactly.
    // THATS NOT TRUE!!!!!
    // MAYBE: pass mindist back to the caller to save a repeated
    // distanceFunction call.
    return holder->points[minIndex];
}
// static KDTreePoint points[] = { { 1 }, { 3 }, { -5 } };

#define countof(x) (sizeof(x) / sizeof(x[0]))
static const char* const spc = "                                            ";
static const int levStep = 2;
static void printPoint(KDTreePoint* p, int lev) {
    printf("%.*s(", lev, spc);
    for_to(i, DIMS) { printf("%s%g", i ? ", " : "", p->x[i]); }
    puts(")");
}

static void printNode(KDTreeNode* node, int lev) {
    if (node->isleaf) {
        printf("[\n"); //,node->npoints );
        KDTreePointsHolder* holder = node;

        for_to(j, holder->npoints) printPoint(holder->points[j], lev + levStep);

        printf("%.*s]\n", lev, spc);
    } else {
        printf("%.*s<", 0 * lev, spc);
        for_to(i, DIMS) printf("%s%g", i ? ", " : "", node->threshold[i]);
        printf("> {\n");
        for_to(i, POW2(DIMS)) {
            if (node->child[i]) {
                printf("%.*s%d: ", lev + levStep, spc, i);
                printNode(node->child[i], lev + levStep);
            } else {
                // printf("%.*schild[%d] NULL\n", lev, spc,i);
            }
        }
        printf("%.*s}\n", lev, spc);
    }
}

static void printDotRec(FILE* f, KDTreeNode* node) {
    for_to(i, POW2(DIMS)) {
        int directionInDim[DIMS];
        for_to(j, DIMS) directionInDim[j] = i & (1 << j);

        if (node->child[i]) {
            if (node->child[i]->isleaf) {
                KDTreePointsHolder* holder = node->child[i];

                fprintf(f,
                    "\"Node\\n<%g, %g, %g>\" -> \"Points[%d]\\n____________\\n",
                    node->threshold[0], node->threshold[1], node->threshold[2],
                    holder->npoints);
                for_to(j, holder->npoints) {
                    KDTreePoint* point = holder->points[j];
                    fprintf(f, "(%g, %g, %g)\\n", point->x[0], point->x[1],
                        point->x[2]);
                }
                fprintf(f, "\" [label=\"[%d]\\n", i);
                for_to(j, DIMS) fprintf(f, "%c", directionInDim[j] ? '>' : '<');
                fprintf(f, "\"]\n");
            } else {
                fprintf(f,
                    "\"Node\\n<%g, %g, %g>\" -> \"Node\\n<%g, %g, %g>\" "
                    "[label=\"[%d]\\n",
                    node->threshold[0], node->threshold[1], node->threshold[2],
                    node->child[i]->threshold[0], node->child[i]->threshold[1],
                    node->child[i]->threshold[2], i);
                for_to(j, DIMS) fprintf(f, "%c", directionInDim[j] ? '>' : '<');
                fprintf(f, "\"]\n");

                printDotRec(f, node->child[i]);
            }
        }
    }
}

static void printDot(KDTreeNode* node) {
    FILE* f = fopen("kdt.dot", "w");
    fputs(
        "digraph {\nnode [fontname=\"Miriam Libre\"]; edge [fontname=\"Miriam "
        "Libre\"];\n",
        f);
    printDotRec(f, node);
    fputs("}\n", f);
    fclose(f);
}

static KDTreePoint g_Points[] = { { -5 }, { -4 }, { -3 }, { -2 }, { 6 },
    { -5.35 }, { -4.35 }, { -3.35 }, { -2.4545 }, { 1.45 }, { -5.3578 },
    { -4.6789 }, { -3.8679 }, { -2.5557 }, { -1.6767 }, { -5.10354225 },
    { -4.035 }, { -3.3335 }, { -2.45 }, { -1.04455 } };

static KDTreePoint gPoints[] = {
    { -0, -2, 3 }, //
    { -0, 1, 4 }, //
    { -0, 2, -4 }, //
    { -0, 5, -2 }, //
    { -1, -2, -3 }, //
    { -1, -3, 4 }, //
    { -1, -5, -2 }, //
    { -1, 3, 3.5 }, //
    { -1, 3, 3.9 }, //
    { -1, 3, 3.6 }, //
    { -1, 3, 4.18 }, //
    { -1, 3, 4 }, //
    { -1, 3, 4.3 }, //
    { -1, 3, 3 }, //
    { -1, 3, 4.27 }, //
    { -1, 3, 4 }, //
    { -2, -1, -3 }, //
    { -2, -3, -4 }, //
    { -2, -3, 1 }, //
    { -2, 1, 0 }, //
    { -2, 2, -3 }, //
    { -2, 3, -2 }, //
    { -3, -1, -4 }, //
    { -3, -1, 4 }, //
    { -3, -2, 1 }, //
    { -3, -3, 3 }, //
    { -3, -4, 3 }, //
    { -3, 2, -2 }, //
    { -3, 2, -4 }, //
    { -3, 3, 0 }, //
    { -3, 4, -4 }, //
    { -3, 5, -5 }, //
    { -3, 5, 4 }, //
    { -4, -1, 1 }, //
    { -4, -3, -0 }, //
    { -4, -3, 2 }, //
    { -4, -3, 5 }, //
    { -4, -4, -0 }, //
    { -4, -5, 4 }, //
    { -4, 0, -4 }, //
    { -4, 1, -5 }, //
    { -4, 4, 0 }, //
    { -4, 5, -3 }, //
    { -5, -0, -4 }, //
    { 0, -4, -2 }, //
    { 0, 0, 0 }, //
    { 0, 2, -4 }, //
    { 0, 3, -2 }, //
    { 0, 3, 1 }, //
    { 0, 5, -2 }, //
    { 1, -0, 1 }, //
    { 1, -1, 2 }, //
    { 1, -2, 3 }, //
    { 1, -3, -3 }, //
    { 1, -3, 1 }, //
    { 1, -3, 2 }, //
    { 1, -5, -1 }, //
    { 1, 2, -2 }, //
    { 1, 3, -3 }, //
    { 1, 3, -4 }, //
    { 2, -0, -0 }, //
    { 2, -0, -3 }, //
    { 2, -1, 2 }, //
    { 2, -1, 3 }, //
    { 2, -2, 2 }, //
    { 2, -3, 4 }, //
    { 2, -4, 3 }, //
    { 2, 0, -5 }, //
    { 2, 1, -5 }, //
    { 2, 2, -2 }, //
    { 2, 2, 1 }, //
    { 2, 2, 5 }, //
    { 2, 3, 4 }, //
    { 2, 4, 5 }, //
    { 3, -1, 3 }, //
    { 3, -2, -2 }, //
    { 3, -2, -3 }, //
    { 3, 0, 3 }, //
    { 3, 1, 3 }, //
    { 3, 2, -4 }, //
    { 3, 4, -3 }, //
    { 3, 4, -4 }, //
    { 3, 5, 4 }, //
    { 4, -0, -3 }, //
    { 4, -1, 2 }, //
    { 4, -1, 4 }, //
    { 4, -2, -4 }, //
    { 4, -2, 0 }, //
    { 4, 1, -1 }, //
    { 4, 1, 2 }, //
    { 4, 2, -4 }, //
    { 4, 3, 5 }, //
    { 4, 4, 4 }, //
    { 5, -4, 0 }, //
    { 5, -4, 4 }, //
    { 5, 1, 2 }, //
    { 5, 1, 4 }, //
    { 5, 4, -4 } //
};

static KDTreePoint* linsearch(
    KDTreePoint points[], int npoints, KDTreePoint* point) {
    int minIndex = 0;
    double mindist = 1e300;
    for_to(i, npoints) {
        double dist = distanceFunction(points + i, point);
        if (dist < mindist) {
            minIndex = i;
            mindist = dist;
        }
    }
    return points + minIndex;
}
// random in the range of -6 to 6
static double rndf() {
    return ((int)(rand() / (0.0012 * RAND_MAX))) / 100.0 - 6;
}
static KDTreePoint* newPoint(double x, double y, double z) {
    KDTreePoint* p = malloc(sizeof(KDTreePoint));
    *p = (KDTreePoint) { { x, y, z } };
    return p;
}
#include "../modules/jet_clock.h"

// addLevels can only be called on a node which has 1 further level below it
// already. This is because you need 1 node above the current level in order to
// compute the threshold for the new nodes to be added at a new level.
static void addLevels(KDTreeNode* parent, int levels) {
    for_to(k, POW2(DIMS)) {
        KDTreeNode* root = parent->child[k];
        for_to(i, POW2(DIMS)) if (!root->child[i]) {
            root->child[i] = calloc(1, sizeof(KDTreeNode));
            KDTreeNode* child = root->child[i];

            int directionInDim[DIMS];
            for_to(j, DIMS) directionInDim[j] = i & (1 << j);
            for_to(j, DIMS) {
                // KDTreeNode* parent = parentL;
                double th_mid = root->threshold[j];
                double th_lo, th_hi;
                if (root->threshold[j] >= parent->threshold[j]) {
                    th_lo = parent->threshold[j];
                    th_hi = 2 * root->threshold[j] - parent->threshold[j];
                } else {
                    th_lo = 2 * root->threshold[j] - parent->threshold[j];
                    th_hi = parent->threshold[j];
                }

                if (!directionInDim[j])
                    child->threshold[j] = (th_mid + th_lo) / 2;
                else
                    child->threshold[j] = (th_hi + th_mid) / 2;
            }
        }
        if (levels) addLevels(root, levels - 1);
    }
}

static KDTreeNode* init(KDTreePoint boundBox[2], int sizeGuess) {
    // create a fake parent (PARENT OF ROOT, TEMPORARY) with threshold at
    // one of the corners of the bounding box.
    // KDTreeNode parentL[] = { {} };
    // for_to(j, DIMS) parentL->threshold[j] = boundBox[0].x[j];
    // KDTreeNode parentR[] = { {} };
    // for_to(j, DIMS) parentR->threshold[j] = boundBox[1].x[j];

    // The actual root node that will be returned.
    KDTreeNode* parent = jet_new(KDTreeNode); // calloc(1, sizeof(KDTreeNode));
    for_to(j, DIMS) parent->threshold[j] = boundBox[0].x[j];

    KDTreeNode* root = jet_new(KDTreeNode); // calloc(1, sizeof(KDTreeNode));
    // int levels = log2(sizeGuess);
    // if (levels < 1) levels = 1;
    for_to(j, DIMS) root->threshold[j]
        = (boundBox[0].x[j] + boundBox[1].x[j]) / 2.0;

    parent->child[7] = root;
    // here's the key: boundBox[0].x[i] MUST BE < root.threshold[i] for all i

    // here's a bad trick: I'm going to set all children of the parent to root.
    // so if you do send in a point for add or get that happens to be outside
    // the bounding box, it won't crash but it will pass on to the midpoint of
    // the BB and from then on it will be placed / looked up as usual.
    // for_to(j, POW2(DIMS)) parent->child[j] = root;

    // Be warned that if you send in lots of points that are outside the BB,
    // they will cluster in the same node. REALLY, make sure you get the BB
    // right.

    return parent;

    for_to(i, POW2(DIMS)) {
        root->child[i] = jet_new(KDTreeNode); // calloc(1, sizeof(KDTreeNode));
        KDTreeNode* child = root->child[i];

        int directionInDim[DIMS];
        for_to(j, DIMS) directionInDim[j] = i & (1 << j);
        for_to(j, DIMS) {
            // KDTreeNode* parent = parentL;
            double th_mid = root->threshold[j];
            double th_lo, th_hi;
            if (root->threshold[j] >= parent->threshold[j]) {
                th_lo = parent->threshold[j];
                th_hi = 2 * root->threshold[j] - parent->threshold[j];
            } else {
                th_lo = 2 * root->threshold[j] - parent->threshold[j];
                th_hi = parent->threshold[j];
            }

            if (!directionInDim[j])
                child->threshold[j] = (th_mid + th_lo) / 2;
            else
                child->threshold[j] = (th_hi + th_mid) / 2;
        }

        // KDTreePoint mid;
        // for_to(j, DIMS) mid[j] = ...;
        // for_to(j, DIMS) root->child[i]->threshold[j] = mid.x[j];
    }
    // if (levels > 1)
    // addLevels(root, 3); // benchmarked; this doesn't help
    return root;
}

int main(int argc, char* argv[]) {
    srand(time(0));
    // KDTreeNode root[] = { { .threshold = { 0 } } };
    // KDTreeNode rc1[] = { { .threshold = { -3, -3, -3 } } };
    // KDTreeNode rc2[] = { { .threshold = { 3, -3, -3 } } };
    // KDTreeNode rc3[] = { { .threshold = { -3, 3, -3 } } };
    // KDTreeNode rc4[] = { { .threshold = { 3, 3, -3 } } };
    // KDTreeNode rc5[] = { { .threshold = { -3, -3, 3 } } };
    // KDTreeNode rc6[] = { { .threshold = { 3, -3, 3 } } };
    // KDTreeNode rc7[] = { { .threshold = { -3, 3, 3 } } };
    // KDTreeNode rc8[] = { { .threshold = { 3, 3, 3 } } };

    // root->child[0] = rc1;
    // root->child[1] = rc2;
    // root->child[2] = rc3;
    // root->child[3] = rc4;
    // root->child[4] = rc5;
    // root->child[5] = rc6;
    // root->child[6] = rc7;
    // root->child[7] = rc8;

    KDTreePoint boundBox[2] = { { { -6, -6, -6 } }, { { 6, 6, 6 } } };
    // for (int i = 0; i < countof(gPoints); i++) { addPoint(root, gPoints +
    // i);
    // }
    const int NPTS = argc > 1 ? atoi(argv[1]) : 10000000;
    jet_clock_Time t0 = jet_clock_getTime();
    KDTreeNode* root = init(boundBox, NPTS);
    // printDot(root);
    // return 0;
    printf("init: %g ms\n", jet_clock_clockSpanMicro(t0) / 1e3);

    KDTreePoint* pt = malloc(sizeof(KDTreePoint) * NPTS);
    t0 = jet_clock_getTime();
    for_to(i, NPTS) pt[i] = (KDTreePoint) { { rndf(), rndf(), rndf() } };
    printf("%d rndgen+mallocs etc: %g ms\n", NPTS,
        jet_clock_clockSpanMicro(t0) / 1e3);
    t0 = jet_clock_getTime();

    // TODO: preallocate log2(NPTS/POINTS_PER_LEAF) nodes, assuming
    // uniformly distributed points that should be OK
    for_to(i, NPTS) addPoint(root, pt + i);
    printf("add %d pts: %g ms\n", NPTS, jet_clock_clockSpanMicro(t0) / 1e3);

    // printNode(root, 0);
    if (NPTS < 1000) printDot(root);
    KDTreePoint ptest[] = { { -3.14159 } };
    t0 = jet_clock_getTime();
    KDTreePoint* nea = getNearestPoint(root, ptest);
    printf("lookup: %g us\n", jet_clock_clockSpanNano(t0) / 1e3);

    if (nea)
        printPoint(nea, 0);
    else {
        printf("no match for: ");
        printPoint(ptest, 0);
    }
    t0 = jet_clock_getTime();
    printPoint(linsearch(pt, NPTS, ptest), 0);
    printf("linsearch: %g us\n", jet_clock_clockSpanNano(t0) / 1e3);
    printf("total calloc %zu B (%d), malloc %zu B (%d)\n", __totCallocB,
        __totCalloc, __totMallocB, __totMalloc);
}
