#include <stdlib.h>
#include <string.h>
#include <stdio.h>

const int POINTS_PER_LEAF = 8;
const int DIMS = 3;

typedef struct Node {
    double threshold; // [D] // leaves dont hav thresholds!
    int npoints, isleaf; // GET THIS OUT HOWEVER
    union {
        struct Node* child[2]; // [2^D]
        struct MPoint* points[POINTS_PER_LEAF];
    };
} Node;

typedef struct MPoint {
    double x, y, z; // [D]
} MPoint;

double squared(double x) { return x * x; }
double distanceFunction(MPoint* p, MPoint* p2)
{
    return squared(p->x - p2->x);
    //+ squared(p->y - p2->y) + squared(p->z - p2->z);
}

void addPoint(Node* node, MPoint* point)
{
    int direction = (point->x >= node->threshold); // [D] -> * (1<<(dim0based))
    Node* parent = NULL;
    // for (int i=0;i<DIMS;i++) direction |= dird[i];

    // TODO: this ND access of the form arr[l1][l2][l3] ... is actually just
    // l1*4 + l2*2 + l3 ... where l's are either 0 or 1. so it can be reduced
    // to bitwise ops << and |. in fact direction can be defined with * (1 <<
    // (dim-1)) so you can write an ND-generic code unlike with [][][] form.

    // now 3 things can happen: (1) the Node doesn't exist, (2) it does and is a
    // Node, (3) it does and is a leaf.

    // Descend until just before you hit a leaf or NULL.
    while (node->child[direction] && !node->child[direction]->isleaf) {
        parent = node;
        node = node->child[direction];
        direction = (point->x >= node->threshold);
    }

    // now the child is either NULL or a leaf (i.e. has points)
    if (!node->child[direction]) {
        node->child[direction] = calloc(1, sizeof(Node));
        node->child[direction]->isleaf = 1;
    }

    // now the child must be a leaf
    Node* child = node->child[direction];

    if (child->npoints == POINTS_PER_LEAF) {
        // the array of points in this leaf is full. the leaf should now be
        // converted into a node and the POINTS_PER_LEAF points of this ex-leaf
        // should be re-added into this node so so they can go one level deeper
        // in the tree at the right spots.
        MPoint* points[POINTS_PER_LEAF] = {};
        memcpy(points, child->points, sizeof(MPoint*) * POINTS_PER_LEAF);
        child->isleaf = 0; // no longer a leaf.
        child->npoints = 0; // don't need it here, its not a leaf
        // and you know that there are POINTS_PER_LEAF points anyway.

        // you have to add 2 grandchildren here (actually 2^NDIMS). Don't leave
        // them at NULL. Set them as leaves.
        for (int i = 0; i < 2; i++) {
            child->child[i] = calloc(1, sizeof(Node));
            child->child[i]->isleaf = 1;
        }

        // you need the midpoints of the new grandkids.
        // they depend on the midpoint of the child, and the midpoint of the
        // parent (i.e. two levels up the ancestry). For these you first need
        // 3 (!) "limits", the lo/mid/hi limits.
        double th_mid = node->threshold;
        double th_lo, th_hi;
        if (node->threshold >= parent->threshold) {
            // child is increasing, i.e. going to the right
            // Lower bound is the parent's threshold.
            th_lo = parent->threshold;
            th_hi = 2 * node->threshold - parent->threshold;
        } else {
            // child is decreasing, i.e. going to the left.
            // Higher bound is the parent's threshold.
            th_lo = 2 * node->threshold - parent->threshold;
            th_hi = parent->threshold;
        }

        if (!direction)
            child->threshold = (th_mid + th_lo) / 2;
        else
            child->threshold = (th_hi + th_mid) / 2;

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
            addPoint(child, points[ip]);
        
        child = child->child[direction];

    } // TODO: what to do if npoints was somehow set to > POINTS_PER_LEAF?

    // NOW finally add the point.
    // you have a static array of POINTS_PER_LEAF items, but deleting leaves
    // holes. So add will have to loop and find an open hole to plug the new
    // point in. its probably better than rearranging the array on delete,
    // because when POINTS_PER_LEAF is small the array is in cache anyway, but
    // writing to it will access DRAM.
    for (int i = 0; i < POINTS_PER_LEAF; i++)
        if (!child->points[i]) {
            child->points[i] = point;
            child->npoints++;
            break;
        }
}

void removePoint(Node* node, MPoint* point) { }

// always init  with this func with a known number of levels and a bounding box
// this way you have a node hierarchy ready and your add calls will not thrash
// adding subnodes and moving points down over and over.
// Node* init(int levels, MPoint* boundBox[2]) { }

// BTW for jet do you want to allow function xyz(arr[2]) etc. and then check
// at callsites if passed array has exactly 2 (or 2 or more) provably?

MPoint* getNearestPoint(Node* node, MPoint* point)
{
    // here as opposed to add, you descend all the way down to the leaf.
    while (node && !node->isleaf) {
        int direction = point->x >= node->threshold; // [D]
        node = node->child[direction];
    }
    // actually you shouldn't expect to find a NULL leaf here.
    int minIndex = 0;
    double mindist = 1e300;
    if (!node) return NULL;
    for (int i = 0; i < node->npoints; i++) {
        if (distanceFunction(node->points[i], point) < mindist) minIndex = i;
    }
    // a point is always returned. you asked for the near*est* point, and there
    // is always one, regardless of how near or far it is exactly.
    // THATS NOT TRUE!!!!!
    // MAYBE: pass mindist back to the caller to save a repeated
    // distanceFunction call.
    return node->points[minIndex];
}
// static MPoint points[] = { { 1 }, { 3 }, { -5 } };

#define countof(x) (sizeof(x) / sizeof(x[0]))
static const char* const spc = "                                            ";
void printPoint(MPoint* p, int lev)
{
    printf("%.*sPoint (%g)\n", lev, spc, p->x); //, p->y, p->z);
}

void printNode(Node* node, int lev)
{
    // printPoint(node, lev);
    if (node->isleaf)
        for (int j = 0; j < node->npoints; j++)
            printPoint(node->points[j], lev);
    else {
        printf("%.*sNode (%g)\n", lev, spc,
            node->threshold); //, node->p.y, node->p.z);
        for (int i = 0; i < 2; i++)
            // for (int j = 0; j < 2; j++)
            //     for (int k = 0; k < 2; k++) //
            if (node->child[i])
            //[j][k])
            {
                printf("%.*s-%c-\n", lev, spc,"<>"[i]);

                printNode(node->child[i], lev + 4);
            }

            else
                printf("%.*s---\n", lev, spc);
    }
}
static MPoint gPoints[] = { { -5 }, { -4 }, { -3 }, { -2 }, { 6 }, { -5.35 },
    { -4.35 }, { -3.35 }, { -2.4545 }, { 1.45 }, { -5.3578 }, { -4.6789 },
    { -3.8679 }, { -2.5557 }, { -1.6767 }, { -5.10354225 }, { -4.035 }, { -3.3335 },
    { -2.45 }, { -1.04455 } };

static MPoint g__Points[] = {
    { -0, -2, 3 }, //
    { -0, 1, 4 }, //
    { -0, 2, -4 }, //
    { -0, 5, -2 }, //
    { -1, -2, -3 }, //
    { -1, -3, 4 }, //
    { -1, -5, -2 }, //
    { -1, 0, 5 }, //
    { -1, 1, 1 }, //
    { -1, 3, 0 }, //
    { -1, 3, 2 }, //
    { -1, 3, 4 }, //
    { -1, 4, -2 }, //
    { -1, 4, 3 }, //
    { -1, 4, 4 }, //
    { -1, 5, 4 }, //
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

int main()
{
    Node root[] = { { .threshold = 0 } };
    Node rc1[] = { { .threshold = -3 } };
    Node rc2[] = { { .threshold = 3 } };

    root->child[0] = rc1;
    root->child[1] = rc2;
    for (int i = 0; i < countof(gPoints); i++) addPoint(root, gPoints + i);
    printNode(root, 0);
    
    MPoint ptest[] = {{-3.14159}};
    MPoint* nea = getNearestPoint(root, ptest);
    if (nea) printPoint(nea, 0);
}
