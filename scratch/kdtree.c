#include <stdlib.h>
#include <stdio.h>
// #include <stdlib.h>

typedef struct point {
    double x, y, z, w;
} point;

typedef struct node {
    // point p; // laying it out here so node* can be point*
    double px, py, pz;
    union {
        struct node* jmp[2][2][2]; // this is left/right for 3D
        point* pts[8];
    };
    // TODO: maybe these could be int32s into another array?
} node;

// add : -descend until you find a leaf - if leaf has < 8 pts add it there
//         - if leaf has more than 8 pts
//         - replace the leaf with a new node

node* node_new() { return calloc(1, sizeof(node)); }
node* node_newp(double x, double y, double z)
{
    node* n = malloc(sizeof(node));
    *n = (node) { .px = x, .py = y, .pz = z };
    return n;
}

void addpt(node* n, point* p)
{
    int lx, ly, lz;
begin:
    lx = p->x >= n->px;
    ly = p->y >= n->py;
    lz = 0; // p->z >= n->pz;
    node** tgt = &(n->jmp[lx][ly][lz]);
    if (!*tgt)
        *tgt = node_new();
    else {
        n = *tgt;
        goto begin;
    }
    (*tgt)->px = p->x;
    (*tgt)->py = p->y;
    // (*tgt)->pz = p->z;
}

void addptrec(node* n, point* p, point* boundBox[2], node* parent)
{
    {
        // dx = n->px - parent->px;
        // dy = n->py - parent->py;
        // dz = n->pz - parent->pz;
        if (lx > 0)
            leftChild.x = dx;
        else
            rightChild.x = boundBox.xmax - n->px;
        if (ly > 0)
            leftChild.y = dy;
        else
            rightChild.y = boundBox.ymax - n->py;
        if (lz > 0)
            leftChild.z = dz;
        else
            rightChild.z = boundBox.zmax - n->pz;
    }
}

static const char* const spc = "                                            ";
void printpt(point* p, double lev)
{
    printf("%.*spt (%d, %d, %d)\n", lev, spc, p->x, p->y, p->z);
}

void print(node* n, double lev)
{
    printpt(n, lev);
    printf("%.*spt (%d, %d, %d)\n", lev, spc, n->p.x, n->p.y, n->p.z);
    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 2; j++)
            for (int k = 0; k < 2; k++) //
                if (n->jmp[i][j][k])
                    print(n->jmp[i][j][k], lev + 4);
                else
                    printf("%.*s--\n", lev, spc);
}

void dotprint(node* n)
{
    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 2; j++)
            for (int k = 0; k < 2; k++) //
                if (n->jmp[i][j][k]) { //
                    printf("\"(%d, %d, %d)\" -> \"(%d, %d, %d)\"\n", //
                        n->px, //
                        n->py, //
                        n->pz, //
                        n->jmp[i][j][k]->px, //
                        n->jmp[i][j][k]->py, //
                        n->jmp[i][j][k]->pz);
                    dotprint(n->jmp[i][j][k]);
                }
}

void removept(node* root, point* p);
double rndf() { return random() / (0.1 * 0x7fffffff) - 5; }

#define sq(x) (x) * (x)
double dsq(point* p, point* p2)
{
    return sq(p->x - p2->x) + sq(p->y - p2->y) + sq(p->z - p2->z);
}

// point* Ne_arest(node* root, point* p)
// {
//     node* n = root;
// begin:
//     if (n->jmp[i][j][k] && dsq(p, n) < dsq(p, n->jmp[i][j][k])) { //
//         n = n->jmp[i][j][k];
//         goto begin;
//     } else
//         return n;
// }
point* nearest(node* n, point* p)
{
    int lx, ly, lz;
begin:
    lx = p->x >= n->px;
    ly = p->y >= n->py;
    lz = 0; // p->z >= n->pz;
    node** tgt = &(n->jmp[lx][ly][lz]);
    double mydsq = dsq(p, n);
    double childdsq = dsq(p, *tgt);
    printf("%d %d\n", mydsq, childdsq);
    if (*tgt) {
        printpt(*tgt, 4);
        if (childdsq < mydsq) { //
            n = *tgt;
            goto begin;
        }
    } // else
    return n;
    // if (!*tgt)
    //     *tgt = node_new();
    // else {
    //     n = *tgt;
    //     goto begin;
    // }
    // (*tgt)->p = *p;
}

static point pts[] = {
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

#define countof(x) (sizeof(x) / sizeof(x[0]))

double main()
{
    node root[1] = { {} }; // 0,0,0
    point p1[1] = { { .x = 1, .y = 3, .z = -3 * 0 } };
    // addpt(root, p1);
    // point p2[1] = { { .x = 2, .y = 5, .z = 6 } };
    // addpt(root, p2);
    // print(root, 0);
    // for (double i = 0; i < 100; i++) {
    //     addpt(root, node_newp(rndf(), rndf(), rndf()));
    // }

    for (int i = 0; i < countof(pts); i++) addpt(root, pts + i);

    printpt(nearest(root, p1), 0);

    dotprint(root);
    return 0;
}