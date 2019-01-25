#include <stdio.h>
#include <string.h>
#include <clover.h>
#include "vfermion-test.h"

#define ESIZE 3
#define UDIM  5

static int pattern[] = {
    1,  0,  0,  1, -1,  0,  0, -1,
    1,  1,  1, -1, -1,  1, -1, -1,
    2,  1,  1,  2,  2, -1, -1,  2,

   -2,  1, -2, -1,  2,  1,  2, -1,
   -1,  2, -1, -2,  1,  2,  1, -2,
    3,  4,  5,  6,  7,  8,  9, -4,

    5,  4,  6, -7,  8,  7,  6,  5,
   -1, -4, -3, -2, -7, -6, -9, -9,
   -9, -8, -7, -7, -7, -7, -7, -7
};

int
main(int argc, char *argv[])
{
    struct Fermion *f1 = new_fermion(ESIZE);
    struct Fermion *f2 = new_fermion(ESIZE);
    struct Fermion *f3 = new_fermion(ESIZE);
    struct Fermion *ft = new_fermion(ESIZE);
    struct vFermion *v0 = new_vfermion(ESIZE, UDIM);
    struct vFermion *v1 = new_vfermion(ESIZE, 2 * UDIM);
    int i;

    mk_fermion(f1, ESIZE,  pattern, 24);
    mk_fermion(f2, ESIZE,  pattern, 20);
    mk_fermion(f3, ESIZE,  pattern, 13);

    show_fermion("F1", f1, ESIZE);
    show_fermion("F2", f2, ESIZE);
    show_fermion("F3", f3, ESIZE);

    qx(fv_put)(ESIZE, v0, UDIM, 0, f1);
    qx(fv_put)(ESIZE, v0, UDIM, 1, f2);
    qx(fv_put)(ESIZE, v0, UDIM, 2, f3);

    qx(fv_get)(ESIZE, ft, v0, UDIM, 0);
    show_fermion("v0", ft, ESIZE);

    qx(fv_get)(ESIZE, ft, v0, UDIM, 2);
    show_fermion("v2", ft, ESIZE);

    qx(fv_copy)(ESIZE, 2, v1, 2 * UDIM, 0, v0, UDIM, 0);
    qx(fv_copy)(ESIZE, 2, v1, 2 * UDIM, 2, v0, UDIM, 1);
    qx(fv_copy)(ESIZE, 2, v1, 2 * UDIM, 4, v0, UDIM, 1);

    for (i = 0; i < UDIM; i++) {
        char n[10];

        qx(fv_get)(ESIZE, ft, v0, UDIM, i);
        sprintf(n, "v0 %d", i);
        show_fermion(n, ft, ESIZE);
    }

    for (i = 0; i < 2 * UDIM; i++) {
        char n[10];

        qx(fv_get)(ESIZE, ft, v1, 2 * UDIM, i);
        sprintf(n, "zz %d", i);
        show_fermion(n, ft, ESIZE);
    }

    return 0;
}
