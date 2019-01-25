#include <stdio.h>
#include <stdlib.h>
#include <clover.h>
#include <math.h>
#include "vfermion-test.h"

void
test_dot_zv(const char *name,
            int esize, 
            struct vFermion *v,
            int v_w, int v_b, int v_l,
            double *zv,
            struct Fermion *t0,
            struct Fermion *t1)
{
    int i;
    double f0;
    double ff;

    qx(fv_dot_zv)(esize, t0, v, v_w, v_b, v_l, zv);
    qx(f_norm)(&f0, esize, t0);
    for (i = 0; i < v_l; i++) {
        qx(fv_get)(esize, t1, v, v_w, i + v_b);
        qx(f_cadd2)(t0, esize, -zv[2*i], -zv[2*i+1], t1);
    }
    qx(f_norm)(&ff, esize, t0);
    printf("DELTA: %15.7e\n", sqrt(ff/f0));
}

int
main(int argc, char *argv[])
{
    int esize;
    int width;
    int v_begin, v_len;
    struct vFermion *v0;
    struct Fermion *t0;
    struct Fermion *t1;
    double *zv;

    if (argc != 5) {
        fprintf(stderr, "Usage: check/fv:zv esize width v_b v_l\n");
        return 1;
    }
    esize = atoi(argv[1]);
    width = atoi(argv[2]);
    v_begin = atoi(argv[3]);
    v_len = atoi(argv[4]);

    t0 = new_fermion(esize);
    t1 = new_fermion(esize);
    v0 = new_vfermion(esize, width);
    zv = malloc(2 * width * sizeof (double));
    construct_d(2 * width, zv, -756.345234);

    construct_vf(esize, width, v0, t0, 12.45);
    construct_f(esize, t0, 0.45673);

    test_dot_zv("ZV", esize, v0, width, v_begin, v_len, zv, t0, t1);

    return 0;
}
