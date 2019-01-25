#include <stdio.h>
#include <stdlib.h>
#include <clover.h>
#include <math.h>
#include "vfermion-test.h"

void
test_dot_zm(const char *name,
            int esize, 
            struct vFermion *w,
            int w_w, int w_b, int w_l,
            struct vFermion *v,
            int v_w, int v_b, int v_l,
            double *z, int ldz,
            struct Fermion *t0,
            struct Fermion *t1)
{
    int i, j;
    double f0;
    double ff;
    double err;
    double m_e = 0;

    qx(fv_dot_zm)(esize,
                  w, w_w, w_b, w_l,
                  v, v_w, v_b, v_l,
                  z, ldz);
    for (i = 0; i < w_l; i++) {
        qx(fv_get)(esize, t0, w, w_w, i + w_b);
        qx(f_norm)(&f0, esize, t0);
        for (j = 0; j < v_l; j++) {
            qx(fv_get)(esize, t1, v, v_w, j + v_b);
            qx(f_cadd2)(t0, esize,
                        -z[2*(j + ldz * i)], -z[2*(j+ ldz * i) + 1],
                        t1);
        }
        qx(f_norm)(&ff, esize, t0);
        err = sqrt(ff/f0);
        printf("%s %3d %15.7e %15.7e %15.7e\n", name, i, err, ff, f0);
        if (m_e < err)
            m_e = err;
    }
    printf("MAX err: %15.7e\n", err);
}

int
main(int argc, char *argv[])
{
    int esize;
    int v_w, v_b, v_l;
    int w_w, w_b, w_l;
    struct vFermion *Vv;
    struct vFermion *Vw;
    struct Fermion *t0;
    struct Fermion *t1;
    double *z;
    int ldz;

    if (argc != 9) {
        fprintf(stderr, 
                "Usage: check/fv:zm esize v_w v_b v_l w_w w_b w_l ldz\n");
        return 1;
    }
    esize = atoi(argv[1]);
    v_w = atoi(argv[2]);
    v_b = atoi(argv[3]);
    v_l = atoi(argv[4]);
    w_w = atoi(argv[5]);
    w_b = atoi(argv[6]);
    w_l = atoi(argv[7]);
    ldz = atoi(argv[8]);

    t0 = new_fermion(esize);
    t1 = new_fermion(esize);
    Vv = new_vfermion(esize, v_w);
    Vw = new_vfermion(esize, w_w);
    z = malloc(2 * ldz * v_l * sizeof (double));
    construct_d(2 * ldz * v_l, z, -756.345234);
    construct_vf(esize, w_w, Vw, t0, 12.45);

    test_dot_zm("ZM", esize,
                Vv, v_w, v_b, v_l,
                Vw, w_w, w_b, w_l,
                z, ldz, t0, t1);

    return 0;
}
