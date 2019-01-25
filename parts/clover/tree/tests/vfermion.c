#include <stdlib.h>
#include <math.h>
#include <clover.h>
#include "vfermion-test.h"

void
show_fermion_pt(const char *name, int k, struct Fermion *f)
{
    double z[2 * QOP_CLOVER_FERMION_DIM * QOP_CLOVER_COLORS];
    int c, d;

    qx(get_fermion)(z, f, k);

    for (d = 0; d < QOP_CLOVER_FERMION_DIM; d++) {
        printf("%s %d [%d]:", name, k, d);
        for (c = 0; c < QOP_CLOVER_COLORS; c++) {
            printf("  %8.3f %8.3f",
                   z[0 + 2 * (c + QOP_CLOVER_COLORS * d)],
                   z[1 + 2 * (c + QOP_CLOVER_COLORS * d)]);
        }
        printf("\n");
    }
}

void
show_fermion(const char *name, struct Fermion *f, int size)
{
    int i;

    for (i = 0; i < size; i++)
        show_fermion_pt(name, i, f);
    printf("\n");
}

void
show_vfermion(const char *name, struct vFermion *v, int esize, int width,
              struct Fermion *t)
{
    char pref[72];
    int i;

    printf("-------- begin(%s)\n", name);
    for (i = 0; i < width; i++) {
        qx(fv_get)(esize, t, v, width, i);
        sprintf(pref, "%s %d", name, i);
        show_fermion(pref, t, esize);
    }
    printf("-------- (%s)end\n\n", name);
}


void
mk_fermion(struct Fermion *f, int size, int *data, int stride)
{
    int i, c, d;
    int *p;
    double *q;
    double z[2 * QOP_CLOVER_FERMION_DIM * QOP_CLOVER_COLORS];

    for (i = 0; i < size; i++, data += stride) {
        for (p = data, q = z, c = 0; c < QOP_CLOVER_COLORS; c++) {
            for (d = 0; d < QOP_CLOVER_FERMION_DIM; d++, p += 2, q += 2) {
                q[0] = p[0];
                q[1] = p[1];
            }
        }
        qx(put_fermion)(f, i, z);
    }
}

struct Fermion *new_fermion(int size)
{
    void *ref = malloc(2 * qx(sizeof_fermion(size)) + 127);

    return (void *)(((uintptr_t)ref + 127) & (~(uintptr_t)127));
}

struct vFermion *new_vfermion(int size, int width)
{
    void *ref = malloc(2 * qx(sizeof_vfermion)(size, width) + 127);

    return (void *)(((uintptr_t)ref + 127) & (~(uintptr_t)127));
}

void
construct_f(int esize, struct Fermion *f, double m)
{
    int j, k;
    double z[2 * QOP_CLOVER_COLORS * QOP_CLOVER_FERMION_DIM];

    for (j = 0; j < esize; j++) {
        for (k = 0; k < sizeof (z) / sizeof (z[0]); k++) {
            z[k] = sin((k * m - (j + 1) * (k + 2)) / (j + m));
        }
        qx(put_fermion)(f, j, z);
    }
}

void
construct_vf(int esize, int width,
             struct vFermion *dst,
             struct Fermion *t, double m)
{
    int i;

    for (i = 0; i < width; i++) {
        construct_f(esize, t, cos((i * m)/(i + m + 4)));
        qx(fv_put)(esize, dst, width, i, t);
    }
}

void
construct_d(int len,
            double *d,
            double m)
{
    int i;

    for (i = 0; i < len; i++)
        d[i] = sin((i + len)/(i * i + m + len));
}
