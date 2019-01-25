#include <clover.h>

void
qx(x_export)(struct eo_lattice *eo,
             const struct FermionX *data,
             void (*writer)(const int pos[Q(DIM)],
                            int color,
                            int dirac,
                            int re_im,
                            double v,
                            void *env),
             void *env)
{
    double r[Q(FERMION_DIM) * Q(COLORS) * 2];
    int size = eo->full_size;
    int p, c, d;
    int x[Q(DIM)];
    double *v;
    
    for (p = 0; p < size; p++) {
        q(l2v)(x, eo->local, eo->lx2v[p]);
        qx(get_fermion)(r, data, p);
        for (v = r, c = 0; c < Q(COLORS); c++) {
            for (d = 0; d < Q(FERMION_DIM); d++) {
                writer(x, c, d, 0, *v++, env);
                writer(x, c, d, 1, *v++, env);
            }
        }
    }
}
