#include <clover.h>

void
qx(x_import)(struct eo_lattice *eo,
             struct FermionX *data,
             double (*reader)(const int pos[Q(DIM)],
                              int color,
                              int dirac,
                              int re_im,
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
    for (v = r, c = 0; c < Q(COLORS); c++) {
        for (d = 0; d < Q(FERMION_DIM); d++) {
            *v++ = reader(x, c, d, 0, env);
            *v++ = reader(x, c, d, 1, env);
        }
    }
    qx(put_fermion)(data, p, r);
  }
}
