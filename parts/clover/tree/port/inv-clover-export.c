#include <clover.h>

int
QX(export_inv_clover)(void (*writer)(const int pos[Q(DIM)],
                                     int a, int i, int b, int j,
                                     int re_im,
                                     double value,
                                     void *env),
                      void *env,
                      const struct QX(Gauge) *gauge)
{
  struct Q(State) *state;
  double r[Q(COLORS) * Q(COLORS)
           * Q(PROJECTED_FERMION_DIM) * Q(PROJECTED_FERMION_DIM) * 2];
  int size;
  int p, a, i, b, j;
  int x[Q(DIM)];
  double *v;
  struct eo_lattice *eo;

  if (gauge == 0)
    return 1;

  state = gauge->state;

  BEGIN_TIMING(state);
  eo = &state->odd;
  size = eo->full_size;
  for (p = 0; p < size; p++) {
    q(l2v)(x, eo->local, eo->lx2v[p]);

    qx(get_clover_lo)(r, gauge->cox_data, p);
    for (v = r, a = 0; a < Q(COLORS); a++) {
      for (i = 0; i < Q(PROJECTED_FERMION_DIM); i++) {
        for (b = 0; b < Q(COLORS); b++) {
          for (j = 0; j < Q(PROJECTED_FERMION_DIM); j++) {
            writer(x, a, i, b, j, 0, *v++, env);
            writer(x, a, i, b, j, 1, *v++, env);
          }
        }
      }
    }

    qx(get_clover_hi)(r, gauge->cox_data, p);
    for (v = r, a = 0; a < Q(COLORS); a++) {
      for (i = Q(PROJECTED_FERMION_DIM); i < 2*Q(PROJECTED_FERMION_DIM); i++) {
        for (b = 0; b < Q(COLORS); b++) {
          for (j = Q(PROJECTED_FERMION_DIM); j < 2*Q(PROJECTED_FERMION_DIM); j++) {
            writer(x, a, i, b, j, 0, *v++, env);
            writer(x, a, i, b, j, 1, *v++, env);
          }
        }
      }
    }
  }
  END_TIMING(state, 0, 0, 0);

  return 0;
}
