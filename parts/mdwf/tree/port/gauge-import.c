#include <mdwf.h>

int
QX(import_gauge)(struct QX(Gauge) **gauge_ptr,
                 struct Q(State) *state,
                 void (*reader)(double *val_re,
                                double *val_im,
                                int dir,
                                const int pos[Q(DIM)],
                                int a,
                                int b,
                                void *env),
                 void *env)
{
  struct QX(Gauge) *gauge;
  void *ptr;
  size_t size;
  double r[Q(DIM) * Q(COLORS) * Q(COLORS) * 2];
  double *v;
  int p, d, a, b;
  int x[Q(DIM)];
  int us;

  if (state == NULL || state->error_latched)
    return 1;

  if (gauge_ptr == NULL)
    return q(set_error)(state, 0, "import_gauge(): NULL pointer");

  *gauge_ptr = NULL;
  us = qx(sizeof_gauge)(state->volume);
  gauge = q(allocate_aligned)(state, &size, &ptr,
                              sizeof (struct QX(Gauge)), us);
  if (gauge == 0)
    return q(set_error)(state, 0, "import_gauge(): not enough memory");

  BEGIN_TIMING(state);
  gauge->state = state;
  gauge->size = size;
  gauge->data = ptr;
  *gauge_ptr = gauge;

  for (p = 0; p < state->volume; p++) {
    q(l2v)(x, &state->local, state->lx2v[p]);
    for (v = r, d = 0; d < Q(DIM); d++) {
      for (a = 0; a < Q(COLORS); a++) {
        for (b = 0; b < Q(COLORS); b++) {
            double v_re, v_im;
            reader(&v_re, &v_im, d, x, a, b, env);
            *v++ = -0.5 * v_re;
            *v++ = -0.5 * v_im;
        }
      }
    }
    qx(put_gauge)(gauge->data, p, r);
  }
  END_TIMING(state, 0, 0, 0);
  return 0;
}
