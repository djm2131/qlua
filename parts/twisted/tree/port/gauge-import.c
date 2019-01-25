#include "twisted.h"


int
QX(import_gauge)(struct QX(Gauge) **gauge_ptr,
                 struct Q(State) *state,
		 double mq_re,
		 double mq_im,
		 double mu_re,
		 double mu_im,
                 double (*U_reader)(int dir,
                                    const int pos[Q(DIM)],
                                    int a,
                                    int b,
                                    int re_im,
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
  int u_s;
  double x_re = 4.0 + mq_re - mu_im;
  double x_im = mq_im + mu_re;
  double y_re = 4.0 + mq_re + mu_im;
  double y_im = mq_im - mu_re;
  double x2 = x_re * x_re + x_im * x_im;
  double y2 = y_re * y_re + y_im * y_im;
  double ix_re = x_re / x2 ;
  double ix_im = -x_im / x2 ;
  double iy_re = y_re / y2 ;
  double iy_im = -y_im / y2 ;

  if (state == NULL || state->error_latched)
    return 1;

  if (gauge_ptr == NULL)
    return q(set_error)(state, 0, "import_gauge(): NULL pointer");

  *gauge_ptr = NULL;
  u_s = qx(sizeof_gauge)(state->volume);
  gauge = q(allocate_aligned)(state, &size, &ptr, sizeof (struct QX(Gauge)), u_s);
  if (gauge == 0)
    return q(set_error)(state, 0, "import_gauge(): not enough memory");

  gauge->twist = q(malloc)(state, q(sizeof_twist)());
  if (gauge->twist == 0) {
    q(free)(state, gauge, size);
    return q(set_error)(state, 0, "import_gauge(): not enough memory");
  }

  gauge->twist_inv = q(malloc)(state, q(sizeof_twist)());
  if (gauge->twist_inv == 0) {
    q(free)(state, gauge->twist, q(sizeof_twist)());
    q(free)(state, gauge, size);
    return q(set_error)(state, 0, "import_gauge(): not enough memory");
  }

  BEGIN_TIMING(state);
  gauge->state = state;
  gauge->size = size;
  gauge->g_data = ptr;
  *gauge_ptr = gauge;

  q(put_twisted)(gauge->twist, x_re, x_im, y_re, y_im);
  q(put_twisted)(gauge->twist_inv, ix_re, ix_im, iy_re, iy_im);

  for (p = 0; p < state->volume; p++) {
      q(l2v)(x, &state->local, state->lx2v[p]);
      for (v = r, d = 0; d < Q(DIM); d++) {
          for (a = 0; a < Q(COLORS); a++) {
              for (b = 0; b < Q(COLORS); b++) {
                  *v++ = -0.5 * U_reader(d, x, a, b, 0, env);
                  *v++ = -0.5 * U_reader(d, x, a, b, 1, env);
              }
          }
      }
      qx(put_gauge)(gauge->g_data, p, r);
  }

  END_TIMING(state, 0, 0, 0);
  return 0;
}
