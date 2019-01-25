#include "twisted.h"

int
Q(gauge_float_from_double)(struct QF(Gauge) **result_ptr,
                           const struct QD(Gauge) *gauge_d)
{
  DECLARE_STATE;
  struct QF(Gauge) *g;
  int u_s;
  void *ptr;
  size_t size;

  CHECK_ARG0(gauge_d);
  *result_ptr = NULL;
  u_s = qf(sizeof_gauge)(state->volume);
  g = q(allocate_aligned)(state, &size, &ptr, sizeof (struct QF(Gauge)), u_s);
  if (g == 0)
    return q(set_error)(state, 0, "gauge_float_from_double(): not enough memory");

  g->twist = q(malloc)(state, q(sizeof_twist)());
  if (g->twist == 0) {
    q(free)(state, g, size);
    return q(set_error)(state, 0, "gauge_float_from_double(): not enough memory");
  }
  g->twist_inv = q(malloc)(state, q(sizeof_twist)());
  if (g->twist_inv == 0) {
    q(free)(state, g->twist, q(sizeof_twist)());
    q(free)(state, g, size);
    return q(set_error)(state, 0, "gauge_float_from_double(): not enough memory");
  }

  memcpy(g->twist, gauge_d->twist, q(sizeof_twist)());
  memcpy(g->twist_inv, gauge_d->twist_inv, q(sizeof_twist)());

  g->state = state;
  g->size = size;
  g->g_data = ptr;
  *result_ptr = g;
  
  q(g_f_eq_d)(g->g_data, Q(DIM) * state->volume, gauge_d->g_data);

  return 0;
}
