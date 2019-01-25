#include <clover.h>

int
Q(gauge_float_from_double)(struct QF(Gauge) **result_ptr,
                           struct QD(Gauge) *gauge_d)
{
  DECLARE_STATE;
  struct QF(Gauge) *g;
  int u_s, ce_s, co_s;
  void *ptr;
  size_t size;

  CHECK_ARG0(gauge_d);
  *result_ptr = NULL;
  u_s = qf(sizeof_gauge)(state->volume);
  ce_s = qf(sizeof_clover)(state->even.full_size);
  co_s = qf(sizeof_clover)(state->odd.full_size);
  g = q(allocate_aligned)(state, &size, &ptr,
                          sizeof (struct QF(Gauge)), u_s + ce_s + 2 * co_s);
  if (g == 0)
    return q(set_error)(state, 0, "gauge_float_from_double(): not enough memory");
  g->state = state;
  g->size = size;
  g->g_data = ptr;
  g->ce_data = (void *)(((char *)(g->g_data)) + u_s);
  g->co_data = (void *)(((char *)(g->ce_data)) + ce_s);
  g->cox_data = (void *)(((char *)(g->co_data)) + co_s);
  *result_ptr = g;
  
  q(g_f_eq_d)(g->g_data, Q(DIM) * state->volume, gauge_d->g_data);
  q(c_f_eq_d)(g->ce_data, state->even.full_size, gauge_d->ce_data);
  q(c_f_eq_d)(g->co_data, state->odd.full_size, gauge_d->co_data);
  q(c_f_eq_d)(g->cox_data, state->odd.full_size, gauge_d->cox_data);

  return 0;
}
