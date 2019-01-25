#include "twisted.h"

int
QX(norm2_half_fermion)(double *v_r,
                       const struct QX(HalfFermion) *a)
{
  long long flops;
  DECLARE_STATE;

  CHECK_ARG0(a);
  CHECK_POINTER(v_r, "norm2_half_fermion");

  BEGIN_TIMING(state);
  flops = qx(f_norm)(v_r,
                     state->even.full_size,
                     a->even);
  QMP_sum_double(v_r);
  END_TIMING(state, flops, sizeof (double), sizeof (double));
  return 0;
  
}
