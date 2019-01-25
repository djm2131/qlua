#include <mdwf.h>

int
QX(norm2_half_fermion)(double *v_r,
		       const struct QX(HalfFermion) *a)
{
  long long flops;
  DECLARE_STATE;

  CHECK_ARG0(a);
  CHECK_POINTER(v_r, "norm2_half_fermion");

  BEGIN_TIMING(state);
  flops = qx(omp_f_norm)(state, v_r,
			 state->lat_x->full_size, state->Ls,
			 a->cb_x);
  QMP_sum_double(v_r);
  END_TIMING(state, flops, sizeof (double), sizeof (double));
  return 0;
  
}
