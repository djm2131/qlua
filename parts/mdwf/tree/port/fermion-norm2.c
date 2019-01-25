#include <mdwf.h>

size_t
qx(op_norm2)(double *norm,
	    const struct QX(Fermion) *a,
	    struct Q(State) *state)
{
    double n_x;
    double n_y;
    size_t flops = 0;

    flops += qx(omp_f_norm)(state, &n_x, state->lat_x->full_size, state->Ls, a->cb_x);
    flops += qx(omp_f_norm)(state, &n_y, state->lat_y->full_size, state->Ls, a->cb_y);
    *norm = n_x + n_y;
    QMP_sum_double(norm);
    return flops + 1;
}

int
QX(norm2_fermion)(double *v_r,
		  const struct QX(Fermion) *a)
{
  size_t flops = 0;
  DECLARE_STATE;

  CHECK_ARG0(a);
  CHECK_POINTER(v_r, "norm2_fermion");

  BEGIN_TIMING(state);
  flops += qx(op_norm2)(v_r, a, state);
  END_TIMING(state, flops, sizeof (double), sizeof (double));

  return 0;  
}
