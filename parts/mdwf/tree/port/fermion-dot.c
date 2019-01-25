#include <mdwf.h>

int
QX(dot_fermion)(double *v_r, double *v_i,
                const struct QX(Fermion) *a,
                const struct QX(Fermion) *b)
{
  long long flops;
  double x_r = 0;
  double x_i = 0;
  double y_r = 0;
  double y_i = 0;
  DECLARE_STATE;

  CHECK_ARG0(a);
  CHECK_ARGn(b, "dot_fermion");
  CHECK_POINTER(v_r, "dot_fermion");
  CHECK_POINTER(v_i, "dot_fermion");

  BEGIN_TIMING(state);
  flops  = qx(omp_f_dot)(state, &x_r, &x_i,
		 	 state->lat_x->full_size, state->Ls,
		         a->cb_x, b->cb_x);
  flops += qx(omp_f_dot)(state, &y_r, &y_i,
			 state->lat_y->full_size, state->Ls,
			 a->cb_y, b->cb_y);
  *v_r = x_r + y_r;
  *v_i = x_i + y_i;
  QMP_sum_double(v_r);
  QMP_sum_double(v_i);
  END_TIMING(state, flops, 2 * sizeof (double), 2 * sizeof (double));
  return 0;
  
}
