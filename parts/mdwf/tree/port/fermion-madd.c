#include <mdwf.h>

int
QX(madd_fermion)(struct QX(Fermion) *r,
		 const struct QX(Fermion) *a,
		 double alpha,
		 const struct QX(Fermion) *b)
{
  long long flops;
  DECLARE_STATE;

  CHECK_ARG0(r);
  CHECK_ARGn(a, "madd_fermion");
  CHECK_ARGn(b, "madd_fermion");

  BEGIN_TIMING(state);
  flops = qx(omp_f_add3)(state, r->cb_x,
			 state->lat_x->full_size, state->Ls,
			 a->cb_x, alpha, b->cb_x);
  flops += qx(omp_f_add3)(state, r->cb_y,
			  state->lat_y->full_size, state->Ls,
			  a->cb_y, alpha, b->cb_y);
  END_TIMING(state, flops, 0, 0);
  return 0;
  
}
