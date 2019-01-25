#include <mdwf.h>

int
QX(get_vector_fermion)(struct QX(HalfFermion) *r,
		       const struct QX(VectorFermion) *a,
		       int index)
{
  DECLARE_STATE;
  int count;

  CHECK_ARG0(r);
  CHECK_ARGn(a, "get_vector_fermion");
  count = a->count;

  if (index < 0 || index >= count)
      return q(set_error)(state, 0, "get_vector_fermion(): bad index");

  BEGIN_TIMING(state);
  qx(omp_fv_get)(state, r->cb_x,
		 state->lat_x->full_size, state->Ls, count,
		 a->cb_x, index);
  END_TIMING(state, 0, 0, 0);
  return 0;
  
}
