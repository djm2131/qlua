#include <mdwf.h>

int
QX(put_vector_fermion)(struct QX(VectorFermion) *r,
		       int index,
		       const struct QX(HalfFermion) *a)
{
  DECLARE_STATE;
  int count;

  CHECK_ARG0(r);
  CHECK_ARGn(a, "put_vector_fermion");
  count = r->count;

  if (index < 0 || index >= count)
      return q(set_error)(state, 0, "put_vector_fermion(): bad index");

  BEGIN_TIMING(state);
  qx(omp_fv_put)(state, r->cb_x, index,
		 state->lat_x->full_size, state->Ls, count,
		 a->cb_x);
  END_TIMING(state, 0, 0, 0);
  return 0;
  
}
