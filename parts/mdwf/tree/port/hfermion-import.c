#include <mdwf.h>

int
QX(import_half_fermion)(struct QX(HalfFermion) **half_fermion_ptr,
                        struct Q(State) *state,
                        void (*reader)(double *val_re,
                                       double *val_im,
                                       const int pos[5],
                                       int color,
                                       int dirac,
                                       void *env),
                        void *env)
{
  double *m;
  int size;

  if (QX(allocate_half_fermion)(half_fermion_ptr, state) != 0)
    return 1;

  size = state->Ls * Q(FERMION_DIM) * Q(COLORS) * 2 * sizeof (double);
  m = q(malloc)(state, size);
  if (m == 0) {
    QX(free_half_fermion)(half_fermion_ptr);
    return q(set_error)(state, 0, "import_half_fermion(): not enough space");
  }
  BEGIN_TIMING(state);
  qx(x_import)(state->lat_x, m, (*half_fermion_ptr)->cb_x, reader, env);
  END_TIMING(state, 0, 0, 0);

  q(free)(state, m, size);
  return 0;
}
