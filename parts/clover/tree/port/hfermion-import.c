#include <clover.h>

int
QX(import_half_fermion)(struct QX(HalfFermion) **half_fermion_ptr,
                        struct Q(State) *state,
                        double (*reader)(const int pos[4],
                                         int color,
                                         int dirac,
                                         int re_im,
                                         void *env),
                        void *env)
{
  if (QX(allocate_half_fermion)(half_fermion_ptr, state) != 0)
    return 1;

  BEGIN_TIMING(state);
  qx(x_import)(&state->even, (*half_fermion_ptr)->even, reader, env);
  END_TIMING(state, 0, 0, 0);

  return 0;
}
