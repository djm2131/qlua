#include <clover.h>

int
QX(export_half_fermion)(void (*writer)(const int pos[4],
                                       int color,
                                       int dirac,
                                       int re_im,
                                       double value,
                                       void *env),
                        void *env,
                        const struct QX(HalfFermion) *half_fermion)
{
  struct Q(State) *state;

  if (half_fermion == 0)
    return 1;

  state = half_fermion->state;
  BEGIN_TIMING(state);
  qx(x_export)(&state->even, half_fermion->even, writer, env);
  END_TIMING(state, 0, 0, 0);

  return 0;
}
