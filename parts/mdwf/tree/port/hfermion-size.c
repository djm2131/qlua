#include <mdwf.h>

/* Even/odd must agree in half_fermion_size() and parity() */

int
Q(half_fermion_size)(struct Q(State) *state)
{
  if (state == NULL || state->error_latched)
    return 0;

  return state->lat_x->full_size * state->Ls * 2 * QOP_MDWF_FERMION_DIM * QOP_MDWF_COLORS;
}

int
Q(parity)(struct Q(State) *state)
{
  return state->parity;
}
