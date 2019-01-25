#include <clover.h>

/* Even/odd must agree in half_fermion_size() and parity() */

int
Q(half_fermion_size)(struct Q(State) *state)
{
  if (state == NULL || state->error_latched)
    return 0;

  return state->even.full_size * 2 * Q(FERMION_DIM) * Q(COLORS);
}

int
Q(parity)(struct Q(State) *state)
{
  return 0;
}
