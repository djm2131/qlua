#include <clover.h>

int
QX(allocate_fermion)(struct QX(Fermion) **fermion_ptr,
                     struct Q(State) *state)
{
  struct QX(Fermion) *fermion;
  void *even;
  size_t size;

  if (state == NULL || state->error_latched)
    return 1;

  if (fermion_ptr == NULL)
    return q(set_error)(state, 0, "allocate_fermion(): NULL pointer");
  
  *fermion_ptr = NULL;
  fermion = qx(allocate_eo)(state, &size, &even,
                            sizeof (struct QX(Fermion)), 1, 1);
  if (fermion == 0)
    return q(set_error)(state, 0, "allocate_fermion(): not enough memory");

  BEGIN_TIMING(state);
  fermion->state = state;
  fermion->size = size;
  fermion->even = even;
  fermion->odd = qx(step_even)(state, even);
  *fermion_ptr = fermion;
  END_TIMING(state, 0, 0, 0);

  return 0;
}
