#include <mdwf.h>

int
Q(set_Shamir)(struct Q(Parameters) **param_ptr,
              struct Q(State) *state,
              double a_5,
              double M_5,
              double m)
{
  int i;
  double *b;
  double *c;
  int status;

  if (state == NULL || state->error_latched)
    return 1;

  if (param_ptr == NULL)
    return q(set_error)(state, 0, "set_Shamir(): NULL pointer");

  b = q(malloc)(state, 2 * state->Ls * sizeof (double));
  if (b == 0)
    return q(set_error)(state, 0, "set_Shamir(): Not enough space");
  c = b + state->Ls;
  for (i = 0; i < state->Ls; i++) {
    b[i] = a_5;
    c[i] = 0;
  }
  status = Q(set_generic)(param_ptr, state, b, c, M_5, m);
  q(free)(state, b, 2 * state->Ls * sizeof (double));
  return status;
}
