#include "twisted.h"

size_t
qx(op_norm2)(double *norm,
            const struct QX(Fermion) *a,
            struct Q(State) *state)
{
    double n_e;
    double n_o;
    size_t flops = 0;

    flops += qx(f_norm)(&n_e, state->even.full_size, a->even);
    flops += qx(f_norm)(&n_o, state->odd.full_size, a->odd);
    *norm = n_e + n_o;
    QMP_sum_double(norm);
    return flops + 1;
}

int
QX(norm2_fermion)(double *v_r,
                  const struct QX(Fermion) *a)
{
  size_t flops = 0;
  DECLARE_STATE;

  CHECK_ARG0(a);
  CHECK_POINTER(v_r, "norm2_fermion");

  BEGIN_TIMING(state);
  flops += qx(op_norm2)(v_r, a, state);
  END_TIMING(state, flops, sizeof (double), sizeof (double));

  return 0;  
}
