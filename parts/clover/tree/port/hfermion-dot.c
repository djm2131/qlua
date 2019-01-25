#include <clover.h>

int
QX(dot_half_fermion)(double *v_r, double *v_i,
                     const struct QX(HalfFermion) *a,
                     const struct QX(HalfFermion) *b)
{
    double s[2];
  long long flops;
  DECLARE_STATE;

  CHECK_ARG0(a);
  CHECK_ARGn(b, "dot_half_fermion");
  CHECK_POINTER(v_r, "dot_half_fermion");
  CHECK_POINTER(v_i, "dot_half_fermion");

  BEGIN_TIMING(state);
    flops = qx(f_dot)(&s[0], &s[1],
                    state->even.full_size,
                    a->even, b->even);
    QMP_sum_double_array(s, 2);
    *v_r = s[0];
    *v_i = s[1];
  END_TIMING(state, flops, 2 * sizeof (double), 2 * sizeof (double));

  return 0;
}
