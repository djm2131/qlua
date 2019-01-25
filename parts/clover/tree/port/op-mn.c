#include <clover.h>

void
qx(op_even_Mn)(struct FermionX *r_e,
               double *global_norm,
               struct Q(State) *state,
               const struct QX(Gauge) *gauge,
               const struct FermionX *s_e,
               long long *flops,
               long long *sent,
               long long *received,
               struct FermionX *t_o)
{
    qx(op_AB)(t_o, &state->odd, gauge->g_data, gauge->cox_data, s_e,
              flops, sent, received);
    qx(op_AmB_norm)(r_e, global_norm,
                    &state->even, gauge->g_data, gauge->ce_data, s_e, t_o,
                    flops, sent, received);
    QMP_sum_double(global_norm);
}
