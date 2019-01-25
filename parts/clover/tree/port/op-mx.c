#include <clover.h>

void
qx(op_even_Mx)(struct FermionX *r_e,
               struct Q(State) *state,
               const struct QX(Gauge) *gauge,
               const struct FermionX *s_e,
               long long *flops,
               long long *sent,
               long long *received,
               struct FermionX *t_o)
{
    qx(op_AxBx)(t_o, &state->odd, gauge->g_data, gauge->cox_data, s_e,
                flops, sent, received);
    qx(op_AxmBx)(r_e, &state->even, gauge->g_data, gauge->ce_data, s_e, t_o,
                 flops, sent, received);
}
