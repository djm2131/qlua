#include <clover.h>

void
qx(cg_inflate)(struct FermionX *psi_o,
               struct Q(State) *state,
               const struct QX(Gauge) *gauge,
               const struct FermionX *eta_o,
               const struct FermionX *psi_e,
               long long *flops,
               long long *sent,
               long long *received,
               struct FermionX *t_o)
{
    qx(op_CmB)(t_o, &state->odd, gauge->g_data, eta_o, psi_e,
               flops, sent, received);
    qx(op_A)(psi_o, &state->odd, gauge->cox_data, t_o, flops);
}

