#include <clover.h>

void
qx(cg_precondition)(struct FermionX *chi_e,
                    struct Q(State) *state,
                    const struct QX(Gauge) *gauge,
                    const struct FermionX *eta_e,
                    const struct FermionX *eta_o,
                    long long *flops,
                    long long *sent,
                    long long *received,
                    struct FermionX *t0_e,
                    struct FermionX *t0_o)
{
    qx(op_A)(t0_o, &state->odd, gauge->cox_data, eta_o, flops);
    qx(op_CmB)(t0_e, &state->even, gauge->g_data, eta_e, t0_o,
               flops, sent, received);
    qx(op_even_Mx)(chi_e, state, gauge, t0_e,
                   flops, sent, received, t0_o);
}
