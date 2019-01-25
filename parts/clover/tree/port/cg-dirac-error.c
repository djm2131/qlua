#include <clover.h>

double
qx(cg_dirac_error)(const struct FermionX *psi_e,
                   const struct FermionX *psi_o,
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
    double e_norm, o_norm, norm;
    
    qx(op_ApB)(t0_e, &state->even, gauge->g_data, gauge->ce_data,
               psi_e, psi_o, flops, sent, received);
    qx(op_ApB)(t0_o, &state->odd, gauge->g_data, gauge->co_data,
               psi_o, psi_e, flops, sent, received);
    *flops += qx(f_diff_norm)(&e_norm, state->even.full_size, t0_e, eta_e);
    *flops += qx(f_diff_norm)(&o_norm, state->odd.full_size, t0_o, eta_o);
    
    norm = e_norm + o_norm;
    *flops += 1; /* every flop counts ... */
    QMP_sum_double(&norm);

    return norm;
}

