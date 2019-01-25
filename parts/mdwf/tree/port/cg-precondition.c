#include <mdwf.h>

void
qx(cg_pc2_precondition)(struct Fermion *xi0_x,
                        struct Fermion *chi_x,
                        struct Q(State) *state,
                        const struct Q(Parameters) *params,
                        const struct SUn *U,
                        const struct Fermion *psi0_x,
                        const struct Fermion *eta_x,
                        const struct Fermion *eta_y,
                        long long *flops,
                        long long *sent,
                        long long *received,
                        struct Fermion *t0_x,
                        struct Fermion *t1_x,
                        struct Fermion *t0_y)
{
    qx(op_BA1)(t0_y, state->lat_y, params, eta_y, 
                flops);
    qx(op_1mF)(t0_x, state->lat_x, U, eta_x, t0_y, 
                flops, sent, received);
    qx(op_K)(t1_x, state->lat_x, params, t0_x, 
                flops);
    qx(op_pc2_Mx)(chi_x, state, params, U, t1_x, 
                flops, sent, received, t0_x, t0_y);
    qx(op_A)(t0_x, state->lat_x, params, psi0_x, 
                flops);
    qx(op_K)(xi0_x, state->lat_x, params, t0_x, 
                flops);
}
void
qx(cg_pc2p_precondition)(struct Fermion *xi0_x,
                         struct Fermion *chi_x,
                         struct Q(State) *state,
                         const struct Q(Parameters) *params,
                         const struct SUn *U,
                         const struct Fermion *psi0_x,
                         const struct Fermion *eta_x,
                         const struct Fermion *eta_y,
                         long long *flops,
                         long long *sent,
                         long long *received,
                         struct Fermion *t0_x,
                         struct Fermion *t1_x,
                         struct Fermion *t0_y)
{
    qx(op_BA1)(t0_y, state->lat_y, params, eta_y, 
                flops);
    qx(op_1mF)(t1_x, state->lat_x, U, eta_x, t0_y, 
                flops, sent, received);
    qx(op_pc2p_Mx)(chi_x, state, params, U, t1_x, 
                flops, sent, received, t0_x, t0_y);
    qx(op_A)(xi0_x, state->lat_x, params, psi0_x, 
                flops);
}

void
qx(cg_precondition)(struct Fermion *xi0_x,
                         struct Fermion *chi_x,
                         struct Q(State) *state,
                         const struct Q(Parameters) *params,
                         const struct SUn *U,
                         const struct Fermion *psi0_x,
                         const struct Fermion *eta_x,
                         const struct Fermion *eta_y,
                         long long *flops,
                         long long *sent,
                         long long *received,
                         struct Fermion *t0_x,
                         struct Fermion *t1_x,
                         struct Fermion *t0_y)
{
    if (QOP_MDWF_EOPC2 == state->eopc_type)
        qx(cg_pc2_precondition)(xi0_x, chi_x, state, params, U, psi0_x, eta_x, eta_y,
                flops, sent, received, t0_x, t1_x, t0_y);
    else if (QOP_MDWF_EOPC2PRIME == state->eopc_type)
        qx(cg_pc2p_precondition)(xi0_x, chi_x, state, params, U, psi0_x, eta_x, eta_y,
                flops, sent, received, t0_x, t1_x, t0_y);
    else 
        q(set_error)(state, 1, "unknown eoprec");
}
