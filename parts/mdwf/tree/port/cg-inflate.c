#include <mdwf.h>

void
qx(cg_pc2_inflate)(struct Fermion *psi_x,
                   struct Fermion *psi_y,
                   struct Q(State) *state,
                   const struct Q(Parameters) *params,
                   const struct SUn *U,
                   const struct Fermion *eta_y,
                   const struct Fermion *xi_x,
                   long long *flops,
                   long long *sent,
                   long long *received,
                   struct Fermion *t_x,
                   struct Fermion *t_y)
{
    qx(op_K1)(t_x, state->lat_x, params, xi_x, 
                flops);
    qx(op_A1)(psi_x, state->lat_x, params, t_x, 
                flops);
    qx(op_B)(t_x, state->lat_x, params, psi_x, 
                flops);
    qx(op_1mF)(t_y, state->lat_y, U, eta_y, t_x, 
                flops, sent, received);
    qx(op_A1)(psi_y, state->lat_y, params, t_y, 
                flops);
}

void
qx(cg_pc2p_inflate)(struct Fermion *psi_x,
                    struct Fermion *psi_y,
                    struct Q(State) *state,
                    const struct Q(Parameters) *params,
                    const struct SUn *U,
                    const struct Fermion *eta_y,
                    const struct Fermion *xi_x,
                    long long *flops,
                    long long *sent,
                    long long *received,
                    struct Fermion *t_x,
                    struct Fermion *t_y)
{
    qx(op_A1)(psi_x, state->lat_x, params, xi_x, 
                flops);
    qx(op_B)(t_x, state->lat_x, params, psi_x, 
                flops);
    qx(op_1mF)(t_y, state->lat_y, U, eta_y, t_x, 
                flops, sent, received);
    qx(op_A1)(psi_y, state->lat_y, params, t_y, 
                flops);
}

void
qx(cg_inflate)(struct Fermion *psi_x,
                    struct Fermion *psi_y,
                    struct Q(State) *state,
                    const struct Q(Parameters) *params,
                    const struct SUn *U,
                    const struct Fermion *eta_y,
                    const struct Fermion *xi_x,
                    long long *flops,
                    long long *sent,
                    long long *received,
                    struct Fermion *t_x,
                    struct Fermion *t_y)
{
    if (QOP_MDWF_EOPC2 == state->eopc_type)
        qx(cg_pc2_inflate)(psi_x, psi_y, state, params, U, eta_y, xi_x, 
                flops, sent, received, t_x, t_y);
    else if (QOP_MDWF_EOPC2PRIME == state->eopc_type)
        qx(cg_pc2p_inflate)(psi_x, psi_y, state, params, U, eta_y, xi_x, 
                flops, sent, received, t_x, t_y);
    else 
        q(set_error)(state, 1, "unknown eoprec");
}
