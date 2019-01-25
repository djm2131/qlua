#include <mdwf.h>

void
qx(op_pc2_Mxn)(struct Fermion *r_x,
                double *global_norm,
                struct Q(State) *state,
                const struct Q(Parameters) *params,
                const struct SUn *gauge,
                const struct Fermion *s_x,
                long long *flops,
                long long *sent,
                long long *received,
                struct Fermion *t_x,
                struct Fermion *t_y)
{
    *global_norm = 0.0;
    qx(op_Kx)(t_x, state->lat_x, params, s_x,
                flops);
    qx(op_A1xBxFx)(t_y, state->lat_y, params, gauge, t_x, 
                flops, sent, received);
    qx(op_1mK1xA1xBxFx_norm)(
                r_x, global_norm, state->lat_x, params, gauge, s_x, t_y,
                flops, sent, received);
    QMP_sum_double(global_norm);
}

void
qx(op_pc2p_Mxn)(struct Fermion *r_x,
                double *global_norm,
                struct Q(State) *state,
                const struct Q(Parameters) *params,
                const struct SUn *gauge,
                const struct Fermion *s_x,
                long long *flops,
                long long *sent,
                long long *received,
                struct Fermion *t_x,
                struct Fermion *t_y)
{
    *global_norm = 0.0;
    qx(op_A1xBxFx)(t_y, state->lat_y, params, gauge, s_x,
                flops, sent, received);
// TODO replace with the fused function
//    qx(op_1mA1xBxFx_norm)(r_x, global_norm, state->lat_x, params, gauge, s_x, t_y,
//                flops, sent, received);
    qx(op_A1xBxFx)(t_x, state->lat_x, params, gauge, t_y,
                flops, sent, received);
    qx(omp_f_copy)(state, r_x, state->lat_x->full_size, state->Ls, s_x);
    state->flops += qx(omp_f_add2_norm)(state, r_x, global_norm, state->lat_x->full_size, state->Ls, -1., t_x);
// to here
    QMP_sum_double(global_norm);
}
void
qx(op_Mxn)(struct Fermion *r_x,
           double *global_norm,
           struct Q(State) *state,
           const struct Q(Parameters) *params,
           const struct SUn *gauge,
           const struct Fermion *s_x,
           long long *flops,
           long long *sent,
           long long *received,
           struct Fermion *t_x,
           struct Fermion *t_y)
{
    if (QOP_MDWF_EOPC2 == state->eopc_type)
        qx(op_pc2_Mxn)(r_x, global_norm, state, params, gauge, s_x, 
                flops, sent, received, t_x, t_y);
    else if (QOP_MDWF_EOPC2PRIME == state->eopc_type)
        qx(op_pc2p_Mxn)(r_x, global_norm, state, params, gauge, s_x, 
                flops, sent, received, t_x, t_y);
    else 
        q(set_error)(state, 1, "unknown eoprec");
}
